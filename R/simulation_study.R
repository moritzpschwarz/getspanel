# This scripts contains various functions to set up, run, and evaluate simulation experiments for assessing the performance of getspanel
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(gets)
library(data.table)
library(future)
library(furrr)
library(progressr)
library(patchwork)

# devtools::load_all()
base_time <- 1900

# Creates single treatment effect based on type, location, and magnitude
# Returns a list with treatment vector and coefficient used
# For "step", magnitude is applied as a constant shift
# For "trend" and "trendbreaks", magnitude is scaled to the length of the trend
impose_treatment <- function(type, n_time, location, magnitude) {
  if (!(type %in% c("trend", "trendbreak", "step"))) {
    stop("treatment type not recognized")
  }

  # Initialize treatment vector and determine absolute location
  treat_dummies <- rep(0, n_time)
  if (type == "trend") {
    # "Trend" always starts at the beginning
    treat_dummies <- 1:n_time
    coef <- magnitude / n_time
  } else if (type == "trendbreak") {
    # "Trendbreak" starts at abs_location and sets increasing dummies
    treat_dummies[location:n_time] <- seq_along(treat_dummies[location:n_time])
    coef <- magnitude / (n_time - location + 1)
  } else if (type == "step") {
    # "Step" sets a constant step-shift from abs_location
    treat_dummies[location:n_time] <- 1
    coef <- magnitude
  }
  treat_eff <- treat_dummies * coef

  list(treat_eff = treat_eff, coef = coef)
}

# Creates random panel data with imposed treatments
# n_id, n_time: panel dimensions
# fe_sigma: standard deviation of unit fixed effects (giving each unit a different mean level)
# beta: vector of coefficients for random input data (x variables)
# sigma: standard deviation of idiosyncratic error term
# treatment_params: tibble with columns id, type, magnitude, location, each row specifying a treatment to impose
# rel_treat_params: if TRUE, treatment parameters are interpreted relative to panel setting, i.e., id and location (0-1) are scaled to n_id and n_time, and magnitude is scaled by unit fixed effect. Otherwise, absolute values are used to impose treatments as specified. Default is TRUE for flexibility in simulation design.
# plot: if TRUE, a plot is generated showing the data, unit fixed effects, treatment effects, and outcome variable for each unit
# Returns a list with input_data (data.frame with columns id, time, x variables, y) and treatment_collection (tibble with imposed treatments: id, type, time, coef)
create_input_data <- function(n_id, n_time, treatment_params, fe_sigma, beta, sigma, rel_treat_params = TRUE, plot = FALSE, ...) {
  # Initialize unit fixed effects with fe_sigma and return vectors
  means <- rnorm(n_id, sd = fe_sigma)
  input_data <- data.frame()
  treatment_collection <- tibble(
    id = character(), type = character(), time = double(), coef = double()
  )

  # Adjust treatment parameters if relative specification is used
  if (rel_treat_params == TRUE) {
    treatment_params <- treatment_params %>%
      mutate(id = pmax(ceiling(n_id * id), 1)) %>%
      mutate(location = pmax(ceiling(n_time * location), 1)) %>%
      group_by(id) %>%
      mutate(magnitude = magnitude * means[id]) %>%
      ungroup()
  }

  # Generate data and impose treatments for each unit
  for (id in 1:n_id) {
    # Create random input data (x)
    x <- matrix(rnorm(n_time * length(beta)), ncol = length(beta))
    # Compute outcome variable (y) with unit fixed effect and error term
    fe <- means[id]
    eps <- rnorm(n_time, mean = 0, sd = sigma)
    y <- x %*% beta + fe + eps

    data <- data.frame(
      id = LETTERS[id],
      time = (1:n_time) + base_time,
      x = x,
      y = y
    )

    # Impose all treatments specified for this unit to outcome variable and collect absolute values for each treatment
    # Treatment effects are summed up in a separate data column for plotting
    data$treat_eff <- 0
    if (id %in% treatment_params$id) {
      params <- treatment_params %>% filter(id == !!id)

      # Get effect and coef for each treatment
      for (i in seq_len(nrow(params))) {
        treatment <- impose_treatment(
          type = params$type[i],
          n_time = n_time,
          location = params$location[i],
          magnitude = params$magnitude[i]
        )

        # Add treatment effect to outcome variable ond plotting column
        data$y <- data$y + treatment$treat_eff
        data$treat_eff <- data$treat_eff + treatment$treat_eff

        # Store treatment information with absolute values (i.e. id and time scaled to panel dimensions and coefficient scaled by unit fixed effect and panel length for trends)
        # This is later used to match detected treatments to true treatments
        treat_entry <- tibble(
          id = LETTERS[id],
          type = params$type[i],
          time = params$location[i],
          coef = treatment$coef
        )
        treatment_collection <- bind_rows(treatment_collection, treat_entry)
      }
    } else {
      # NA column to avoid zero-lines for units without treatment in plotting
      data$treat_eff <- NA
    }

    # Append rows for all units into one data frame
    input_data <- bind_rows(input_data, data)
  }

  if (plot == TRUE) {
    # Prepare data for plotting
    tmp <- input_data %>%
      mutate(unit_fe = rep(means, each = n_time)) %>%
      pivot_longer(-c(id, time)) %>%
      mutate(plot_group = case_when(
        name %in% c("x.1", "x.2", "x.3", "x.4", "x.5") ~ "data",
        TRUE ~ name
      ))

    p <- ggplot() +
      # Gray lines for "data" group, with plot_group
      # group/color arguments make sure each x variable is a separate line but they share a legend entry
      geom_line(
        data = tmp %>% filter(plot_group == "data"),
        aes(x = time, y = value, group = name, color = plot_group),
        size = 0.7, alpha = 0.7
      ) +
      # Dashed lines for unit_fe and treatment
      # na.rm = TRUE for units without treatment
      geom_line(
        data = tmp %>% filter(plot_group %in% c("unit_fe", "treat_eff")),
        aes(x = time, y = value, color = plot_group),
        size = 0.8, na.rm = TRUE, linetype = "dashed"
      ) +
      # Solid line for y
      geom_line(
        data = tmp %>% filter(plot_group == "y"),
        aes(x = time, y = value, color = plot_group),
        size = 1
      ) +
      facet_wrap(~id) +
      scale_color_manual(
        values = c(
          "data" = "gray60",
          "unit_fe" = "#E41A1C",
          "treat_eff" = "#377EB8",
          "y" = "#4DAF4A"
        )
      ) +
      labs(color = "Variable")
    plot(p)
  }

  # Remove plotting column and return final data and treatment collection
  list(
    input_data = input_data %>% select(-treat_eff),
    treatment_collection = treatment_collection
  )
}

# Runs a single simulation model with specified parameters
# Returns a tibble with all relevant information for the simulation run
# n_id, n_time, treatment_params, fe_sigma, beta, sigma: see create_input_data()
# method ("fesis", "tis", "both"), t.pval: see getspanel::isatpanel()
# max.block.size: see gets::isat()
# ...: additional parameters passed to create_input_data() and isatpanel() (i.e. rel_treat_params, print.searchinfo, plot, etc.)
# Any additional factors to be studied can be added to the function signature and return tibble as needed
run_single_model <- function(sim_id, n_id, n_time, method, treatment_params, fe_sigma, beta, sigma, t.pval, max.block.size, ...) {
  # Create input data with imposed treatments and treatment information
  data_creation <- create_input_data(
    n_id = n_id,
    n_time = n_time,
    treatment_params = treatment_params,
    fe_sigma = fe_sigma,
    beta = beta,
    sigma = sigma,
    ...
  )
  input_data <- data_creation$input_data
  treatment_collection <- data_creation$treatment_collection

  # Prepare formula and run getspanel
  variables <- paste0(input_data %>% select(-c(id, time, y)) %>% names,
                      collapse = " + ")
  form <- as.formula(paste0("y ~ ", variables))
  result <- isatpanel(
    data = input_data,
    formula = form,
    index = c("id", "time"),
    effect = "individual",
    fesis = ifelse(method %in% c("fesis", "both"), TRUE, FALSE),
    tis = ifelse(method %in% c("tis", "both"), TRUE, FALSE),
    iis = FALSE,
    t.pval = t.pval,
    max.block.size = max.block.size,
    ...
  )

  # Return tibble with all relevant information for the simulation run
  tibble(
    sim_id = sim_id,
    n_id = n_id,
    n_time = n_time,
    treatment_params = list(treatment_params),
    indic_method = method,
    t.pval = t.pval,
    max.block.size = max.block.size,
    treatment_collection = list(treatment_collection),
    indicators = list(get_indicators(result)),
    getspanel_object = list(result)
  )
}

# Runs a full simulation study over combinations of specified parameters
# n_rep: number of repetitions per parameter combination
# All other parameters are lists of values to be combined in the study and are explained in run_single_model()
# Returns a tibble with results for all simulation runs
run_simulation_study <- function(n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep = 1, methods = c("both"), t.pvals = c(0.05, 0.01, 0.001), max.block.sizes = c(30), n_cores = NULL, ...) {
  # Create parameter combinations
  param_grid <- expand_grid(
    method = methods,
    t.pval = t.pvals,
    max.block.size = max.block.sizes,
    treatment_params = treatment_params_list,
    n_time = n_times,
    n_id = n_ids,
    rep = 1:n_rep
  ) %>%
    mutate(sim_id = row_number())

  print(paste("Total simulations to run:", nrow(param_grid)))
  # p <- progressr::progressor(along = param_grid)

  # Set up parallel processing
  if (is.null(n_cores)) {
    n_cores <- min(nrow(param_grid), parallel::detectCores() - 1)
  }
  plan(multisession, workers = n_cores)

  # Run simulations in parallel
  overall <- future_pmap(
    param_grid,
    function(method, t.pval, max.block.size, treatment_params, n_time, n_id, rep, sim_id) {

      # print(paste("Running simulation", sim_id))
      suppressMessages(devtools::load_all())

      result <- run_single_model(
        sim_id = sim_id,
        n_id = n_id,
        n_time = n_time,
        method = method,
        treatment_params = treatment_params,
        fe_sigma = fe_sigma,
        beta = beta,
        sigma = sigma,
        t.pval = t.pval,
        max.block.size = max.block.size,
        ...
      )
    },
    .options = furrr_options(seed = TRUE),
    .progress = TRUE
  )
  # Clean up
  plan(sequential)
  overall <- bind_rows(overall)

  return(overall)
}

# Extract true and detected treatments from the overall tibble
extract_treatments <- function(overall_tibble) {
  true_treatments <- overall_tibble %>%
    select(sim_id, treatment_collection) %>%
    unnest(treatment_collection) %>%
    mutate(type = ifelse(type == "trendbreak", "trend", type)) %>%
    select(sim_id, id, type, time, coef)

  detected_treatments <- overall_tibble %>%
    select(sim_id, indicators) %>%
    unnest(indicators) %>%
    unnest(indicators) %>%
    select(sim_id, id, time, name) %>%
    mutate(
      time = time - base_time,
      type = case_when(
        grepl("^fesis", name) ~ "step",
        grepl("^tis", name) ~ "trend"
      )
    ) %>%
    select(sim_id, id, type, time)

  if (nrow(true_treatments) == 0) {
    true_treatments <- tibble(sim_id = integer(), id = character(), type = character(), time = double(), coef = double())
  }
  if (nrow(detected_treatments) == 0) {
    detected_treatments <- tibble(sim_id = integer(), id = character(), type = character(), time = double())
  }

  list(
    true_treatments = true_treatments,
    detected_treatments = detected_treatments
  )
}

# Optimal bipartite matching for true and detected treatments
# true_treatments, detected_treatments: tibbles with columns id, type, time
# tolerance: maximum allowed timing difference for a match
# allow_type_mismatch: if TRUE, type mismatches are allowed (with penalty); if FALSE, type mismatches are not allowed
# Returns a tibble with matched treatments (match = TRUE) and unmatched treatments from both true and detected sets (match = FALSE)
# The matching algorithm prioritizes maximizing the number of matches, and among those, minimizing the cost. Cost is defined as the timing difference, with additional penalty for type mismatches if allowed.
# This can only be used for matching within a single simulation run (i.e., for one sim_id)
optimal_match_treatments <- function(true_treatments = NULL, detected_treatments = NULL, tolerance = 0, allow_type_mismatch = FALSE) {
  # Use explicit column names for joining and index treatments
  true_treatments <- true_treatments %>%
    rename(true_time = time, true_type = type) %>%
    mutate(true_idx = row_number())
  detected_treatments <- detected_treatments %>%
    rename(detected_time = time, detected_type = type) %>%
    mutate(detected_idx = row_number())

  # Process each id separately
  all_matches <- tibble(
    id = character(), true_idx = integer(), detected_idx = integer(), true_time = integer(), detected_time = integer(), true_type = character(), detected_type = character(), timing_diff = integer(), type_mismatch = logical(), cost = integer(), match = logical()
  )
  for (id in unique(c(true_treatments$id, detected_treatments$id))) {
    true_subset <- true_treatments %>%
      dplyr::filter(id == !!id)
    detected_subset <- detected_treatments %>%
      dplyr::filter(id == !!id)
    
    if (nrow(true_subset) == 0 || nrow(detected_subset) == 0) next

    # Create cost matrix of all (true, detected) pairs for this id
    cost_matrix <- expand_grid(
      true_idx = true_subset$true_idx,
      detected_idx = detected_subset$detected_idx
    ) %>%
      mutate(id = id) %>%
      left_join(true_subset, by = c("id", "true_idx")) %>%
      left_join(detected_subset, by = c("id", "detected_idx")) %>%
      mutate(
        timing_diff = abs(true_time - detected_time),
        type_mismatch = true_type != detected_type,
        # Create cost: high cost for invalid matches
        cost = case_when(
          timing_diff > tolerance ~ Inf,
          !allow_type_mismatch & type_mismatch ~ Inf,
          TRUE ~ timing_diff + (as.numeric(type_mismatch) * (tolerance + 1))
        )
      ) %>%
      select(id, true_idx, detected_idx, true_time, detected_time, true_type, detected_type, timing_diff, type_mismatch, cost)

    # Find optimal one-to-one assignment for this id and append to all matches
    pair_matches <- find_optimal_assignment(cost_matrix)
    all_matches <- bind_rows(all_matches, pair_matches)
  }

  # Add unmatched treatments with match = FALSE for completeness
  if (nrow(all_matches) == 0) {
    matched_true <- c()
    matched_detected <- c()
  } else {
    matched_true <- all_matches %>% filter(match) %>% pull(true_idx)
    matched_detected <- all_matches %>% filter(match) %>% pull(detected_idx)
  }
  unmatched_true <- true_treatments %>%
    filter(!true_idx %in% matched_true) %>%
    mutate(match = FALSE) %>%
    select(id, true_type, true_time)
  unmatched_detected <- detected_treatments %>%
    filter(!detected_idx %in% matched_detected) %>%
    mutate(match = FALSE) %>%
    select(id, detected_type, detected_time)

  all_results <- all_matches %>%
    select(-true_idx, -detected_idx) %>%
    bind_rows(unmatched_true) %>%
    bind_rows(unmatched_detected)

  all_results
}

# Find Optimal Assignment using brute force enumeration
# Prioritizes: 1) Maximum number of matches, 2) Minimum total cost
find_optimal_assignment <- function(cost_matrix) {
  # Get valid matches only
  valid_matches <- cost_matrix %>% 
    filter(is.finite(cost))

  if (nrow(valid_matches) == 0) {
    return(tibble())
  }

  # For small problems, enumerate all possible subsets of matches
  n_matches <- nrow(valid_matches)
  best_num_matches <- 0
  best_cost <- Inf
  best_selection <- c()

  # Try all possible combinations of matches (2^n possibilities)
  for (i in 0:(2^n_matches - 1)) {
    # Convert number to binary to select matches
    selection <- as.logical(intToBits(i)[1:n_matches])
    selected_matches <- valid_matches[selection, ]

    if (nrow(selected_matches) == 0) next

    # Check if this is a valid assignment (no duplicate true or detected treatments)
    if (any(duplicated(selected_matches$true_idx)) || 
        any(duplicated(selected_matches$detected_idx))) {
      next
    }

    # Calculate metrics
    num_matches <- nrow(selected_matches)
    total_cost <- sum(selected_matches$cost)

    # Update best solution if this is better
    # Priority: 1) More matches, 2) Lower cost if same number of matches
    if (num_matches > best_num_matches || 
        (num_matches == best_num_matches && total_cost < best_cost)) {
      best_num_matches <- num_matches
      best_cost <- total_cost
      best_selection <- selection
    }
  }

  if (length(best_selection) == 0 || !any(best_selection)) {
    return(tibble())
  }

  # Return best matches
  result_matches <- valid_matches[best_selection, ] %>%
    mutate(match = TRUE)

  return(result_matches)
}

candidate_count <- function(n_id, n_time, method) {
  per_family <- n_id * (n_time - 1)
  if (method == "both") {
    return(2 * per_family)
  } else if (method %in% c("fesis","tis")) {
    return(per_family)
  } else {
    return(0L)
  }
}

# Compute metrics for each simulation in overall_tibble in parallel
# overall_tibble: tibble with all simulation runs
# tolerance: maximum allowed timing difference for a match
# allow_type_mismatch: if TRUE, type mismatches are allowed (with penalty); if FALSE, type mismatches are not allowed (and are not matched)
# Returns a tibble with factors and metrics per simulation run
# Metrics computed: n_detected, gauge, potency, precision, recall, f1
compute_metrics <- function(overall_tibble, tolerance = 0, allow_type_mismatch = FALSE) {
  # Extract factors/parameters and treatments separately
  meta <- overall_tibble %>%
    dplyr::select(-treatment_collection, -indicators, -getspanel_object)
  tx <- extract_treatments(overall_tibble)
  true_all <- tx$true_treatments
  det_all <- tx$detected_treatments

  purrr::pmap_dfr(meta, function(sim_id, n_id, n_time, treatment_params, indic_method, t.pval, max.block.size) {
    true_sim <- true_all %>%
      dplyr::filter(sim_id == !!sim_id)
    det_sim  <- det_all %>%
      dplyr::filter(sim_id == !!sim_id)

    matches <- optimal_match_treatments(
      true_treatments = true_sim,
      detected_treatments = det_sim,
      tolerance = tolerance,
      allow_type_mismatch = allow_type_mismatch
    )

    tp <- nrow(matches %>% dplyr::filter(match))
    det <- nrow(det_sim)
    fp <- max(det - tp, 0)

    total_candidates <- candidate_count(n_id, n_time, indic_method)
    rel <- nrow(true_sim)
    irrel <- max(total_candidates - rel, 0)

    prec <- ifelse(det > 0, tp / det, NA_real_)
    rec <- ifelse(rel > 0, tp / rel, NA_real_)
    f1 <- if (!is.na(prec) & !is.na(rec) & (prec + rec) > 0) {
      2 * (prec * rec) / (prec + rec)
    } else {
      NA_real_
    }

    metrics <- tibble::tibble(
      # Simulation parameters/factors
      sim_id = sim_id,
      n_id = n_id,
      n_time = n_time,
      indic_method = indic_method,
      t.pval = t.pval,
      max.block.size = max.block.size,
      n_true = rel,
      n_true_per_method = ifelse(indic_method == "both", rel / 2, rel),
      magnitude = mean(treatment_params$magnitude),
      tolerance = tolerance,
      # Metrics
      n_detected = det,
      gauge = ifelse(irrel > 0, fp / irrel, NA_real_),
      potency = ifelse(rel  > 0, tp / rel, NA_real_),
      precision = prec,
      recall = rec,
      f1 = f1,
      matches = list(matches)
    )

    return(metrics)
  })
}

# Summarizes metrics over all simulations in overall_tibble
# overall_tibble: tibble with all simulation runs
# tolerances: vector of tolerances to compute metrics for
# allow_type_mismatch: if TRUE, type mismatches are allowed (with penalty); if FALSE, type mismatches are not allowed
# factors: vector of column names to group by for summary statistics
# Returns a list with two tibbles:
# - per_simulation: metrics computed per simulation run (sim_id) for each tolerance (using compute_metrics())
# - by_factor: average metrics grouped by specified factors
metrics_summary <- function(overall_tibble, tolerances = c(0), allow_type_mismatch = FALSE, factors = c("indic_method", "t.pval", "tolerance")) {
  gp <- dplyr::bind_rows(
    lapply(tolerances, function(t) {
      compute_metrics(overall_tibble, t, allow_type_mismatch)
    })
  )

  # Group by the columns specified in 'factors' and then summarise metrics.
  by_factor <- gp %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(factors))) %>%
    dplyr::summarise(
      avg_gauge = mean(gauge, na.rm = TRUE),
      avg_potency = mean(potency, na.rm = TRUE),
      avg_precision = mean(precision, na.rm = TRUE),
      avg_recall = mean(recall, na.rm = TRUE),
      avg_f1 = mean(f1, na.rm = TRUE),
      avg_detected = mean(n_detected, na.rm = TRUE),
      .groups = "drop"
    )

  list(per_simulation = gp, by_factor = by_factor)
}

plot_metrics <- function(analysis_per_simulation, plot_type = "boxplot", metrics = c("gauge", "potency", "f1"), factors = NULL, title = "Metrics per Simulation by Factor", separate_metrics = TRUE, ncol = NULL) {
  # Define custom labels for methods
  method_labels <- c(
    "fesis" = "Step",
    "tis" = "Trend", 
    "both" = "Both"
  )
  
  # Identify varying factors (exclude indicators, treatment_collection, getspanel_object, sim_id, num_breaks)
  if (is.null(factors)) {
    meta <- c("sim_id", "gauge", "potency", "precision", "recall", "f1", "n_detected", "matches")
    potential_factors <- setdiff(names(analysis_per_simulation), c(meta))
    potential_factors <- sapply(
      analysis_per_simulation %>% select(all_of(potential_factors)),
      function(x) length(unique(x)) > 1
    )
    varying_factors <- names(potential_factors[potential_factors])
    print(paste("Varying factors identified for plotting:", paste(varying_factors, collapse = ", ")))
  } else {
    varying_factors <- factors
  }

  analysis_long <- analysis_per_simulation %>%
    mutate(across(all_of(varying_factors), as.character)) %>%
    # Apply custom labeling for indic_method if it's one of the varying factors
    mutate(indic_method = ifelse("indic_method" %in% varying_factors, 
                                 recode(indic_method, !!!method_labels), 
                                 indic_method)) %>%
    pivot_longer(
      cols = all_of(metrics),
      names_to = "metric",
      values_to = "metric_value"
    ) %>%
    pivot_longer(
      cols = all_of(varying_factors),
      names_to = "factor",
      values_to = "factor_value"
    )

  # Create a lookup table for factor levels
  factor_level_lookup <- split(analysis_long, analysis_long$factor) %>%
    map(function(factor_data) {
      factor_values <- factor_data$factor_value
      unique_values <- unique(factor_values)

      # Check if this factor is numeric
      is_numeric <- all(suppressWarnings(!is.na(as.numeric(unique_values))))

      # Create appropriate factor levels
      if(is_numeric) {
        ordered_levels <- as.character(sort(as.numeric(unique_values)))
      } else {
        ordered_levels <- sort(unique_values)
      }

      return(ordered_levels)
    })

  # Create separate plots for each factor to ensure proper ordering
  factor_plots <- list()
  for (factor_name in names(factor_level_lookup)) {
    factor_data <- analysis_long %>%
      filter(factor == factor_name) %>%
      mutate(factor_value_ordered = factor(factor_value, levels = factor_level_lookup[[factor_name]]))

    p <- ggplot(factor_data, aes(x = factor_value_ordered, y = metric_value, color = metric, fill = metric))

    if (plot_type == "scatter") {
      p <- p + geom_jitter(position = position_dodge(width = 0.75))
    } else if (plot_type == "boxplot") {
      p <- p + geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.5, outlier.alpha = 1.0)
    }

    p <- p +
      labs(
        x = paste(factor_name),
        y = "Metric Value",
        color = ifelse(factor_name == "indic_method", "Method", "Metric"),
        fill = ifelse(factor_name == "indic_method", "Method", "Metric")
      ) +
      theme(legend.position = "none")

    # Add metric faceting if requested (for separate_metrics = TRUE)
    if (separate_metrics && length(unique(factor_data$metric)) > 1) {
      p <- p +
        facet_wrap(~metric, ncol = 1, scales = "free") +
        theme(strip.text = element_blank())
    }

    factor_plots[[factor_name]] <- p
  }

  # Combine all plots
  p <- wrap_plots(
    factor_plots,
    ncol = ifelse(is.null(ncol), length(factor_plots), ncol),
    axis_titles = "collect_y"
  ) +
    plot_annotation(title = title) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  p
}

plot_compare_experiments <- function(experiments, metric, factors, labels, tolerance = 0, title, scales = "free", tolerance_ribbon = NULL) {
  # Define custom labels for methods
  method_labels <- c(
    "fesis" = "Step",
    "tis" = "Trend", 
    "both" = "Both"
  )
  
  # Combine by_factor metrics from all experiments
  combined <- dplyr::bind_rows(
    lapply(names(experiments), function(name) {
      experiments[[name]]$by_factor %>%
        dplyr::mutate(scenario = name)
    })
  )

  combined <- combined %>%  
    # Apply custom labeling for indic_method
    mutate(indic_method = recode(indic_method, !!!method_labels)) %>%
    pivot_longer(
      cols = starts_with("avg_"),
      names_to = "metric",
      values_to = "metric_value"
    ) %>%
    pivot_longer(
      cols = all_of(factors),
      names_to = "factor",
      values_to = "factor_value"
    ) %>%
    filter(metric == paste0("avg_", !!metric))

  # If tolerance_ribbon is provided, use it for ribbon bounds, otherwise filter to single tolerance
  if (!is.null(tolerance_ribbon)) {
    combined_for_ribbon <- combined %>%
      filter(tolerance %in% tolerance_ribbon) %>%
      group_by(indic_method, t.pval, factor, factor_value) %>%
      summarise(
        ymin = ifelse(all(is.na(metric_value)), NA_real_, min(metric_value, na.rm = TRUE)),
        ymax = ifelse(all(is.na(metric_value)), NA_real_, max(metric_value, na.rm = TRUE)),
        .groups = "drop"
      )
  }
  
  # Always filter main combined data to the specified tolerance
  combined <- combined %>%
    filter(tolerance == !!tolerance)

  factor_plots <- list()
  for (cur_fac in factors) {
    cur_combined <- combined %>%
      filter(factor == cur_fac) %>%
      mutate(factor_value = factor(factor_value, levels = sort(unique(factor_value)))) %>%
      filter(factor_value != "NA")

    # Prepare ribbon data if tolerance_ribbon is provided
    ribbon_data <- NULL
    if (!is.null(tolerance_ribbon)) {
      ribbon_data <- combined_for_ribbon %>%
        filter(factor == cur_fac) %>%
        mutate(factor_value = factor(factor_value, levels = sort(unique(factor_value)))) %>%
        filter(factor_value != "NA")
    }

    p <- ggplot(cur_combined, aes(x = factor_value, y = metric_value, color = indic_method, group = indic_method))
    
    # Add ribbon if data is available
    if (!is.null(ribbon_data)) {
      p <- p + geom_ribbon(
        data = ribbon_data,
        aes(x = factor_value, ymin = ymin, ymax = ymax, fill = indic_method, group = indic_method),
        alpha = 0.2,
        color = NA,
        inherit.aes = FALSE
      )
    }
    
    p <- p +
      geom_point(na.rm = TRUE) +
      geom_line(na.rm = TRUE) +
      facet_wrap(
        c("t.pval"),
        scales = scales,
        labeller = as_labeller(
          c(
            `0.05` = "t.pval = 0.05",
            `0.01` = "t.pval = 0.01",
            `0.001` = "t.pval = 0.001"
          ),
          multi_line = FALSE
        ),
        strip.position = "top",
      ) +
      theme(
        strip.background = element_blank(),
        legend.position = "none",
      ) +
      labs(
        y = paste("Average", metric),
        x = paste(labels[[cur_fac]]),
        color = "Method",
        fill = "Method"
      )
    factor_plots[[cur_fac]] <- p
  }

  # Create subtitle based on whether ribbon is shown
  subtitle_text <- if (!is.null(tolerance_ribbon)) {
    paste0("Ribbons show timing tolerances ", 
           paste(c(min(tolerance_ribbon), max(tolerance_ribbon)), collapse = "-"),
           "; points show tolerance = ", tolerance)
  } else {
    paste0("Timing tolerance = ", tolerance)
  }
  
  wrap_plots(
    factor_plots,
    ncol = 1,
    axis_titles = "collect",
    axes = "collect_y"
  ) +
    plot_annotation(
      title = title,
      subtitle = subtitle_text
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "right")
}