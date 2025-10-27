library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(gets)
library(data.table)
library(future)
library(furrr)
library(progressr)

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
      mutate(id = max(ceiling(n_id * id), 1)) %>%
      mutate(location = max(ceiling(n_time * location), 1)) %>%
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
run_simulation_study <- function(n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep = 1, methods = c("both"), t.pvals = c(0.05, 0.01, 0.001), max.block.sizes = c(30), ...) {
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
  n_cores <- min(nrow(param_grid), parallel::detectCores() - 1)
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
  true_treatments <- tibble(sim_id = integer(), id = character(), type = character(), time = double(), coef = double())
  detected_treatments <- tibble(sim_id = integer(), id = character(), type = character(), time = double())

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

    metrics <- tibble::tibble(
      # Simulation parameters/factors
      sim_id = sim_id,
      n_id = n_id,
      n_time = n_time,
      indic_method = indic_method,
      t.pval = t.pval,
      max.block.size = max.block.size,
      n_true = rel,
      magnitude = mean(treatment_params$magnitude),
      tolerance = tolerance,
      # Metrics
      n_detected = det,
      gauge = ifelse(irrel > 0, fp / irrel, NA_real_),
      potency = ifelse(rel  > 0, tp / rel, NA_real_ ),
      precision = prec,
      recall = rec,
      f1 = ifelse(prec + rec > 0, 2 * (prec * rec) / (prec + rec), NA_real_),
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

plot_metrics <- function(analysis_per_simulation, plot_type = "scatter", metrics = c("gauge", "potency", "f1"), factors = NULL, title = "Metrics per Simulation (by Factor)", separate_metrics = FALSE) {
  # Identify varying factors (exclude indicators, treatment_collection, getspanel_object, simulation_id, num_breaks)
  if (is.null(factors)) {
    varying_factors <- setdiff(
      names(analysis_per_simulation),
      c("simulation_id", "gauge", "potency", "precision", "recall", "f1", "detected", "true", "matches")
    )
  } else {
    varying_factors <- factors
  }

  print(paste("Varying factors identified for plotting:", paste(varying_factors, collapse = ", ")))
  
  # Store original values for proper ordering before converting to character
  factor_types <- sapply(analysis_per_simulation[varying_factors], function(x) {
    all(suppressWarnings(!is.na(as.numeric(as.character(x)))))
  })
  
  # Calculate scaling factor for gauge if it's included in metrics
  gauge_scale_factor <- 1
  # if ("gauge" %in% metrics && length(metrics) > 1) {
  #   gauge_values <- analysis_per_simulation$gauge[!is.na(analysis_per_simulation$gauge)]
    
  #   if (length(gauge_values) > 0) {
  #     # Calculate scaling factor to bring max gauge value to 1 (or close to it)
  #     max_gauge <- max(gauge_values, na.rm = TRUE)
  #     if (max_gauge > 1) {
  #       gauge_scale_factor <- 1 / max_gauge
  #     } else if (max_gauge > 0) {
  #       # If max is already <= 1, scale to use more of the [0,1] range
  #       # Scale so that the 95th percentile reaches around 0.8-0.9
  #       percentile_95 <- quantile(gauge_values, 0.95, na.rm = TRUE)
  #       if (percentile_95 > 0) {
  #         gauge_scale_factor <- 0.85 / percentile_95
  #       }
  #     }
  #     # Ensure scaling factor is reasonable (don't scale down if already in good range)
  #     gauge_scale_factor <- max(gauge_scale_factor, 1)
  #   }
  # }
  
  analysis_long <- analysis_per_simulation %>%
    # Convert varying factors to character so they can be pivoted together
    mutate(across(all_of(varying_factors), as.character)) %>%
    pivot_longer(
      cols = all_of(metrics),
      names_to = "metric",
      values_to = "value"
    ) %>%
    pivot_longer(
      cols = all_of(varying_factors),
      names_to = "factor",
      values_to = "factor_value"
    ) %>%
    # Scale gauge values to [0,1] range for better visualization
    mutate(
      value = ifelse(metric == "gauge", pmin(value * gauge_scale_factor, 1), value)
    ) %>%
    # Create proper ordering based on pre-computed factor types
    group_by(factor) %>%
    mutate(
      current_factor = cur_group()$factor,
      factor_value_ordered = if_else(
        factor_types[current_factor], # Use pre-computed numeric status
        factor(factor_value, levels = as.character(sort(as.numeric(unique(factor_value))))),
        factor(factor_value, levels = sort(unique(factor_value)))
      )
    ) %>%
    select(-current_factor) %>%
    ungroup()
  
  # Plot: facet by factor, x axis is factor_value_ordered, y is value, color/fill by metric
  p <- ggplot(analysis_long, aes(x = factor_value_ordered, y = value, color = metric, fill = metric))
  if (plot_type == "scatter") {
    p <- p + geom_jitter(position = position_dodge(width = 0.75), alpha = 0.7)
  } else if (plot_type == "boxplot") {
    p <- p + geom_boxplot(outlier.alpha = 0.3, position = position_dodge(width = 0.75), alpha = 0.5)
  }
  p <- p +
    facet_wrap(~factor, scales = "free_x") +
    labs(title = title,
         x = "Factor Value",
         y = "Metric Value",
         color = "Metric",
         fill = "Metric")
  # Add secondary axis for gauge if it's scaled and present
  if ("gauge" %in% metrics && gauge_scale_factor > 1 && !separate_metrics) {
    p <- p + scale_y_continuous(
      sec.axis = sec_axis(~ . / gauge_scale_factor, name = "Gauge (original scale)")
    )
  }
  
  # Option to create separate plots for each metric
  if (separate_metrics && length(metrics) > 1) {
    library(patchwork)
    
    # Create a list to store individual plots
    plot_list <- list()
    
    for (metric_name in metrics) {
      # Get data for this specific metric
      metric_data <- analysis_long %>%
        filter(metric == metric_name)
      
      # Revert gauge scaling if needed for separate plot
      if (metric_name == "gauge" && gauge_scale_factor > 1) {
        metric_data <- metric_data %>%
          mutate(value = value / gauge_scale_factor)
      }
      
      # Create plot for this metric
      p_metric <- ggplot(metric_data, aes(x = factor_value_ordered, y = value, color = metric, fill = metric))
      if (plot_type == "scatter") {
        p_metric <- p_metric + geom_jitter(position = position_dodge(width = 0.75), alpha = 0.7)
      } else if (plot_type == "boxplot") {
        p_metric <- p_metric + geom_boxplot(outlier.alpha = 0.3, position = position_dodge(width = 0.75), alpha = 0.5)
      }
      
      p_metric <- p_metric +
        facet_wrap(~factor, scales = "free_x") +
        labs(title = paste(title, paste0("(", stringr::str_to_title(metric_name), ")")),
             x = "Factor Value",
             y = paste(stringr::str_to_title(metric_name), "Value"),
             color = "Metric",
             fill = "Metric") +
        theme(legend.position = "bottom")
            
      plot_list[[metric_name]] <- p_metric
    }
    
    # Combine all plots horizontally
    return(wrap_plots(plot_list, nrow = 1))
  }
  
  p
}

plot_compare_metrics <- function(analyses, study_names = NULL, metrics = c("avg_gauge", "avg_potency", "avg_f1"), 
                                plot_type = "bar", facet_by = "tolerance", color_by = "indic_method", title = "Average Metric Comparison Across Studies") {
  # Handle single analysis input
  if (!is.list(analyses) || !is.null(names(analyses)) && all(c("per_simulation", "by_method") %in% names(analyses))) {
    analyses <- list(Study1 = analyses)
  }
  
  # Generate study names if not provided
  if (is.null(study_names)) {
    study_names <- if (!is.null(names(analyses))) {
      names(analyses)
    } else {
      paste0("Study", seq_along(analyses))
    }
  }
  
  # Combine all by_method tibbles with study identifiers
  combined_data <- purrr::map2_dfr(analyses, study_names, function(analysis, study_name) {
    if (!is.null(analysis$by_method)) {
      analysis$by_method %>%
        mutate(study = study_name)
    } else {
      # If analyses is a list of by_method tibbles directly
      analysis %>%
        mutate(study = study_name)
    }
  })
  
  print(paste("Available metrics:", paste(names(combined_data), collapse = ", ")))
  print(paste("Requested metrics:", paste(metrics, collapse = ", ")))
  
  # Check which metrics are available
  available_metrics <- intersect(metrics, names(combined_data))
  if (length(available_metrics) == 0) {
    stop("None of the requested metrics are available in the data.")
  }
  
  # Pivot data for plotting
  plot_data <- combined_data %>%
    pivot_longer(
      cols = all_of(available_metrics),
      names_to = "metric",
      values_to = "value"
    ) %>%
    # Clean up metric names for better display
    mutate(
      metric_clean = case_when(
        metric == "avg_gauge" ~ "Gauge",
        metric == "avg_potency" ~ "Potency", 
        metric == "avg_precision" ~ "Precision",
        metric == "avg_recall" ~ "Recall",
        metric == "avg_f1" ~ "F1 Score",
        TRUE ~ stringr::str_remove(metric, "avg_") %>% stringr::str_to_title()
      ),
      tolerance = as.factor(tolerance),
      t.pval = as.factor(t.pval)
    )
  # Create base plot
  p <- ggplot(plot_data, aes_string(x = color_by, y = "value"))
  
  if (plot_type == "bar") {
    p <- p + geom_col(aes(fill = study), position = "dodge", alpha = 0.8)
  } else if (plot_type == "point") {
    p <- p + geom_point(aes(color = study, shape = study), size = 3, alpha = 0.8)
    
    # For lines, calculate mean across p_val groups to avoid connecting unrelated points
    if (length(unique(plot_data$t.pval)) > 1) {
      line_data <- plot_data %>%
        group_by(study, indic_method, tolerance, metric, metric_clean) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
      
      p <- p + geom_line(data = line_data, aes(color = study, group = study), alpha = 0.6)
    } else {
      p <- p + geom_line(aes(color = study, group = study), alpha = 0.6)
    }
  } else if (plot_type == "line") {
    # For line plots, always use mean across p_val groups
    if (length(unique(plot_data$t.pval)) > 1) {
      line_data <- plot_data %>%
        group_by(study, indic_method, tolerance, metric, metric_clean) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
      
      p <- p + 
        geom_line(data = line_data, aes(color = study, group = study), size = 1, alpha = 0.8) +
        geom_point(data = line_data, aes(color = study), size = 2)
      
      # Also show individual p_val points with transparency
      p <- p + geom_point(aes(color = study), alpha = 0.3, size = 1)
    } else {
      p <- p + 
        geom_line(aes(color = study, group = study), size = 1, alpha = 0.8) +
        geom_point(aes(color = study), size = 2)
    }
  }
  
  # Add faceting
  if (facet_by == "tolerance") {
    p <- p + facet_grid(metric_clean ~ tolerance, scales = "free_y", 
                        labeller = labeller(tolerance = function(x) paste("Tolerance:", x)))
  } else if (facet_by == "metric") {
    p <- p + facet_wrap(~ metric_clean, scales = "free_y")
  } else if (facet_by == "t.pval") {
    p <- p + facet_grid(metric_clean ~ t.pval, scales = "free_y",
                        labeller = labeller(t.pval = function(x) paste("p-value:", x)))
  } else if (facet_by == "both") {
    p <- p + facet_grid(metric_clean ~ tolerance + t.pval, scales = "free_y",
                        labeller = labeller(tolerance = function(x) paste("Tol:", x),
                                          t.pval = function(x) paste("p:", x)))
  }
  
  # Styling
  p <- p +
    labs(
      title = title,
      x = stringr::str_to_title(gsub("_", " ", color_by)),
      y = "Average Metric Value",
      fill = "Study",
      color = "Study",
      shape = "Study"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 10),
      legend.position = "bottom"
    )
  
  # Add horizontal line at key values for reference
  if ("avg_gauge" %in% available_metrics) {
    # Add reference line at gauge = 0.05 (5% false positive rate)
    p <- p + geom_hline(data = filter(plot_data, metric_clean == "Gauge"), 
                        aes(yintercept = 0.05), linetype = "dashed", alpha = 0.5, color = "red")
  }
  
  return(p)
}

# Convenience function for quick comparison of two studies
plot_compare_two_studies <- function(study1, study2, study1_name = "Study 1", study2_name = "Study 2", ...) {
  studies <- list()
  studies[[study1_name]] <- study1
  studies[[study2_name]] <- study2
  
  plot_compare_metrics(studies, ...)
}