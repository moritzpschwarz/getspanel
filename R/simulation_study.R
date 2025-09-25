library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(gets)
library(data.table)

# devtools::load_all()
base_time <- 1900

impose_treatment <- function(type, n_time, location, magnitude, fe) {
  if (!(type %in% c("trend", "trendbreak", "step"))) {
    stop("treatment type not recognized")
  }

  # Initialize treatment vector and determine absolute location
  treatment <- rep(0, n_time)
  abs_location <- max(ceiling(n_time * location), 1)
  if (type == "trend") {
    # "Trend" always starts at the beginning
    treatment <- 1:n_time
  } else if (type == "trendbreak") {
    # "Trendbreak" starts at abs_location and sets increasing dummies
    treatment[abs_location:n_time] <- seq_along(treatment[abs_location:n_time])
  } else if (type == "step") {
    # "Step" sets a constant step-shift from abs_location
    treatment[abs_location:n_time] <- 1
  }
  treatment <- treatment * magnitude

  # Not sure why fixed effects are added again for steps, copied from Moritz
  if (type == "step") {
    treatment <- treatment + fe
  }

  list(treatment = treatment, time = abs_location)
}

create_input_data <- function(n_id, n_time, treatment_params, fe_sigma, beta, sigma, plot_data = FALSE) {
  # Initialize unit fixed effects and return vectors
  means <- rnorm(n_id, sd = fe_sigma)
  input_data <- data.frame()
  treatment_collection <- tibble()

  for (id in 1:n_id) {
    # Create random input data (x) and compute outcome variable (y)
    fe <- means[id]
    x <- matrix(rnorm(n_time * length(beta)), ncol = length(beta))
    eps <- rnorm(n_time, mean = 0, sd = sigma)
    y <- x %*% beta + fe + eps

    data <- data.frame(
      id = LETTERS[id],
      time = (1:n_time) + base_time,
      x = x,
      y = y
    )

    # Initialize treatment column to NA (only used to plot treatment impact)
    data$treatment <- NA
    if (id %in% treatment_params$id) {
      params <- treatment_params %>% filter(id == !!id)
      # Get effect and timing for each treatment
      # Add treatment effect to outcome and store timing for the collection
      for (i in seq_len(nrow(params))) {
        treat <- impose_treatment(
          type = params$type[i],
          n_time = n_time,
          location = params$location[i],
          magnitude = params$magnitude[i],
          fe = fe
        )
        data$y <- data$y + treat$treatment

        # Initialize treatment column to 0 if NA, then add treatment effect
        # This way, overlapping treatments are summed and units without any treatments remain with NA values
        data$treatment <- ifelse(is.na(data$treatment), 0, data$treatment)
        data$treatment <- data$treatment + treat$treatment

        treat_entry <- tibble(
          id = LETTERS[id],
          treated = params$type[i],
          time = treat$time
        )
        treatment_collection <- bind_rows(treatment_collection, treat_entry)
      }
    }
    input_data <- bind_rows(input_data, data)
  }

  if (plot_data == TRUE) {
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
        data = tmp %>% filter(plot_group %in% c("unit_fe", "treatment")),
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
          "treatment" = "#377EB8",
          "y" = "#4DAF4A"
        )
      ) +
      labs(color = "Variable")
    plot(p)
  }

  # treatment column was only used for plotting
  list(
    input_data = input_data %>% select(-treatment), treatment_collection = treatment_collection
  )
}

run_single_model <- function(n_id, n_time, engine, method, treatment_params, fe_sigma, beta, sigma, t.pval, ar, max.block.size, plot_data = FALSE, plot_isatpanel = FALSE) {
  # Create input data with imposed treatments and treatment information
  data_creation <- create_input_data(
    n_id = n_id,
    n_time = n_time,
    treatment_params = treatment_params,
    fe_sigma = fe_sigma,
    beta = beta,
    sigma = sigma,
    plot_data = plot_data
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
    effect = "individual",
    index = c("id", "time"),
    fesis = ifelse(method %in% c("fesis", "both"), TRUE, FALSE),
    tis = ifelse(method %in% c("tis", "both"), TRUE, FALSE),
    iis = FALSE,
    print.searchinfo = FALSE,
    t.pval = t.pval,
    ar = ar,
    plot = plot_isatpanel,
    max.block.size = max.block.size
  )

  # Return tibble with all relevant information for the simulation run
  tibble(
    n_id,
    n_time,
    getspanel_object = list(result),
    indicators = list(get_indicators(result)),
    treatment_collection = list(treatment_collection),
    engine,
    indic_method = method,
    t.pval = t.pval,
    ar = ar,
    max.block.size = max.block.size,
    adaptive = NA
  )
}

run_simulation_study <- function() {
  set.seed(99726)

  # Simulation parameters (panel structure and data generation)
  n_ids <- c(2, 3, 5, 10)
  n_times <- c(20, 30, 50)
  beta <- c(0.3, 0.7, -.3, 0, 0) # the betas for the coefficients
  sigma <- 0.5
  fe_sigma <- 5

  # Treatment parameters (imposed treatments to be detected)
  treatment_params_list <- list(
    tribble(
      ~id, ~type, ~magnitude, ~location,
      3, "step", 2, 0.2,
      5, "step", 2, 0.2,
      2, "trend", 0.2, 0,
      1, "trendbreak", -0.4, 0.65
    )
  )

  # Benchmark parameters (getspanel parameters to be varied)
  n_rep <- 1
  engines <- c("gets")
  methods <- c("both")
  t.pvals <- c(0.05, 0.01, 0.001)
  ars <- c(0)
  max.block.sizes <- c(30)

  n_simulations <- length(engines) * length(methods) * length(t.pvals) * length(ars) * length(max.block.sizes) * length(treatment_params_list) * length(n_times) * length(n_ids) * n_rep
  print(paste("Total simulations to run:", n_simulations))
  overall <- tibble()
  for (engine in engines) {
    for (method in methods) {
      for (t.pval in t.pvals) {
        for (ar in ars) {
          for (max.block.size in max.block.sizes) {
            for (treatment_params in treatment_params_list) {
              for (n_time in n_times) {
                for (n_id in n_ids) {
                  for (rep in 1:n_rep) {
                    print(paste("Running simulation number =", nrow(overall) + 1, "/", n_simulations, "with method =", method, ", t.pval =", t.pval, ", ar =", ar, ", max.block.size =", max.block.size, ", n_time =", n_time, ", n_id =", n_id, ", rep =", rep))
                    result <- run_single_model(
                      n_id = n_id,
                      n_time = n_time,
                      engine = engine,
                      method = method,
                      treatment_params = treatment_params,
                      fe_sigma = fe_sigma,
                      beta = beta,
                      sigma = sigma,
                      t.pval = t.pval,
                      ar = ar,
                      max.block.size = max.block.size
                    )
                    # Add simulation_id and place it at the front
                    result <- result %>%
                      mutate(simulation_id = nrow(overall) + 1) %>%
                      select(simulation_id, everything())

                    overall <- bind_rows(overall, result)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  overall
}

# Treatment Extraction
extract_treatments <- function(overall_tibble) {
  # Extract true and detected treatments from the overall tibble
  true_treatments <- overall_tibble %>%
    select(treatment_collection, simulation_id) %>%
    unnest(treatment_collection)

  if(nrow(true_treatments) == 0) {
    true_treatments <- tibble(simulation_id = integer(0), id = character(0), type = character(0), timing = integer(0))
  } else {
    true_treatments <- true_treatments %>%
      mutate(type = ifelse(treated == "trendbreak", "trend", treated)) %>%
      select(simulation_id, id, type, timing = time)
  }

  detected_treatments <- overall_tibble %>%
    select(indicators, simulation_id) %>%
    unnest(indicators) %>%
    unnest(indicators) %>%
    select(simulation_id, id, time, name) %>%
    rowwise() %>%
    mutate(
      timing = time - base_time,
      type = case_when(
        grepl("^fesis", name) ~ "step",
        grepl("^tis", name) ~ "trend"
      )
    ) %>%
    ungroup() %>%
    select(simulation_id, id, type, timing)

  list(
    true_treatments = true_treatments,
    detected_treatments = detected_treatments
  )
}

#' Treatment Matches
#' Allows for small timing errors (e.g., ±1 or ±2 periods or type mismatches)
#' Only allows one detected treatment to match each true treatment (best match wins)
match_treatments <- function(true_treatments = NULL, detected_treatments = NULL, overall = NULL, tolerance = 0, allow_type_mismatch = FALSE) {
  if (!is.null(overall)) {
    treatments <- extract_treatments(overall)
    true_treatments <- treatments$true_treatments
    detected_treatments <- treatments$detected_treatments
  } else if (is.null(true_treatments) | is.null(detected_treatments)) {
    stop("Either overall or both true_treatments and detected_treatments must be provided.")
  }

  # Always use both type columns for consistency
  true_treatments <- true_treatments %>%
    rename(true_timing = timing, true_type = type)
  detected_treatments <- detected_treatments %>%
    rename(detected_timing = timing, detected_type = type)

  # Create all potential matches within tolerance
  potential_matches <- full_join(
    true_treatments,
    detected_treatments,
    by = c("simulation_id", "id")
  ) %>%
    mutate(
      timing_diff = abs(true_timing - detected_timing),
      type_mismatch = true_type != detected_type
    ) %>%
    filter(timing_diff <= tolerance)
  
  # Implement one-to-one matching using greedy algorithm
  # Create composite score: timing difference primary, type mismatch secondary (if allowed)
  if (allow_type_mismatch) {
    potential_matches <- potential_matches %>%
      mutate(composite_score = timing_diff + (as.numeric(type_mismatch) * (tolerance + 1)))
  } else {
    # Filter by type matching if not allowed
    potential_matches <- potential_matches %>%
      filter(!type_mismatch) %>%
      mutate(composite_score = timing_diff)
  }
  
  matches <- potential_matches %>%
    arrange(composite_score) %>%
    group_by(simulation_id) %>%
    # Track used true and detected treatments
    mutate(
      true_key = paste(id, true_timing, true_type, sep = "_"),
      detected_key = paste(id, detected_timing, detected_type, sep = "_")
    ) %>%
    # Select matches greedily (best matches first, no duplicates)
    filter(!duplicated(true_key) & !duplicated(detected_key)) %>%
    select(-true_key, -detected_key, -composite_score) %>%
    ungroup()

  matches <- matches %>%
    mutate(match = TRUE)

  # Find unmatched true treatments - always use same column structure
  unmatched_true <- anti_join(
    true_treatments,
    matches,
    by = c("simulation_id", "id", "true_timing", "true_type")
  ) %>%
    mutate(
      detected_timing = NA_real_,
      detected_type = NA_character_,
      timing_diff = NA_real_,
      type_mismatch = NA,
      match = FALSE
    )

  # Find unmatched detected treatments - always use same column structure
  unmatched_detected <- anti_join(
    detected_treatments,
    matches,
    by = c("simulation_id", "id", "detected_timing", "detected_type")
  ) %>%
    mutate(
      true_timing = NA_real_,
      true_type = NA_character_,
      timing_diff = NA_real_,
      type_mismatch = NA,
      match = FALSE
    )

  # Combine all
  all_results <- bind_rows(matches, unmatched_true, unmatched_detected)

  all_results
}

#' Optimal Bipartite Matching for Treatment Detection
#' Uses optimal matching algorithm to find best one-to-one assignment
optimal_match_treatments <- function(true_treatments = NULL, detected_treatments = NULL, overall = NULL, tolerance = 0, allow_type_mismatch = FALSE) {
  if (!is.null(overall)) {
    treatments <- extract_treatments(overall)
    true_treatments <- treatments$true_treatments
    detected_treatments <- treatments$detected_treatments
  } else if (is.null(true_treatments) | is.null(detected_treatments)) {
    stop("Either overall or both true_treatments and detected_treatments must be provided.")
  }

  # Setup consistent column names
  true_treatments <- true_treatments %>%
    rename(true_timing = timing, true_type = type) %>%
    mutate(true_idx = row_number())
  
  detected_treatments <- detected_treatments %>%
    rename(detected_timing = timing, detected_type = type) %>%
    mutate(detected_idx = row_number())

  # Process each (simulation_id, id) pair separately
  all_matches <- tibble()
  
  # Get all unique (simulation_id, id) combinations
  id_combinations <- unique(rbind(
    true_treatments %>% select(simulation_id, id),
    detected_treatments %>% select(simulation_id, id)
  ))
  
  for (i in 1:nrow(id_combinations)) {
    sim_id <- id_combinations$simulation_id[i]
    entity_id <- id_combinations$id[i]
    
    true_subset <- filter(true_treatments, simulation_id == sim_id, id == entity_id)
    detected_subset <- filter(detected_treatments, simulation_id == sim_id, id == entity_id)
    
    if (nrow(true_subset) == 0 || nrow(detected_subset) == 0) {
      next
    }

    # Create cost matrix for this specific (simulation_id, id) pair
    cost_matrix <- expand_grid(
      true_idx = true_subset$true_idx,
      detected_idx = detected_subset$detected_idx
    ) %>%
      left_join(true_subset, "true_idx") %>%
      left_join(detected_subset, c("detected_idx", "id", "simulation_id")) %>%
      mutate(
        timing_diff = abs(true_timing - detected_timing),
        type_mismatch = true_type != detected_type,
        # Create cost: high cost for invalid matches
        cost = case_when(
          timing_diff > tolerance ~ Inf,
          !allow_type_mismatch & type_mismatch ~ Inf,
          TRUE ~ timing_diff + (as.numeric(type_mismatch) * (tolerance + 1))
        )
      )
    
    # Find optimal assignment for this (simulation_id, id) pair
    pair_matches <- find_optimal_assignment(cost_matrix, true_subset, detected_subset)
    all_matches <- bind_rows(all_matches, pair_matches)
  }

  # Add unmatched treatments
  if (nrow(all_matches) == 0) {
    matched_true <- c()
    matched_detected <- c()
  } else {
    matched_true <- all_matches %>% filter(match) %>% pull(true_idx)
    matched_detected <- all_matches %>% filter(match) %>% pull(detected_idx)
  }

  unmatched_true <- true_treatments %>%
    filter(!true_idx %in% matched_true) %>%
    mutate(
      detected_timing = NA_real_,
      detected_type = NA_character_,
      detected_idx = NA_integer_,
      timing_diff = NA_real_,
      type_mismatch = NA,
      match = FALSE
    )
  
  unmatched_detected <- detected_treatments %>%
    filter(!detected_idx %in% matched_detected) %>%
    mutate(
      true_timing = NA_real_,
      true_type = NA_character_,
      true_idx = NA_integer_,
      timing_diff = NA_real_,
      type_mismatch = NA,
      match = FALSE
    )
  
  all_results <- bind_rows(all_matches, unmatched_true, unmatched_detected) %>%
    select(-true_idx, -detected_idx, -cost)

  all_results
}

#' Find Optimal Assignment using brute force enumeration
#' Prioritizes: 1) Maximum number of matches, 2) Minimum total cost
find_optimal_assignment <- function(cost_matrix, true_sim, detected_sim) {
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

relevant_types_for_method <- function(method) {
  if (method == "fesis") {
    return("step")
  } else if (method == "tis") {
    return("trend")
  } else if (method == "both") {
    return(c("step","trend"))
  } else {
    return(character(0))
  }
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

compute_metrics <- function(overall_tibble, tolerance = 0) {
  meta <- overall_tibble %>%
    dplyr::select(n_id, n_time, indic_method, simulation_id, t.pval)

  tx <- extract_treatments(overall_tibble)
  true_all <- tx$true_treatments
  det_all  <- tx$detected_treatments

  purrr::pmap_dfr(meta, function(n_id, n_time, indic_method, simulation_id, t.pval) {
    rel_types <- relevant_types_for_method(indic_method)
    total_candidates <- candidate_count(n_id, n_time, indic_method)

    true_sim <- true_all %>%
      dplyr::filter(simulation_id == !!simulation_id, type %in% rel_types)
    det_sim  <- det_all %>%
      dplyr::filter(simulation_id == !!simulation_id, type %in% rel_types)

    matches <- match_treatments(
      true_treatments = true_sim,
      detected_treatments = det_sim,
      tolerance = tolerance
    )

    tp  <- nrow(matches %>% dplyr::filter(match))
    det <- nrow(det_sim)
    fp  <- max(det - tp, 0)

    rel    <- nrow(true_sim)
    irrel  <- max(total_candidates - rel, 0)

    prec <- ifelse(det > 0, tp / det, NA_real_)
    rec <- ifelse(rel > 0, tp / rel, NA_real_)

    tibble::tibble(
      simulation_id = simulation_id,
      n_id = n_id,
      n_time = n_time,
      indic_method = indic_method,
      t.pval = t.pval,
      tolerance = tolerance,
      gauge   = ifelse(irrel > 0, fp / irrel, NA_real_),
      potency = ifelse(rel  > 0, tp / rel, NA_real_ ),
      precision = prec,
      recall = rec,
      f1 = ifelse(prec + rec > 0, 2 * (prec * rec) / (prec + rec), NA_real_),
      detected = det,
      true = rel,
      matches = list(matches)
    )
  })
}

metrics_summary <- function(overall_tibble, tolerances = c(0, 1)) {
  gp <- dplyr::bind_rows(
    lapply(tolerances, function(t) {
      compute_metrics(overall_tibble, t)
    })
  )

  by_method <- gp %>%
    dplyr::group_by(indic_method, tolerance, t.pval) %>%
    dplyr::summarise(
      avg_gauge = mean(gauge, na.rm = TRUE),
      avg_potency = mean(potency, na.rm = TRUE),
      avg_precision = mean(precision, na.rm = TRUE),
      avg_recall = mean(recall, na.rm = TRUE),
      avg_f1 = mean(f1, na.rm = TRUE),
      .groups = "drop"
    )

  list(per_simulation = gp, by_method = by_method)
}
