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
  n_times <- c(20, 30, 50, 100)
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
  methods <- c("fesis", "tis", "both")
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
    unnest(treatment_collection) %>%
    mutate(type = ifelse(treated == "trendbreak", "trend", treated)) %>%
    select(simulation_id, id, type, timing = time)

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

  precision <- ifelse(
    nrow(detected_treatments) > 0,
    nrow(matches) / nrow(detected_treatments),
    0
  )
  recall <- ifelse(
    nrow(true_treatments) > 0,
    nrow(matches) / nrow(true_treatments),
    0
  )
  f1_score <- ifelse(
    precision + recall > 0,
    2 * (precision * recall) / (precision + recall),
    0
  )

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

  list(
    matches = all_results,
    precision = precision,
    recall = recall,
    f1_score = f1_score
  )
}

# Timing tolerance over type mismatch precedence example
true_treatment <- tribble(
  ~simulation_id, ~id, ~type, ~timing,
  1, "A", "step", 5,
  1, "B", "step", 5,
  1, "C", "step", 5,
  1, "D", "step", 5,
)
detected_treatments <- tribble(
  ~simulation_id, ~id, ~type, ~timing,
  1, "A", "step", 5,
  1, "A", "step", 6,
  1, "B", "trend", 5,
  1, "B", "step", 10,
  1, "C", "trend", 5,
  1, "C", "step", 6,
  1, "D", "trend", 6,
  1, "D", "step", 6,
)


true_treatment <- tribble(
  ~simulation_id, ~id, ~type, ~timing,
  1, "A", "step", 5,
  1, "A", "trend", 8,
  1, "A", "step", 10,
)
detected_treatments <- tribble(
  ~simulation_id, ~id, ~type, ~timing,
  1, "A", "step", 5,
  1, "A", "step", 6,
  1, "A", "trend", 8,
)

#' Analyze False Detections
#' Categorizes false positives and false negatives
analyze_false_detections <- function(true_treatments, detected_treatments) {
  # False positives: detected but not true
  true_key <- paste(true_treatments$id, true_treatments$type, true_treatments$timing, sep = "_")
  detected_key <- paste(detected_treatments$id, detected_treatments$type, detected_treatments$timing, sep = "_")
  
  false_positives <- detected_treatments[!detected_key %in% true_key, ]
  false_negatives <- true_treatments[!true_key %in% detected_key, ]
  
  fp_rate <- if(nrow(detected_treatments) > 0) nrow(false_positives) / nrow(detected_treatments) else 0
  fn_rate <- if(nrow(true_treatments) > 0) nrow(false_negatives) / nrow(true_treatments) else 0
  
  list(
    false_positives = false_positives,
    false_negatives = false_negatives,
    fp_rate = fp_rate,
    fn_rate = fn_rate
  )
}

#' Comprehensive Treatment Detection Evaluation
#' Main evaluation function that combines all metrics
evaluate_treatment_detection <- function(overall_tibble, timing_tolerances = c(0, 1, 2)) {
  
  # Extract simulation metadata
  sim_metadata <- overall_tibble %>%
    select(n_id, n_time, indic_method) %>%
    mutate(simulation_id = seq_len(n()))

  # Extract true and detected treatments
  treatments <- extract_treatments(overall_tibble)
  true_treatments <- treatments$true_treatments
  detected_treatments <- treatments$detected_treatments

  # Run evaluation for each simulation
  results <- list()
  
  for(sim_id in unique(true_treatments$simulation_id)) {
    true_sim <- true_treatments %>% filter(simulation_id == sim_id)
    detected_sim <- detected_treatments %>% filter(simulation_id == sim_id)
    sim_meta <- sim_metadata %>% filter(simulation_id == sim_id)
    
    sim_results <- list()
    sim_results$simulation_id <- sim_id
    sim_results$n_id <- sim_meta$n_id
    sim_results$n_time <- sim_meta$n_time
    sim_results$indic_method <- sim_meta$indic_method
    
    # 1. Exact matches
    sim_results$exact <- match_treatments(true_treatments = true_sim, detected_treatments = detected_sim)
    
    # 2. Timing tolerance analysis
    sim_results$timing_tolerance <- lapply(timing_tolerances, function(tol) {
      match_treatments(true_treatments = true_sim, detected_treatments = detected_sim, tolerance = tol)
    })
    names(sim_results$timing_tolerance) <- paste0("tolerance_", timing_tolerances)
    
    # 3. Type confusion
    sim_results$type_confusion <- match_treatments(
      true_treatments = true_sim,
      detected_treatments = detected_sim,
      tolerance = 0,
      allow_type_mismatch = TRUE
    )$matches %>%
      filter(type_mismatch)

    # 4. False positives and negatives
    sim_results$false_analysis <- analyze_false_detections(true_sim, detected_sim)
    
    results[[sim_id]] <- sim_results
  }
  
  return(results)
}

#' Create Summary Table
#' Aggregates results across simulations including method comparison
create_summary_table <- function(evaluation_results) {
  summary_data <- data.frame()
  
  for(i in seq_along(evaluation_results)) {
    result <- evaluation_results[[i]]
    
    row <- data.frame(
      simulation_id = result$simulation_id,
      n_id = result$n_id,
      n_time = result$n_time,
      indic_method = result$indic_method,
      exact_precision = result$exact$precision,
      exact_recall = result$exact$recall,
      exact_f1 = result$exact$f1_score,
      tol1_precision = result$timing_tolerance$tolerance_1$precision,
      tol1_recall = result$timing_tolerance$tolerance_1$recall,
      tol2_precision = result$timing_tolerance$tolerance_2$precision,
      tol2_recall = result$timing_tolerance$tolerance_2$recall,
      fp_rate = result$false_analysis$fp_rate,
      fn_rate = result$false_analysis$fn_rate,
      n_type_confusions = nrow(result$type_confusion)
    )
    
    summary_data <- rbind(summary_data, row)
  }
  
  return(summary_data)
}

#' Plot Detection Performance
#' Visualizes performance across timing tolerances
plot_detection_performance <- function(evaluation_results) {
  library(ggplot2)
  library(reshape2)
  
  summary_table <- create_summary_table(evaluation_results)
  
  # Performance across timing tolerances - fix the data transformation
  tol_data <- summary_table %>%
    summarise(
      tolerance_0_precision = mean(exact_precision, na.rm = TRUE),
      tolerance_0_recall = mean(exact_recall, na.rm = TRUE),
      tolerance_1_precision = mean(tol1_precision, na.rm = TRUE),
      tolerance_1_recall = mean(tol1_recall, na.rm = TRUE),
      tolerance_2_precision = mean(tol2_precision, na.rm = TRUE),
      tolerance_2_recall = mean(tol2_recall, na.rm = TRUE)
    ) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    mutate(
      tolerance = case_when(
        grepl("tolerance_0", metric) ~ 0,
        grepl("tolerance_1", metric) ~ 1,
        grepl("tolerance_2", metric) ~ 2,
        TRUE ~ NA_real_
      ),
      measure = case_when(
        grepl("precision", metric) ~ "precision",
        grepl("recall", metric) ~ "recall",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(tolerance) & !is.na(measure))

  print(tol_data)
  
  ggplot(tol_data, aes(x = tolerance, y = value, color = measure)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(title = "Detection Performance vs Timing Tolerance",
         x = "Timing Tolerance (periods)",
         y = "Performance Metric") +
    theme_minimal()
}

#' Compare Method Performance
#' Compares performance across different indicator methods
compare_method_performance <- function(evaluation_results) {
  library(ggplot2)
  
  summary_table <- create_summary_table(evaluation_results)
  
  # Aggregate by method
  method_comparison <- summary_table %>%
    group_by(indic_method) %>%
    summarise(
      avg_exact_precision = mean(exact_precision, na.rm = TRUE),
      avg_exact_recall = mean(exact_recall, na.rm = TRUE),
      avg_exact_f1 = mean(exact_f1, na.rm = TRUE),
      avg_tol1_precision = mean(tol1_precision, na.rm = TRUE),
      avg_tol1_recall = mean(tol1_recall, na.rm = TRUE),
      avg_tol2_precision = mean(tol2_precision, na.rm = TRUE),
      avg_tol2_recall = mean(tol2_recall, na.rm = TRUE),
      avg_fp_rate = mean(fp_rate, na.rm = TRUE),
      avg_fn_rate = mean(fn_rate, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create multiple comparison plots
  plots <- list()
  
  # 1. Precision and Recall comparison
  precision_recall_data <- method_comparison %>%
    select(indic_method, avg_exact_precision, avg_exact_recall, 
           avg_tol1_precision, avg_tol1_recall, avg_tol2_precision, avg_tol2_recall) %>%
    pivot_longer(-indic_method, names_to = "metric", values_to = "value") %>%
    mutate(
      tolerance = case_when(
        grepl("exact", metric) ~ "0",
        grepl("tol1", metric) ~ "1", 
        grepl("tol2", metric) ~ "2",
        TRUE ~ NA_character_
      ),
      measure = case_when(
        grepl("precision", metric) ~ "Precision",
        grepl("recall", metric) ~ "Recall",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(tolerance) & !is.na(measure))
  
  plots$precision_recall <- ggplot(precision_recall_data, 
                                   aes(x = tolerance, y = value, 
                                       color = indic_method, group = indic_method)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    facet_wrap(~measure) +
    labs(title = "Precision and Recall by Method and Tolerance",
         x = "Timing Tolerance",
         y = "Performance",
         color = "Method") +
    theme_minimal()
  
  # 2. F1 Score comparison
  plots$f1_score <- ggplot(method_comparison, 
                           aes(x = indic_method, y = avg_exact_f1, fill = indic_method)) +
    geom_col() +
    labs(title = "F1 Score by Method (Exact Matches)",
         x = "Indicator Method",
         y = "Average F1 Score") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # 3. False Positive and False Negative rates
  fp_fn_data <- method_comparison %>%
    select(indic_method, avg_fp_rate, avg_fn_rate) %>%
    pivot_longer(-indic_method, names_to = "rate_type", values_to = "rate") %>%
    mutate(rate_type = case_when(
      rate_type == "avg_fp_rate" ~ "False Positive Rate",
      rate_type == "avg_fn_rate" ~ "False Negative Rate",
      TRUE ~ rate_type
    ))
  
  plots$fp_fn_rates <- ggplot(fp_fn_data, 
                              aes(x = indic_method, y = rate, fill = rate_type)) +
    geom_col(position = "dodge") +
    labs(title = "False Positive and False Negative Rates by Method",
         x = "Indicator Method",
         y = "Rate",
         fill = "Rate Type") +
    theme_minimal()
  
  return(plots)
}

#' Detailed Method Analysis
#' Provides detailed breakdown by method, n_time, and n_id
detailed_method_analysis <- function(evaluation_results) {
  library(ggplot2)
  library(dplyr)
  
  summary_table <- create_summary_table(evaluation_results)
  
  # Performance by method, n_time, and n_id
  detailed_comparison <- summary_table %>%
    group_by(indic_method, n_time, n_id) %>%
    summarise(
      avg_exact_precision = mean(exact_precision, na.rm = TRUE),
      avg_exact_recall = mean(exact_recall, na.rm = TRUE),
      avg_exact_f1 = mean(exact_f1, na.rm = TRUE),
      avg_fp_rate = mean(fp_rate, na.rm = TRUE),
      avg_fn_rate = mean(fn_rate, na.rm = TRUE),
      n_simulations = n(),
      .groups = "drop"
    )
  
  plots <- list()
  
  # 1. F1 Score by n_time and method
  plots$f1_by_time <- ggplot(detailed_comparison, 
                             aes(x = as.factor(n_time), y = avg_exact_f1, 
                                 color = indic_method, group = indic_method)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    facet_wrap(~n_id, labeller = label_both) +
    labs(title = "F1 Score by Time Periods and Number of IDs",
         x = "Number of Time Periods",
         y = "Average F1 Score",
         color = "Method") +
    theme_minimal()
  
  # 2. Precision vs Recall scatter
  plots$precision_recall_scatter <- ggplot(detailed_comparison, 
                                           aes(x = avg_exact_precision, y = avg_exact_recall, 
                                               color = indic_method, shape = as.factor(n_id))) +
    geom_point(size = 3, alpha = 0.7) +
    facet_wrap(~n_time, labeller = label_both) +
    labs(title = "Precision vs Recall Trade-off by Method",
         x = "Average Precision",
         y = "Average Recall",
         color = "Method",
         shape = "Number of IDs") +
    theme_minimal()
  
  # 3. Heatmap of F1 scores
  plots$f1_heatmap <- ggplot(detailed_comparison, 
                             aes(x = as.factor(n_time), y = as.factor(n_id), fill = avg_exact_f1)) +
    geom_tile() +
    facet_wrap(~indic_method) +
    scale_fill_gradient(low = "white", high = "darkblue") +
    labs(title = "F1 Score Heatmap by Method",
         x = "Number of Time Periods",
         y = "Number of IDs",
         fill = "Average F1 Score") +
    theme_minimal()
  
  return(list(plots = plots, data = detailed_comparison))
}

#' Method Performance Summary Table
#' Creates a summary table comparing methods
method_performance_summary <- function(evaluation_results) {
  summary_table <- create_summary_table(evaluation_results)
  
  method_summary <- summary_table %>%
    group_by(indic_method) %>%
    summarise(
      n_simulations = n(),
      avg_exact_precision = round(mean(exact_precision, na.rm = TRUE), 3),
      sd_exact_precision = round(sd(exact_precision, na.rm = TRUE), 3),
      avg_exact_recall = round(mean(exact_recall, na.rm = TRUE), 3),
      sd_exact_recall = round(sd(exact_recall, na.rm = TRUE), 3),
      avg_exact_f1 = round(mean(exact_f1, na.rm = TRUE), 3),
      sd_exact_f1 = round(sd(exact_f1, na.rm = TRUE), 3),
      avg_fp_rate = round(mean(fp_rate, na.rm = TRUE), 3),
      avg_fn_rate = round(mean(fn_rate, na.rm = TRUE), 3),
      .groups = "drop"
    )
  
  return(method_summary)
}

#' Plot Number of Breaks Detected per Simulation
#' Simple scatter plot of number of breaks in the indicators column of the overall tibble
plot_number_of_breaks <- function(overall_tibble, plot_type = "scatter", factors = NULL) {
  treatments <- extract_treatments(overall_tibble)
  true_treatments <- treatments$true_treatments
  detected_treatments <- treatments$detected_treatments

  true_treatments <- true_treatments %>%
    group_by(simulation_id) %>%
    summarise(n_true = n(), .groups = "drop") %>%
    select(simulation_id, n_true)

  detected_treatments <- detected_treatments %>%
    group_by(simulation_id) %>%
    summarise(n_detected = n(), .groups = "drop") %>%
    select(simulation_id, n_detected)

  overall_tibble <- left_join(overall_tibble, true_treatments, by = "simulation_id")
  overall_tibble <- left_join(overall_tibble, detected_treatments, by = "simulation_id")

  # break_counts <- overall_tibble %>%
  #   mutate(num_breaks = map_int(indicators, function(.x) {
  #     if (is.null(.x) || length(.x) == 0) {
  #       0
  #     } else if (is.data.frame(.x)) {
  #       nrow(.x)
  #     } else {
  #       nrow(.x[[1]])
  #     }
  #   })) %>%
  #   mutate(simulation_id = row_number())

  # Identify varying factors (exclude indicators, treatment_collection, getspanel_object, simulation_id, num_breaks)
  if (is.null(factors)) {
    varying_factors <- setdiff(
      names(overall_tibble),
      c("indicators", "treatment_collection", "getspanel_object", "simulation_id", "num_breaks", "n_true", "n_detected")
    )
  }

  if (length(varying_factors) == 0) {
    print("No varying factors found for plotting, plotting simulation_id vs num_breaks as scatter plot.")
    # No varying factors, just plot simulation_id vs num_breaks
    p <- ggplot(overall_tibble, aes(x = simulation_id, y = n_detected, color = n_true)) +
      geom_point() +
      labs(title = "Number of Breaks Detected per Simulation",
           x = "Simulation ID",
           y = "Number of Breaks")
  } else {
    print(paste("Varying factors identified for plotting:", paste(varying_factors, collapse = ", ")))

    # Gather into long format: one row per simulation per factor
    overall_tibble <- overall_tibble %>%
      mutate(across(all_of(varying_factors), as.character)) %>%
      pivot_longer(cols = all_of(varying_factors), names_to = "factor", values_to = "factor_value")

    # Plot: facet by factor, x axis is factor_value, y is num_breaks
    p <- ggplot(overall_tibble, aes(x = as.factor(factor_value), y = n_detected, color = n_true))
    if (plot_type == "scatter") {
      p <- p + geom_jitter(width = 0.2, height = 0, alpha = 0.7)
    } else if (plot_type == "boxplot") {
      p <- p + geom_boxplot(outlier.alpha = 0.3)
    }
    p <- p +
      facet_wrap(~factor, scales = "free_x") +
      labs(title = "Number of Breaks Detected per Simulation (by Factor)",
          x = "Factor Value",
          y = "Number of Breaks")
  }
  p
}
