library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(gets)
library(data.table)
# library(getspanel)

# global parameters
# base_time <- 1900

# sigma <- 0.5
# fe_sigma <- 5

# #beta <- c(0.3)
# beta <- c(0.3, 0.7, -.3, 0, 0) # the betas for the coefficients

# For reference, keep the original variables (optional)
# trend_mag <- 0.2 # trend magnitude
# trendbreak_loc <- 0.65 # location of the trendbreak (% of the sample)
# trendbreak_mag <- -0.4 # trend after the break
# strbreak_loc <- 0.2 # location of the structural break (% of the sample)
# strbreak_mag <- 2 # will be fe + (1 + step_mag)
# treated_step <- c(3,5)
# treated_trend <- c(2)
# treated_trendbreak <- 1 # probably country 1 should be untreated... not done yet

# Define treatment parameters in a tidy data frame
# treatment_params <- tibble::tibble(
#   id = c(3, 5, 2, 1),
#   type = c("step", "step", "trend", "trendbreak"),
#   magnitude = c(0.2, 0.2, 0.2, -0.4),
#   location = c(0.2, 0.2, NA, 0.65)
# )

# create_input_data <- function(id, n_id, n_time, beta, sigma, treated_step, treated_trend, treated_trendbreak) {
create_input_data <- function(id, fe, n_time, beta, sigma) {
  # browser()
  x <- matrix(rnorm(n_time * length(beta)), ncol = length(beta))
  eps <- rnorm(n_time, mean = 0, sd = sigma)
  # means <- rnorm(n_id, sd = fe_sigma) # maybe has to be outside to only be called once?
  # fe <- means[id]

  y <- x %*% beta + fe + eps
  # ggplot(data.frame(x = 1:n_time, y = y),aes(x = x, y = y)) + geom_line() -> p
  
  # Plot x, eps, means, fe, and y
  # df_plot <- data.frame(
  #   time = 1:n_time,
  #   x1 = x[,1],
  #   x2 = if(ncol(x) > 1) x[,2] else NA,
  #   x3 = if(ncol(x) > 2) x[,3] else NA,
  #   eps = eps,
  #   means = rep(means[id], n_time),
  #   fe = rep(fe, n_time),
  #   y = as.vector(y)
  # ) %>%
  #   pivot_longer(-time, names_to = "variable", values_to = "value")

  # ggplot(df_plot, aes(x = time, y = value, color = variable)) +
  #   geom_line() +
  #   labs(title = paste("id =", id)) -> p_vars
  # plot(p_vars)

  # treatments <- tibble()
  # if (id %in% treated_step) {
  #   treatment <- impose_treatment("step", n_time, strbreak_loc, strbreak_mag)
  #   y <- y + treatment$treatment
  #   treatments <- bind_rows(treatments, tibble(id = id, treated = "step", time = treatment$time))
  # } else if (id %in% treated_trend) {
  #   treatment <- impose_treatment("trend", n_time, NA, trend_mag)
  #   y <- y + treatment$treatment
  #   treatments <- bind_rows(treatments, tibble(id = id, treated = "step", time = treatment$time))
  # } else if (id %in% treated_trendbreak) {
  #   treatment <- impose_treatment("trendbreak", n_time, trendbreak_loc, trendbreak_mag)
  #   y <- y + treatment$treatment
  #   treatments <- bind_rows(treatments, tibble(id = id, treated = "step", time = treatment$time))
  # }

  input_data <- data.frame(id = id,
                           time = (1:n_time) + base_time,
                           x = x,
                           y = y)
  return(input_data)
  # return(list(input_data = input_data, treatments = treatments))
}

impose_treatment <- function(type, n_time, location, magnitude, fe) {
  if (!(type %in% c("trend", "trendbreak", "step"))) {
    stop("treatment type not recognized")
  }

  treatment <- rep(0, n_time)
  abs_location <- max(ceiling(n_time * location), 1)
  if (type == "trend") {
    treatment <- abs_location:n_time
  } else if (type == "trendbreak") {
    treatment[abs_location:n_time] <- seq_along(treatment[abs_location:n_time])
  } else if (type == "step") {
    treatment[abs_location:n_time] <- 1
  }
  treatment <- treatment * magnitude
  if (type == "step") {
    treatment <- treatment + fe
  }

  return(list(treatment = treatment, time = abs_location))
}

#   #if(i %in% treated_trendbreak & !i %in% treated_trend){stop("must have trend to impose trendbreak")}

#   # setup breaks, trends and trendbreaks
#   # trend
#   trend <- 1:n_time
  
#   # impose a trendbreak
#   trendbreak <- rep(0,n_time)
#   trendbreak[ceiling(n_time * trendbreak_loc):n_time] <- seq_along(trendbreak[ceiling(n_time*trendbreak_loc):n_time])
#   # 1:length(trendbreak[ceiling(n_time*trendbreak_loc):n_time])
  
#   # create structural break
#   strbreak <- rep(0,n_time)
#   strbreak[ceiling(n_time*strbreak_loc):n_time] <- 1

#   # trend
#   if (id %in% treated_trend) {
#     old_y <- y
#     trend_y <- trend * trend_mag
#     y <- y + trend * trend_mag
#     ggplot(data.frame(x = 1:n_time, y = y, old_y = old_y, trend_y = trend_y), aes(x = x)) +
#       geom_line(aes(y = old_y), color = "blue", linetype = "dashed") +
#       geom_line(aes(y = y), color = "red") +
#       geom_line(aes(y = trend_y)) -> p
#     # plot(p)
#   }
#   # y <- if(id %in% treated_trend){y + trend*trend_mag} else {y}
#   # ggplot(data.frame(x = 1:n_time, y = y),aes(x = x, y = y)) + geom_line() -> p
#   # # plot(p)

#   # trendbreak
#   if (id %in% treated_trendbreak) {
#     old_y <- y
#     trendbreak_y <- trendbreak * trendbreak_mag
#     y <- y + trendbreak * trendbreak_mag
#     ggplot(data.frame(x = 1:n_time, y = y, old_y = old_y, trendbreak_y = trendbreak_y), aes(x = x)) +
#       geom_line(aes(y = old_y), color = "blue", linetype = "dashed") +
#       geom_line(aes(y = y), color = "red") +
#       geom_line(aes(y = trendbreak_y)) -> p
#     # plot(p)
#   }
#   # y <- if(id %in% treated_trendbreak){y + trendbreak*trendbreak_mag} else {y}
#   # ggplot(data.frame(x = 1:n_time, y = y),aes(x = x, y = y)) + geom_line() -> p
#   # # plot(p)

#   # structural break
#   if (id %in% treated_step) {
#     old_y <- y
#     strbreak_y <- strbreak * strbreak_mag
#     y <- y + strbreak * strbreak_mag
#     ggplot(data.frame(x = 1:n_time, y = y, old_y = old_y, strbreak_y = strbreak_y), aes(x = x)) +
#       geom_line(aes(y = old_y), color = "blue", linetype = "dashed") +
#       geom_line(aes(y = y), color = "red") +
#       geom_line(aes(y = strbreak_y)) -> p
#     # plot(p)
#   }
#   # y <- if(id %in% treated_step){y + strbreak*strbreak_mag + fe} else {y}
#   # ggplot(data.frame(x = 1:n_time, y = y),aes(x = x, y = y)) + geom_line() -> p
#   # # plot(p)

#   input_data <- data.frame(id = id,
#                            time = (1:n_time) + base_time,
#                            x = x,
#                            y = y)
  
#   treatment_dat <- tibble() %>%
#   {if(id %in% treated_trend){bind_rows(., tibble(id, treated = "trend", time = 1))}else{.}} %>%
#   {if(id %in% treated_trendbreak){bind_rows(., tibble(id, treated = "trendbreak", time = ceiling(n_time*trendbreak_loc)))}else{.}} %>%
#   {if(id %in% treated_step){bind_rows(., tibble(id, treated = "step", time = ceiling(n_time*strbreak_loc)))}else{.}}

#   out <- list()
#   out$input_data <- input_data
#   out$treatment_dat <- treatment_dat

#   out
# }

run_single_model <- function(n_id, n_time, engine, method, treatment_params, fe_sigma, beta, sigma) {
  # browser()
  input_data <- data.frame()
  treatment_collection <- tibble()

  means <- rnorm(n_id, sd = fe_sigma)
  print(means)
  for (id in 1:n_id) {
    fe = means[id]
    data <- create_input_data(id = id,
                              fe = fe,
                              n_time = n_time,
                              beta = beta,
                              sigma = sigma)

    if (id %in% treatment_params$id) {
      params <- treatment_params %>% filter(id == !!id)
      for (i in seq_len(nrow(params))) {
        print(params[i,])
        treat <- impose_treatment(type = params$type[i],
                      n_time = n_time,
                      location = params$location[i],
                      magnitude = params$magnitude[i],
                      fe = fe)
        data$y <- data$y + treat$treatment
        treatment_collection <- bind_rows(treatment_collection,
                          tibble(id = id,
                            treated = params$type[i],
                            time = treat$time))
      }
    }
    input_data <- bind_rows(input_data, data)
    # treatment_collection <- bind_rows(treatment_collection, data$treatment_dat)
  }

  input_data %>%
    pivot_longer(-c(id, time)) %>%
    ggplot(aes(x = time, y = value, color = name)) +
    geom_line() +
    facet_wrap(~id) -> p
  plot(p)

  form <- as.formula(paste0("y ~ ", paste0(input_data %>%
                                            select(-c(id, time, y)) %>%
                                            names,
                                          collapse = " + ")))

  result <- isatpanel(input_data, formula = form,
                      effect = "individual",
                      index = c("id","time"),
                      fesis = if(method %in% c("fesis","both")){TRUE}else{FALSE},
                      tis = if(method %in% c("tis","both")){TRUE}else{FALSE},
                      iis = FALSE,
                      print.searchinfo = FALSE)

  summary <- tibble(
    n_id,
    n_time,
    getspanel_object = list(result),
    indicators = list(get_indicators(result)),
    treatment_collection = list(treatment_collection),
    engine,
    indic_method = method,
    adaptive = NA
  )
  summary
}

run_simulation_study <- function() {
  set.seed(99726)
  n_time <- 20
  n_id <- 3
  engine <- "gets"

  base_time <- 1900

  sigma <- 0.5
  fe_sigma <- 5

  #beta <- c(0.3)
  beta <- c(0.3, 0.7, -.3, 0, 0) # the betas for the coefficients

  # Define treatment parameters in a tidy data frame
  treatment_params <- tibble::tibble(
    id = c(3, 5, 2, 1),
    type = c("step", "step", "trend", "trendbreak"),
    magnitude = c(2, 2, 0.2, -0.4),
    location = c(0.2, 0.2, NA, 0.65)
  )

  overall <- tibble()
  for (method in c("fesis", "tis", "both")){
    for (n_time in c(20, 30, 50, 100)){
      for (n_id in c(2, 3, 5, 10)){
        tmp <- run_single_model(n_id = n_id, n_time = n_time, engine = engine, method = method, treatment_params = treatment_params, fe_sigma = fe_sigma, beta = beta, sigma = sigma)
        overall <- bind_rows(overall, tmp)
      }
    }
  }

  # overall <- overall %>% mutate(indic_method = "both")
  # overall <- overall %>% mutate(adaptive = NA)

  # treatment_rows <- overall %>%
  #   select(treatment_collection) %>%
  #   mutate(simulation_id = seq_len(n())) %>%
  #   unnest(treatment_collection) %>%
  #   mutate(treated = ifelse(treated == "trendbreak", "trend", treated)) %>%
  #   rename(type = treated) %>%
  #   rename(treated_time = time) %>%
  #   setcolorder(c("simulation_id", "id", "type", "treated_time"))

  # treatment_rows <- treatment_rows %>%
  #   mutate(treated_time = ifelse(is.na(treated_time), 1, treated_time))

  # indicator_rows <- overall %>%
  #   select(indicators) %>%
  #   mutate(simulation_id = seq_len(n())) %>%
  #   unnest(indicators) %>%
  #   unnest(indicators) %>%
  #   select(-y, -value) %>%
  #   rowwise() %>%
  #   mutate(
  #     time = time - base_time,
  #     treated = case_when(grepl("^fesis", name) ~ "step",
  #                         grepl("^tis", name) ~ "trend")
  #   ) %>%
  #   ungroup() %>%
  #   rename(type = treated) %>%
  #   rename(indicator_time = time) %>%
  #   rename(indicator_name = name) %>%
  #   setcolorder(c("simulation_id", "id", "type", "indicator_time", "indicator_name")) %>%
  #   mutate(id = as.integer(id)) # this was not working

  # match <- full_join(treatment_rows, indicator_rows) %>%
  #   arrange(simulation_id) %>%
  #   mutate(diff = indicator_time - treated_time,
  #          incor = ifelse(is.na(diff), TRUE, FALSE),
  #          missed = NA,
  #          too_many = NA) %>%
  #   group_by(simulation_id, id) %>%
  #   mutate(n_treatment = sum(!is.na(treated_time)),
  #          n_indicators = sum(!is.na(indicator_time)),
  #          missed = case_when(n_indicators == 0 & n_treatment > 0 ~ type)) %>%
  #   #too_many = case_when(indic_number > correct_number ~ TRUE, TRUE ~ FALSE)) %>% View
  #   ungroup

  # correct <- match %>%
  #   filter(!incor | !is.na(missed))

  #   intermediate_data %>%
  #     filter(incor & is.na(missed)) %>%
  #     group_by(simulation_id,id) %>%
  #     mutate(correct_misspec = case_when(is.na(time) ~ list(treated))) %>%
  #     mutate(time = case_when(is.na(time) ~ list(time))) %>%

  #     unnest(correct_misspec, keep_empty = TRUE) %>%
  #     filter(correct_misspec != treated) %>%

  #     unnest(time, keep_empty = TRUE) %>%
  #     filter(!is.na(time)) %>%

  #     mutate(diff = time_ind - time,
  #           order = order(abs(diff)),
  #           additional = order > 1) %>%
  #     ungroup() -> incorrect


  #   correct %>%
  #     bind_rows(incorrect) %>%
  #     arrange(simulation_id, id) -> final_data

  #   final_data %>%
  #     summarise(.by = c(simulation_id,n_time,n_id),
  #               true_misspecification_missed = sum(!is.na(missed))) %>%
  #     ggplot(aes(x = n_time, y = true_misspecification_missed, color = as.factor(n_id))) +
  #     geom_line() +
  #     facet_wrap(~n_id) +
  #     scale_y_continuous(breaks = as.integer)

  #   final_data %>%
  #     filter(!incor) %>%
  #     summarise(diff = sum(abs(diff)), .by = c(simulation_id,n_time,n_id)) %>%
  #     ggplot(aes(x = n_time, y = diff, color = as.factor(n_id))) +
  #     geom_line() +
  #     facet_wrap(~n_id)

  #   overall %>%
  #     #filter(engine == "lasso") %>%
  #     mutate(no_indicators = map(getspanel_object, function(x){
  #       if(is.null(x$isatpanel.result$ISnames)){
  #         x$finaldata %>%
  #           select(-starts_with("x."), -starts_with("id"),-time,-y) %>%
  #           names %>%
  #           length
  #       } else {
  #         length(x$isatpanel.result$ISnames)
  #       }
  #     })) %>%
  #     unnest(no_indicators) %>%

  #     #filter(n_time == 100, k == 10, engine == "lasso", adaptive)

  #     mutate(adaptive = case_when(is.na(adaptive) ~ "N/A (gets)",
  #                                 TRUE ~ as.character(adaptive))) %>%


  #     ggplot(aes(x = as.factor(n_time), y = no_indicators, color = engine, shape = adaptive)) +
  #     #geom_line() +
  #     geom_point() +

  #     facet_grid(indic_method~n_id) +
  #     labs(x = "N per unit", y = "Number of Indicators", title = "Simulation Performance",
  #         subtitle = "DGP contains one trend, one trendbreak, two steps") +
  #     theme_minimal(base_size = 12) +
  #     theme(panel.grid.minor = element_blank(),
  #           panel.background = element_rect(fill = NA)) -> plt
            
  return(overall)
}

# Treatment Detection Evaluation Functions
# =======================================

#' Exact Match Accuracy
#' Measures perfect matches where id, type, and timing are all correct
exact_matches <- function(true_treatments, detected_treatments) {
  true_key <- paste(true_treatments$id, true_treatments$type, true_treatments$timing, sep = "_")
  detected_key <- paste(detected_treatments$id, detected_treatments$type, detected_treatments$timing, sep = "_")
  
  matches <- intersect(true_key, detected_key)
  
  precision <- if(length(detected_key) > 0) length(matches) / length(detected_key) else 0
  recall <- if(length(true_key) > 0) length(matches) / length(true_key) else 0
  f1_score <- if(precision + recall > 0) 2 * (precision * recall) / (precision + recall) else 0
  
  list(
    n_matches = length(matches),
    precision = precision,
    recall = recall,
    f1_score = f1_score
  )
}

#' Timing Tolerant Matches
#' Allows for small timing errors (e.g., ±1 or ±2 periods)
timing_tolerant_matches <- function(true_treatments, detected_treatments, tolerance = 1) {
  matches <- 0
  matched_true <- c()
  matched_detected <- c()
  
  for(i in 1:nrow(detected_treatments)) {
    det <- detected_treatments[i, ]
    
    # Find potential matches with same id and type
    candidates <- true_treatments[
      true_treatments$id == det$id & 
      true_treatments$type == det$type &
      abs(true_treatments$timing - det$timing) <= tolerance, 
    ]
    
    if(nrow(candidates) > 0) {
      # Choose closest timing match
      best_match <- candidates[which.min(abs(candidates$timing - det$timing)), ]
      match_key <- paste(best_match$id, best_match$type, best_match$timing, sep = "_")
      
      if(!match_key %in% matched_true) {
        matches <- matches + 1
        matched_true <- c(matched_true, match_key)
        matched_detected <- c(matched_detected, i)
      }
    }
  }
  
  precision <- if(nrow(detected_treatments) > 0) matches / nrow(detected_treatments) else 0
  recall <- if(nrow(true_treatments) > 0) matches / nrow(true_treatments) else 0
  
  list(
    n_matches = matches,
    precision = precision,
    recall = recall
  )
}

#' Type Confusion Analysis
#' Analyzes cases where timing and id are correct but type is wrong
type_confusion_analysis <- function(true_treatments, detected_treatments) {
  # Create id-timing keys
  true_key <- paste(true_treatments$id, true_treatments$timing, sep = "_")
  detected_key <- paste(detected_treatments$id, detected_treatments$timing, sep = "_")
  
  # Find overlapping id-timing combinations
  common_keys <- intersect(true_key, detected_key)
  
  confusion_cases <- data.frame()
  
  for(key in common_keys) {
    true_subset <- true_treatments[paste(true_treatments$id, true_treatments$timing, sep = "_") == key, ]
    detected_subset <- detected_treatments[paste(detected_treatments$id, detected_treatments$timing, sep = "_") == key, ]
    
    # Check if types match
    if(!all(true_subset$type %in% detected_subset$type)) {
      confusion_cases <- rbind(confusion_cases, data.frame(
        id_timing = key,
        true_type = paste(true_subset$type, collapse = ","),
        detected_type = paste(detected_subset$type, collapse = ",")
      ))
    }
  }
  
  return(confusion_cases)
}

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
  
  # Extract true and detected treatments from the overall tibble
  true_treatments <- overall_tibble %>%
    select(treatment_collection) %>%
    mutate(simulation_id = seq_len(n())) %>%
    unnest(treatment_collection) %>%
    mutate(type = ifelse(treated == "trendbreak", "trend", treated)) %>%
    select(simulation_id, id, type, timing = time)
    # filter(!is.na(timing))

  # temporary
  true_treatments <- true_treatments %>%
    mutate(timing = ifelse(is.na(timing), 1, timing))
  
  detected_treatments <- overall_tibble %>%
    select(indicators) %>%
    mutate(simulation_id = seq_len(n())) %>%
    unnest(indicators) %>%
    unnest(indicators) %>%
    select(-y, -value) %>%
    rowwise() %>%
    mutate(
      timing = time - base_time,
      type = case_when(
        grepl("^fesis", name) ~ "step",
        grepl("^tis", name) ~ "trend"
      )
    ) %>%
    ungroup() %>%
    select(simulation_id, id, type, timing) %>%
    filter(!is.na(timing)) %>%
    mutate(id = as.integer(id))
  
  # Run evaluation for each simulation
  results <- list()
  
  for(sim_id in unique(true_treatments$simulation_id)) {
    true_sim <- true_treatments %>% filter(simulation_id == sim_id)
    detected_sim <- detected_treatments %>% filter(simulation_id == sim_id)
    
    sim_results <- list()
    sim_results$simulation_id <- sim_id
    
    # 1. Exact matches
    sim_results$exact <- exact_matches(true_sim, detected_sim)
    
    # 2. Timing tolerance analysis
    sim_results$timing_tolerance <- lapply(timing_tolerances, function(tol) {
      timing_tolerant_matches(true_sim, detected_sim, tol)
    })
    names(sim_results$timing_tolerance) <- paste0("tolerance_", timing_tolerances)
    
    # 3. Type confusion
    sim_results$type_confusion <- type_confusion_analysis(true_sim, detected_sim)
    
    # 4. False positives and negatives
    sim_results$false_analysis <- analyze_false_detections(true_sim, detected_sim)
    
    results[[sim_id]] <- sim_results
  }
  
  return(results)
}

#' Create Summary Table
#' Aggregates results across simulations
create_summary_table <- function(evaluation_results) {
  summary_data <- data.frame()
  
  for(i in seq_along(evaluation_results)) {
    result <- evaluation_results[[i]]
    
    row <- data.frame(
      simulation_id = result$simulation_id,
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