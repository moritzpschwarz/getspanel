devtools::load_all()

# No breaks in DGP -------------------------------------------------------------
set.seed(99726)
# Simulation parameters (panel structure and data generation)
n_ids <- c(3, 5, 10)
n_times <- c(20, 30, 50)
beta <- c(0.3, 0.7, -.3, 0, 0) # the betas for the coefficients
sigma <- 0.5
fe_sigma <- 5

# Treatment parameters (imposed treatments to be detected)
treatment_params_list <- list(
  tribble(
    ~id, ~type, ~magnitude, ~location,
  )
)

# Benchmark parameters (getspanel parameters to be varied)
n_rep <- 3
engines <- c("gets")
methods <- c("fesis", "tis", "both")
t.pvals <- c(0.05, 0.01, 0.001)
ars <- c(0)
max.block.sizes <- c(30)

# Run the simulation study
no_breaks <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list,
  n_rep, engines, methods, t.pvals, ars, max.block.sizes
)

# Save results
saveRDS(no_breaks, file = "no_breaks_fesis_tis_both.rds")

# Analyze results
no_breaks_analysis <- metrics_summary(no_breaks, tolerances = c(0))
# print(no_breaks)

# Visualize results
plot_metrics(
  no_breaks_analysis$per_simulation,
  metrics = c("gauge", "detected"),
  plot_type = "boxplot",
  factors <- c("n_id", "n_time", "indic_method", "t.pval"),
  title = "Gauge/Detected Breaks by factor; No breaks in DGP",
  separate_metrics = TRUE
)

# One break per method (step; trend; step+trend) -------------------------------
set.seed(99726)
# Simulation parameters (panel structure and data generation)
n_ids <- c(3, 5, 10)
n_times <- c(20, 30, 50)
beta <- c(0.3, 0.7, -.3, 0, 0) # the betas for the coefficients
sigma <- 0.5
fe_sigma <- 5

# Benchmark parameters (getspanel parameters to be varied)
n_rep <- 3
engines <- c("gets")
methods <- c("fesis")
t.pvals <- c(0.05, 0.01, 0.001)
ars <- c(0)
max.block.sizes <- c(30)

# Treatment parameters (imposed treatments to be detected)
treatment_params_list <- list(
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "step", 2, 0.2,
  )
)

# Run the simulation study
r1 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list,
  n_rep, engines, methods, t.pvals, ars, max.block.sizes
)

methods <- c("tis")
treatment_params_list <- list(
  tribble(
    ~id, ~type, ~magnitude, ~location,
    3, "trendbreak", -0.4, 0.65
  )
)

r2 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list,
  n_rep, engines, methods, t.pvals, ars, max.block.sizes
)
r2 <- r2 %>%
  dplyr::mutate(simulation_id = simulation_id + max(r1$simulation_id))

methods <- c("both")
treatment_params_list <- list(
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "step", 2, 0.2,
    3, "trendbreak", -0.4, 0.65
  )
)

r3 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list,
  n_rep, engines, methods, t.pvals, ars, max.block.sizes
)
r3 <- r3 %>%
  dplyr::mutate(simulation_id = simulation_id + max(r2$simulation_id))

one_break_per_method <- rbind(r1, r2, r3)

# Save results
saveRDS(one_break_per_method, file = "one_break_per_method_fesis_tis_both.rds")

# Analyze results
one_break_per_method_analysis <- metrics_summary(one_break_per_method, tolerances = c(0, 1, 2))
# print(one_break_per_method_analysis)

# Visualize results
plot_metrics(
  one_break_per_method_analysis$per_simulation,
  metrics = c("gauge", "detected"),
  plot_type = "boxplot",
  factors <- c("n_id", "n_time", "indic_method", "t.pval"),
  title = "Gauge/Detected Breaks by factor; One break per method in DGP",
  separate_metrics = TRUE
)

# Multiple breaks (step+trend for all 3 methods) -------------------------------
set.seed(99726)
# Simulation parameters (panel structure and data generation)
n_ids <- c(3, 5, 10)
n_times <- c(20, 30, 50)
beta <- c(0.3, 0.7, -.3, 0, 0) # the betas for the coefficients
sigma <- 0.5
fe_sigma <- 5

# Treatment parameters (imposed treatments to be detected)
treatment_params_list <- list(
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "step", 2, 0.2,
    3, "trendbreak", -0.4, 0.65
  )
)

# Benchmark parameters (getspanel parameters to be varied)
n_rep <- 3
engines <- c("gets")
methods <- c("fesis", "tis", "both")
t.pvals <- c(0.05, 0.01, 0.001)
ars <- c(0)
max.block.sizes <- c(30)

# Run the simulation study
two_breaks <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list,
  n_rep, engines, methods, t.pvals, ars, max.block.sizes
)

# Save results
saveRDS(two_breaks, file = "two_breaks_fesis_tis_both.rds")

# Analyze results
# TODO: should be evaluated with allow_type_mismatch
two_breaks_analysis <- metrics_summary(two_breaks, tolerances = c(0, 1, 2))
two_breaks_analysis_mismatch <- metrics_summary(two_breaks, tolerances = c(0, 1, 2), allow_type_mismatch = TRUE)
# print(two_breaks_analysis)

# Visualize results
plot_metrics(
  two_breaks_analysis$per_simulation,
  metrics = c("gauge", "detected"),
  plot_type = "boxplot",
  factors <- c("n_id", "n_time", "indic_method", "t.pval"),
  title = "Gauge/Detected Breaks by factor; Two Breaks (step+trend) in DGP",
  separate_metrics = TRUE
)

plot_compare_two_studies(
  one_break_per_method_analysis,
  two_breaks_analysis,
  "One break per method",
  "Two breaks (step+trend)",
  plot_type = "line",
  metrics = c("avg_gauge", "avg_potency", "avg_f1", "avg_detected"),
  title = "One break per method vs Two breaks (step+trend) in DGP",
)

studies <- list(
  "No breaks" = no_breaks_analysis,
  "One break per method" = one_break_per_method_analysis,
  "Two breaks" = two_breaks_analysis
  # "Two breaks (with mismatch)" = two_breaks_analysis_mismatch
)

plot_compare_metrics(
  studies,
  plot_type = "line",
  metrics = c("avg_gauge", "avg_potency", "avg_f1", "avg_detected"),
)
