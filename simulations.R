devtools::load_all()

# No breaks in DGP -------------------------------------------------------------
set.seed(99726)
# Simulation parameters (panel structure and data generation)
n_ids <- c(5, 10, 20)
n_times <- c(20, 30, 50)
beta <- c(0.3, 0.7, -.3, 0, 0) # the betas for the coefficients
sigma <- 0.5
fe_sigma <- 5
# Treatment parameters (imposed treatments to be detected)
treatment_params_list <- list(
  tribble(
    ~id, ~type, ~location, ~magnitude,
  )
)
# Benchmark parameters (getspanel parameters to be varied)
n_rep <- 5
methods <- c("fesis", "tis", "both")
t.pvals <- c(0.05, 0.01, 0.001)
max.block.sizes <- c(30)

no_breaks <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, print.searchinfo = FALSE, plot = FALSE
)
# Save results
saveRDS(no_breaks, file = "no_breaks.rds")

# Analyze results
no_breaks_analysis <- metrics_summary(no_breaks, tolerances = c(0))
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
n_ids <- c(5, 10, 20)
n_times <- c(20, 30, 50)
beta <- c(0.3, 0.7, -.3, 0, 0) # the betas for the coefficients
sigma <- 0.5
fe_sigma <- 5
# Treatment parameters (imposed treatments to be detected)
treatment_params_list <- list(
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.5, "step", 0.3, 1,
  )
)
# Benchmark parameters (getspanel parameters to be varied)
n_rep <- 5
methods <- c("fesis")
t.pvals <- c(0.05, 0.01, 0.001)
max.block.sizes <- c(30)

# Run the simulation study
r1 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, print.searchinfo = FALSE, plot = FALSE
)

treatment_params_list <- list(
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.5, "trendbreak", 0.3, 1,
  )
)
methods <- c("tis")

r2 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, print.searchinfo = FALSE, plot = FALSE
)
r2 <- r2 %>%
  dplyr::mutate(simulation_id = simulation_id + max(r1$simulation_id))

treatment_params_list <- list(
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.3 "step", 0.3, 1,
    0.6, "trendbreak", 0.3, 1
  )
)
methods <- c("both")

r3 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list,
  n_rep, engines, methods, t.pvals, ars, max.block.sizes
)
r3 <- r3 %>%
  dplyr::mutate(simulation_id = simulation_id + max(r2$simulation_id))

one_break_per_method <- rbind(r1, r2, r3)
# Save results
saveRDS(one_break_per_method, file = "one_break_per_method.rds")

# Analyze results
one_break_per_method_analysis <- metrics_summary(one_break_per_method, tolerances = c(0, 1, 2))

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
# TODO: should be evaluated with allow_type_mismatch -> not much difference
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

# One break per type with varying magnitude ------------------------------------
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
    2, "step", 3, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "step", 2, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "step", 1, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "step", 0.5, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "step", 0.25, 0.2,
  )
)

# Benchmark parameters (getspanel parameters to be varied)
n_rep <- 3
engines <- c("gets")
methods <- c("fesis")
t.pvals <- c(0.05, 0.01, 0.001)
ars <- c(0)
max.block.sizes <- c(30)

# Run the simulation study
r1 <- run_simulation_study_parallel(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list,
  n_rep, engines, methods, t.pvals, ars, max.block.sizes
)

treatment_params_list <- list(
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "trendbreak", 3, 0.2,
    3, "step", 5, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "trendbreak", 2, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "trendbreak", 1, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "trendbreak", 0.5, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "trendbreak", 0.25, 0.2,
  )
)

methods <- c("tis")

r2 <- run_simulation_study_parallel(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list,
  n_rep, engines, methods, t.pvals, ars, max.block.sizes
)

r2 <- r2 %>%
  dplyr::mutate(simulation_id = simulation_id + max(r1$simulation_id))

treatment_params_list <- list(
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "trendbreak", 3, 0.2,
    3, "step", 3, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "trendbreak", 2, 0.2,
    3, "step", 2, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "trendbreak", 1, 0.2,
    3, "step", 1, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "trendbreak", 0.5, 0.2,
    3, "step", 0.5, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "trendbreak", 0.25, 0.2,
    3, "step", 0.25, 0.2,
  )
)

methods <- c("both")

r3 <- run_simulation_study_parallel(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list,
  n_rep, engines, methods, t.pvals, ars, max.block.sizes
)

r3 <- r3 %>%
  dplyr::mutate(simulation_id = simulation_id + max(r2$simulation_id))

varying_magnitude <- rbind(r1, r2, r3)

# Save results
saveRDS(varying_magnitude, file = "varying_magnitude.rds")

# Analyze results
varying_magnitude_analysis <- metrics_summary(varying_magnitude, tolerances = c(0, 1, 2))
# print(varying_magnitude_analysis)

# Visualize results
plot_metrics(
  varying_magnitude_analysis$per_simulation,
  metrics = c("gauge", "detected"),
  plot_type = "boxplot",
  factors <- c("n_id", "n_time", "indic_method", "t.pval", "magnitude", "tolerance"),
  title = "Gauge/Detected Breaks by factor; One break with varying magnitude in DGP",
  separate_metrics = TRUE
)

# Multiple breaks per type with equal magnitude --------------------------------
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
    1, "step", 2, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    1, "step", 2, 0.2,
    2, "step", 2, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    1, "step", 2, 0.2,
    2, "step", 2, 0.2,
    3, "step", 2, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    1, "step", 2, 0.2,
    2, "step", 2, 0.2,
    3, "step", 2, 0.2,
    1, "step", -1, 0.6,
    2, "step", -1, 0.6,
    3, "step", -1, 0.6,
  )
)

# Benchmark parameters (getspanel parameters to be varied)
n_rep <- 3
engines <- c("gets")
methods <- c("fesis")
t.pvals <- c(0.05, 0.01, 0.001)
ars <- c(0)
max.block.sizes <- c(30)

# Run the simulation study
r1 <- run_simulation_study_parallel(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list,
  n_rep, engines, methods, t.pvals, ars, max.block.sizes
)

treatment_params_list <- list(
  tribble(
    ~id, ~type, ~magnitude, ~location,
    1, "trendbreak", 2, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    1, "trendbreak", 2, 0.2,
    2, "trendbreak", 2, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    1, "trendbreak", 2, 0.2,
    2, "trendbreak", 2, 0.2,
    3, "trendbreak", 2, 0.2,
  ),
  tribble(
    ~id, ~type, ~magnitude, ~location,
    1, "trendbreak", 2, 0.2,
    2, "trendbreak", 2, 0.2,
    3, "trendbreak", 2, 0.2,
    1, "trendbreak", -1, 0.6,
    2, "trendbreak", -1, 0.6,
    3, "trendbreak", -1, 0.6,
  )
)

methods <- c("tis")

r2 <- run_simulation_study_parallel(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list,
  n_rep, engines, methods, t.pvals, ars, max.block.sizes
)
r2 <- r2 %>%
  dplyr::mutate(simulation_id = simulation_id + max(r1$simulation_id))

multiple_breaks <- rbind(r1, r2)

# Save results
saveRDS(multiple_breaks, file = "multiple_breaks.rds")

# Analyze results
multiple_breaks_analysis <- metrics_summary(multiple_breaks, tolerances = c(0, 1, 2),factors = c("indic_method", "t.pval", "tolerance", "true"), include_magnitude = FALSE)
# print(no_breaks)

# Visualize results
plot_metrics(
  multiple_breaks_analysis$per_simulation,
  metrics = c("gauge", "detected"),
  plot_type = "boxplot",
  factors <- c("n_id", "n_time", "indic_method", "t.pval", "true"),
  title = "Gauge/Detected Breaks by factor; No breaks in DGP",
  separate_metrics = TRUE
)

# Varying block size ---------------------------------------------------------
set.seed(99726)
# Simulation parameters (panel structure and data generation)
n_ids <- c(10, 20)
n_times <- c(20, 30, 50)
beta <- c(0.3, 0.7, -.3, 0, 0) # the betas for the coefficients
sigma <- 0.5
fe_sigma <- 5

# Treatment parameters (imposed treatments to be detected)
treatment_params_list <- list(
  tribble(
    ~id, ~type, ~magnitude, ~location,
    2, "step", 2, 0.2,
    2, "step", -0.5, 0.6,
    3, "trendbreak", 1, 0.3,
    3, "trendbreak", -0.4, 0.65,
    5, "step", 1, 0.4,
    5, "trendbreak", -2, 0.8,
    7, "step", 3, 0.5,
    9, "trendbreak", 2, 0.7,
    10, "step", -1, 0.4
  )
)

# Benchmark parameters (getspanel parameters to be varied)
n_rep <- 3
engines <- c("gets")
methods <- c("both")
t.pvals <- c(0.05, 0.01, 0.001)
ars <- c(0)
max.block.sizes <- c(5, 10, 20, 30)

# Run the simulation study
varying_block_size <- run_simulation_study_parallel(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list,
  n_rep, engines, methods, t.pvals, ars, max.block.sizes
)

# Save results
saveRDS(varying_block_size, file = "varying_block_size.rds")

# Analyze results
varying_block_size_analysis <- metrics_summary(varying_block_size, tolerances = c(0, 1, 2))
# print(varying_block_size_analysis)

# Visualize results
plot_metrics(
  varying_block_size_analysis$per_simulation,
  metrics = c("potency", "gauge", "detected"),
  plot_type = "boxplot",
  factors <- c("n_id", "n_time", "indic_method", "t.pval", "max.block.size", "tolerance"),
  title = "Gauge/Detected Breaks by factor; Varying block size in DGP",
  separate_metrics = TRUE
)


ggplot(no_breaks_analysis$by_method %>% mutate(t.pval = as.factor(t.pval)), aes(x = indic_method, y = avg_gauge, color = t.pval, group = t.pval)) +
  geom_point() +
  labs(
    title = "Average Gauge by t.pval, n_id, n_time; No breaks in DGP",
    y = "Average Gauge"
  ) +
  theme_minimal()


combined <- dplyr::bind_rows(
  varying_magnitude_analysis$by_method %>%
    dplyr::mutate(scenario = "varying_magnitude"),
  varying_block_size_analysis$by_method %>%
    dplyr::mutate(scenario = "varying_block_size"),
  multiple_breaks_analysis$by_method %>%
    dplyr::mutate(scenario = "multiple_breaks"),
  no_breaks_analysis$by_method %>%
    dplyr::mutate(scenario = "no_breaks")
)
combined <- combined %>%
  mutate(
    t.pval = as.factor(t.pval),
    magnitude = as.factor(magnitude),
    max.block.size = as.factor(max.block.size),
    true = as.factor(true)
  ) %>%
  pivot_longer(
    cols = c("magnitude", "max.block.size", "true"),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(value != "NA")

# Create baseline data for horizontal lines
baseline_data <- no_breaks_analysis$by_method %>%
  dplyr::filter(tolerance == 0) %>%
  dplyr::select(indic_method, t.pval, avg_gauge, avg_potency) %>%
  dplyr::distinct() %>%
  dplyr::mutate(t.pval = as.factor(t.pval))

# Expand baseline data for all variables to match facet structure
baseline_expanded <- baseline_data %>%
  tidyr::crossing(variable = c("magnitude", "max.block.size", "true"))

ggplot(combined %>% filter(tolerance == 0), aes(x = value, y = avg_potency, color = indic_method, group = indic_method)) +
  # Add horizontal baseline lines
  geom_hline(
    data = baseline_expanded,
    aes(yintercept = avg_potency, color = indic_method),
    linetype = "dashed",
    alpha = 0.7,
    linewidth = 0.5
  ) +
  geom_point(na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  facet_wrap(
    c("variable", "t.pval"),
    scales = "free_x",
    labeller = as_labeller(
      c(
        magnitude = "Varying Magnitude",
        max.block.size = "Varying Block Size",
        true = "Varying Number of Breaks",
        `0.05` = "t.pval = 0.05",
        `0.01` = "t.pval = 0.01",
        `0.001` = "t.pval = 0.001"
      )
    )
  ) +
  labs(
    title = "Average Potency by Magnitude, t.pval; One break with varying magnitude in DGP",
    subtitle = "Dashed lines show baseline gauge from no-breaks scenario",
    y = "Average Potency",
    x = ""
  ) +
  theme_minimal()
