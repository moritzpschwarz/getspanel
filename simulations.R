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
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, n_cores = 9, print.searchinfo = FALSE, plot = FALSE
)
# Save results
saveRDS(no_breaks, file = "no_breaks.rds")

# Analyze results
no_breaks_metrics <- metrics_summary(no_breaks)
# Visualize results
plot_metrics(
  no_breaks_metrics$per_simulation,
  metrics = c("gauge", "n_detected"),
  title = "No breaks in DGP (Baseline)",
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
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, n_cores = 9, print.searchinfo = FALSE, plot = FALSE
)

treatment_params_list <- list(
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.5, "trendbreak", 0.3, 1,
  )
)
methods <- c("tis")

r2 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, n_cores = 9, print.searchinfo = FALSE, plot = FALSE
)
r2 <- r2 %>%
  dplyr::mutate(sim_id = sim_id + max(r1$sim_id))

treatment_params_list <- list(
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.3, "step", 0.3, 1,
    0.6, "trendbreak", 0.3, 1
  )
)
methods <- c("both")

r3 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, n_cores = 9, print.searchinfo = FALSE, plot = FALSE
)
r3 <- r3 %>%
  dplyr::mutate(sim_id = sim_id + max(r2$sim_id))

one_break_per_method <- rbind(r1, r2, r3)
# Save results
saveRDS(one_break_per_method, file = "one_break_per_method.rds")

# Analyze results
one_break_per_method_metrics <- metrics_summary(one_break_per_method, tolerances = c(0, 1, 2))
# Visualize results
plot_metrics(
  one_break_per_method_metrics$per_simulation,
  metrics = c("gauge", "n_detected"),
  title = "One break per method in DGP (step; trend; step+trend)",
  separate_metrics = TRUE
)

# One break per type with varying magnitude ------------------------------------
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
    0.5, "step", 0.3, 3
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.5, "step", 0.3, 2
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.5, "step", 0.3, 1
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.5, "step", 0.3, 0.5
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.5, "step", 0.3, 0.25
  )
)
# Benchmark parameters (getspanel parameters to be varied)
n_rep <- 5
methods <- c("fesis")
t.pvals <- c(0.05, 0.01, 0.001)
max.block.sizes <- c(30)

r1 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, n_cores = 9, print.searchinfo = FALSE, plot = FALSE
)

treatment_params_list <- list(
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.5, "trendbreak", 0.3, 3
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.5, "trendbreak", 0.3, 2
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.5, "trendbreak", 0.3, 1
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.5, "trendbreak", 0.3, 0.5
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.5, "trendbreak", 0.3, 0.25
  )
)
methods <- c("tis")

r2 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, n_cores = 9, print.searchinfo = FALSE, plot = FALSE
)
r2 <- r2 %>%
  dplyr::mutate(sim_id = sim_id + max(r1$sim_id))

treatment_params_list <- list(
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.3, "step", 0.3, 3,
    0.6, "trendbreak", 0.3, 3
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.3, "step", 0.3, 2,
    0.6, "trendbreak", 0.3, 2
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.3, "step", 0.3, 1,
    0.6, "trendbreak", 0.3, 1
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.3, "step", 0.3, 0.5,
    0.6, "trendbreak", 0.3, 0.5
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.3, "step", 0.3, 0.25,
    0.6, "trendbreak", 0.3, 0.25
  )
)
methods <- c("both")

r3 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, n_cores = 9, print.searchinfo = FALSE, plot = FALSE
)
r3 <- r3 %>%
  dplyr::mutate(sim_id = sim_id + max(r2$sim_id))

varying_magnitude <- rbind(r1, r2, r3)
# Save results
saveRDS(varying_magnitude, file = "varying_magnitude.rds")

# Analyze results
varying_magnitude_metrics <- metrics_summary(varying_magnitude, tolerances = c(0, 1, 2), factors = c("indic_method", "t.pval", "tolerance", "magnitude"))
# Visualize results
plot_metrics(
  varying_magnitude_metrics$per_simulation,
  metrics = c("gauge", "n_detected"),
  title = "One break per type with varying magnitude in DGP",
  separate_metrics = TRUE
)

# Multiple breaks per type with equal magnitude --------------------------------
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
    0.5, "step", 0.3, 1
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.3, "step", 0.3, 1,
    0.6, "step", 0.3, 1
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.2, "step", 0.3, 1,
    0.5, "step", 0.3, 1,
    0.8, "step", 0.3, 1
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.2, "step", 0.3, 1,
    0.5, "step", 0.3, 1,
    0.8, "step", 0.3, 1,
    0.2, "step", 0.6, 1,
    0.5, "step", 0.6, 1,
    0.8, "step", 0.6, 1
  )
)
# Benchmark parameters (getspanel parameters to be varied)
n_rep <- 5
methods <- c("fesis")
t.pvals <- c(0.05, 0.01, 0.001)
max.block.sizes <- c(30)

r1 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, n_cores = 9, print.searchinfo = FALSE, plot = FALSE
)

# Treatment parameters (imposed treatments to be detected)
treatment_params_list <- list(
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.5, "trendbreak", 0.3, 1
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.3, "trendbreak", 0.3, 1,
    0.6, "trendbreak", 0.3, 1
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.2, "trendbreak", 0.3, 1,
    0.5, "trendbreak", 0.3, 1,
    0.8, "trendbreak", 0.3, 1
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.2, "trendbreak", 0.3, 1,
    0.5, "trendbreak", 0.3, 1,
    0.8, "trendbreak", 0.3, 1,
    0.2, "trendbreak", 0.6, 1,
    0.5, "trendbreak", 0.6, 1,
    0.8, "trendbreak", 0.6, 1
  )
)
methods <- c("tis")

r2 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, n_cores = 9, print.searchinfo = FALSE, plot = FALSE
)
r2 <- r2 %>%
  dplyr::mutate(sim_id = sim_id + max(r1$sim_id))

treatment_params_list <- list(
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.3, "step", 0.3, 1,
    0.6, "trendbreak", 0.3, 1
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.2, "step", 0.3, 1,
    0.5, "step", 0.6, 1,
    0.3, "trendbreak", 0.3, 1,
    0.6, "trendbreak", 0.6, 1
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.2, "step", 0.3, 1,
    0.5, "step", 0.3, 1,
    0.8, "step", 0.3, 1,
    0.3, "trendbreak", 0.6, 1,
    0.6, "trendbreak", 0.6, 1,
    0.9, "trendbreak", 0.6, 1
  ),
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.2, "step", 0.2, 1,
    0.5, "step", 0.2, 1,
    0.8, "step", 0.2, 1,
    0.3, "trendbreak", 0.4, 1,
    0.6, "trendbreak", 0.4, 1,
    0.9, "trendbreak", 0.4, 1,
    0.2, "step", 0.6, 1,
    0.5, "step", 0.6, 1,
    0.8, "step", 0.6, 1,
    0.3, "trendbreak", 0.8, 1,
    0.6, "trendbreak", 0.8, 1,
    0.9, "trendbreak", 0.8, 1
  )
)
methods <- c("both")

r3 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, n_cores = 9, print.searchinfo = FALSE, plot = FALSE
)
r3 <- r3 %>%
  dplyr::mutate(sim_id = sim_id + max(r2$sim_id))

multiple_breaks <- rbind(r1, r2, r3)
# Save results
saveRDS(multiple_breaks, file = "multiple_breaks.rds")

# Analyze results
multiple_breaks_metrics <- metrics_summary(multiple_breaks, tolerances = c(0, 1, 2), factors = c("indic_method", "t.pval", "tolerance", "n_true"))
# Visualize results
plot_metrics(
  multiple_breaks_metrics$per_simulation,
  metrics = c("gauge", "n_detected"),
  title = "Multiple breaks with equal magnitude in DGP",
  separate_metrics = TRUE
)
plot_metrics(
  multiple_breaks_metrics$per_simulation,
  metrics = c("potency", "f1"),
  title = "Multiple breaks with equal magnitude in DGP",
  separate_metrics = TRUE
)

# Varying block size ---------------------------------------------------------
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
    0.2, "step", 0.2, 2,
    0.2, "step", 0.6, -0.5,
    0.5, "step", 0.4, 1,
    0.7, "step", 0.5, 3,
    1.0, "step", 0.4, -1,
  )
)
# Benchmark parameters (getspanel parameters to be varied)
n_rep <- 5
methods <- c("fesis")
t.pvals <- c(0.05, 0.01, 0.001)
ars <- c(0)
max.block.sizes <- c(5, 10, 20, 30)

# Run the simulation study
r1 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, n_cores = 9, print.searchinfo = FALSE, plot = FALSE
)

# Treatment parameters (imposed treatments to be detected)
treatment_params_list <- list(
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.3, "trendbreak", 0.3, 1,
    0.3, "trendbreak", 0.65, -0.4,
    0.5, "trendbreak", 0.8, -2,
    0.9, "trendbreak", 0.7, 2,
    1.0, "trendbreak", 0.8, -1
  )
)
methods <- c("tis")

r2 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, n_cores = 9, print.searchinfo = FALSE, plot = FALSE
)
r2 <- r2 %>%
  dplyr::mutate(sim_id = sim_id + max(r1$sim_id))

# Treatment parameters (imposed treatments to be detected)
treatment_params_list <- list(
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.2, "step", 0.2, 2,
    0.2, "step", 0.6, -0.5,
    0.3, "trendbreak", 0.3, 1,
    0.3, "trendbreak", 0.65, -0.4,
    0.5, "step", 0.4, 1,
    0.5, "trendbreak", 0.8, -2,
    0.7, "step", 0.5, 3,
    0.9, "trendbreak", 0.7, 2,
    1.0, "step", 0.4, -1,
    1.0, "trendbreak", 0.8, -1
  )
)
methods <- c("both")

r3 <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, max.block.sizes, n_cores = 9, print.searchinfo = FALSE, plot = FALSE
)
r3 <- r3 %>%
  dplyr::mutate(sim_id = sim_id + max(r2$sim_id))

varying_block_size <- rbind(r1, r2, r3)
# Save results
saveRDS(varying_block_size, file = "varying_block_size.rds")

# Analyze results
varying_block_size_metrics <- metrics_summary(varying_block_size, tolerances = c(0, 1, 2))
# Visualize results
plot_metrics(
  varying_block_size_metrics$per_simulation,
  metrics = c("gauge", "n_detected"),
  title = "Multiple breaks in DGP, varying block size",
  separate_metrics = TRUE
)
plot_metrics(
  varying_block_size_metrics$per_simulation,
  metrics = c("potency", "f1"),
  title = "Multiple breaks in DGP, varying block size",
  separate_metrics = TRUE
)

save(no_breaks_metrics, one_break_per_method_metrics, varying_magnitude_metrics, multiple_breaks_metrics, varying_block_size_metrics, file = "simulation_metrics.RData")

combined <- dplyr::bind_rows(
  no_breaks_metrics$by_factor %>%
    dplyr::mutate(scenario = "no_breaks"),
  one_break_per_method_metrics$by_factor %>%
    dplyr::mutate(scenario = "one_break_per_method"),
  varying_magnitude_metrics$by_factor %>%
    dplyr::mutate(scenario = "varying_magnitude"),
  multiple_breaks_metrics$by_factor %>%
    dplyr::mutate(scenario = "multiple_breaks"),
  varying_block_size_metrics$by_factor %>%
    dplyr::mutate(scenario = "varying_block_size")
)
combined <- combined %>%
  mutate(
    t.pval = as.factor(t.pval),
    magnitude = as.factor(magnitude),
    max.block.size = as.factor(max.block.size),
    n_true = as.factor(n_true)
  ) %>%
  pivot_longer(
    cols = c("magnitude", "max.block.size", "n_true"),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(value != "NA")

# Create baseline data for horizontal lines
baseline_data <- no_breaks_metrics$by_factor %>%
  dplyr::filter(tolerance == 0) %>%
  dplyr::select(indic_method, t.pval, avg_gauge, avg_potency) %>%
  dplyr::distinct() %>%
  dplyr::mutate(t.pval = as.factor(t.pval))

# Expand baseline data for all variables to match facet structure
baseline_expanded <- baseline_data %>%
  tidyr::crossing(variable = c("magnitude", "max.block.size", "n_true"))

ggplot(combined %>% filter(tolerance == 0), aes(x = value, y = avg_gauge, color = indic_method, group = indic_method)) +
  # Add horizontal baseline lines
  # geom_hline(
  #   data = baseline_expanded,
  #   aes(yintercept = avg_gauge, color = indic_method),
  #   linetype = "dashed",
  #   alpha = 0.7,
  #   linewidth = 0.5
  # ) +
  geom_point(na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  facet_wrap(
    c("variable", "t.pval"),
    scales = "free",
    labeller = as_labeller(
      c(
        magnitude = "Magnitude",
        max.block.size = "Block Size",
        n_true = "Number of Breaks",
        `0.05` = "t.pval = 0.05",
        `0.01` = "t.pval = 0.01",
        `0.001` = "t.pval = 0.001"
      ),
      multi_line = FALSE
    ),
    strip.position = "bottom",
  ) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  labs(
    title = "Average Gauge across Simulation Scenarios",
    # subtitle = "Dashed lines show baseline gauge from no-breaks scenario",
    y = "Average Gauge",
    x = ""
  )
