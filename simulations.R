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
multiple_breaks_metrics <- metrics_summary(multiple_breaks, tolerances = c(0, 1, 2), factors = c("indic_method", "t.pval", "tolerance", "n_true_per_method"))
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
  metrics = c("potency"),
  factors = c("n_id", "n_time", "indic_method", "t.pval", "max.block.size", "tolerance"),
  title = "Multiple breaks in DGP, varying block size",
  separate_metrics = TRUE
)

save(no_breaks_metrics, varying_magnitude_metrics, multiple_breaks_metrics, varying_block_size_metrics, file = "simulation_metrics.RData")
