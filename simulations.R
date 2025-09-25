devtools::load_all()

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
results <- run_simulation_study(
  n_ids = n_ids,
  n_times = n_times,
  beta = beta,
  sigma = sigma,
  fe_sigma = fe_sigma,
  treatment_params_list = treatment_params_list,
  n_rep = n_rep,
  engines = engines,
  methods = methods,
  t.pvals = t.pvals,
  ars = ars,
  max.block.sizes = max.block.sizes
)

# Save results
saveRDS(results, file = "no_breaks_fesis_tis_both.rds")

# Analyze results
analysis <- metrics_summary(results, tolerances = c(0))
print(analysis)

# Visualize results
plot_metrics(
  analysis$per_simulation,
  metrics = c("gauge"),
  plot_type = "boxplot",
  factors <- c("n_id", "n_time", "indic_method", "t.pval")
)


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
    3, "step", 2, 0.2,
    2, "trendbreak", -0.4, 0.65
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
results2 <- run_simulation_study(
  n_ids = n_ids,
  n_times = n_times,
  beta = beta,
  sigma = sigma,
  fe_sigma = fe_sigma,
  treatment_params_list = treatment_params_list,
  n_rep = n_rep,
  engines = engines,
  methods = methods,
  t.pvals = t.pvals,
  ars = ars,
  max.block.sizes = max.block.sizes
)

# Save results
saveRDS(results2, file = "multiple_breaks_fesis_tis_both.rds")

# Analyze results
analysis2 <- metrics_summary(results2, tolerances = c(0))
print(analysis2)

# Visualize results
plot_metrics(
  analysis2$per_simulation,
  metrics = c("gauge"),
  plot_type = "boxplot",
  factors <- c("n_id", "n_time", "indic_method", "t.pval")
)
