
# save_png <- function(code, width = 1000, height = 600) {
#   path <- tempfile(fileext = ".png")
#   grDevices::png(path, width = width, height = height)
#   on.exit(dev.off())
#   code
#
#   path
# }




##############
#### I M P O R T A N T ###
##############
# This will only work by using the "Build" and then "Run Tests" Buttons. when doing this
# our snapshots will work. They will not be the same if we use
# the console and there use devtools::test() or devtools::test_active_file() then
# the snapshots will be a tiny bit different
# so we need to be consistent when snapshotting plots


###
# Generate Three Unit Example
###
set.seed(123)
# Generate some random data for the two control countries
xA <- rnorm(50, mean = 100)
xB <- rnorm(50, mean = 30)
xC <- rnorm(50, mean = 70)

epA <- rnorm(50, sd = 0.2)
epB <- rnorm(50, sd = 0.2)
epC <- rnorm(50, sd = 0.2)

trend <- 1951:2000
trendbreak <- c(rep(0,19),1:31) # impose a trendbreak from 1975

yA <- 10 + 0.5 * xA + 0.2 * trend - 0.3 * trendbreak + epA
yB <- 0.5 * xB + 0.1 * trend + epB
yC <- 0.5 * xC + 0.1 + epC

trial_df <- data.frame(year = rep(1951:2000,3),
                       id = c(rep("A",50),rep("B",50),rep("C",50)),
                       x = c(xA, xB, xC),
                       y = c(yA,yB,yC))


# Introduce a step shift in A from 40
trial_df_step <- trial_df
trial_df_step$y[40:50] <- trial_df_step$y[40:50]*1.02

# Do the same with a date object
trial_df_date <- trial_df_step
trial_df_date$year <- rep(seq.Date(from = as.Date("2000-01-01"), length.out = 50, by = "month"),3)

outcome1 <- isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), print.searchinfo = FALSE, tis = TRUE) # TIS approximates step shift
outcome2 <- isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), print.searchinfo = FALSE, fesis = TRUE) # Step Shift approximates trend (esp in B)
outcome3 <- isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), print.searchinfo = FALSE, fesis = TRUE, tis = TRUE) # correct specification

outcome4 <- isatpanel(trial_df, formula = y ~ x, index = c("id","year"), print.searchinfo = FALSE, cfesis_time = 1960:1990, cfesis = TRUE)
outcome5 <- isatpanel(trial_df_date, formula = y ~ x, index = c("id","year"), print.searchinfo = FALSE, cfesis = TRUE,
                      cfesis_time = list(A = seq.Date(from = as.Date("2000-01-01"), length.out = 10, by = "month"),
                                         B = seq.Date(from = as.Date("2003-01-01"), length.out = 12, by = "month"),
                                         C = seq.Date(from = as.Date("2000-01-01"), length.out = 50, by = "month")))


# To use expect_snapshot_file() you'll typically need to start by writing
# a helper function that creates a file from your code, returning a path
save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")

  if(ggplot2::is_ggplot(code)){
    ggplot2::ggsave(filename = path, plot = code, width = 7, height = 5)
  } else {
    png(path, width = width, height = height)
    on.exit(dev.off())
    code
  }
  path
}

# You'd then also provide a helper that skips tests where you can't
# be sure of producing exactly the same output
expect_snapshot_plot <- function(name, code) {
  # Other packages might affect results
  skip_if_not_installed("ggplot2", "2.0.0")
  # Or maybe the output is different on some operation systems
  #skip_on_os("windows")
  skip_on_ci()
  # You'll need to carefully think about and experiment with these skips

  name <- paste0(name, ".png")
  # Announce the file before touching `code`. This way, if `code`
  # unexpectedly fails or skips, testthat will not auto-delete the
  # corresponding snapshot file.
  announce_snapshot_file(name = name)

  path <- save_png(code)
  expect_snapshot_file(path, name)
}


test_that("Standard Plot",{
  skip_on_ci()
  skip_on_cran()
  expect_snapshot_plot("Standard_plot_outcome1", code = plot(outcome1))
  expect_snapshot_plot("Standard_plot_outcome2", code = plot(outcome2))
  expect_snapshot_plot("Standard_plot_outcome3", code = plot(outcome3))
  expect_snapshot_plot("Standard_plot_outcome4", code = plot(outcome4))
  expect_snapshot_plot("Standard_plot_outcome5", code = plot(outcome5))
})


test_that("Grid Plot",{
  skip_on_ci()
  skip_on_cran()
  expect_snapshot_plot("Grid_plot_outcome1", code = plot_grid(outcome1))
  expect_snapshot_plot("Grid_plot_outcome2", code = plot_grid(outcome2))
  expect_snapshot_plot("Grid_plot_outcome3", code = plot_grid(outcome3))
  expect_snapshot_plot("Grid_plot_outcome4", code = plot_grid(outcome4))
  expect_snapshot_plot("Grid_plot_outcome5", code = plot_grid(outcome5))

  expect_snapshot_plot("Grid_plot_outcome4_regex", code = plot_grid(outcome4, regex_exclude_indicators = "cfesisC"))


})


test_that("Counterfactual Plot",{
  skip_on_ci()
  skip_on_cran()
  # only carry out on FESIS objects
  expect_snapshot_plot("counterfactual_plot_outcome2", code = plot_counterfactual(outcome2))
  expect_snapshot_plot("counterfactual_plot_outcome3", code = plot_counterfactual(outcome3))

  expect_snapshot_plot("counterfactual_plot_outcome2_regex", code = plot_grid(outcome2, regex_exclude_indicators = "fesisA"))
  expect_snapshot_plot("counterfactual_plot_outcome3_regex", code = plot_grid(outcome3, , regex_exclude_indicators = "fesisA"))
})



test_that("Residuals Plot",{
  skip_on_ci()
  skip_on_cran()
  expect_snapshot_plot("Residuals_plot_outcome1", code = plot_residuals(outcome1))
  expect_snapshot_plot("Residuals_plot_outcome2", code = plot_residuals(outcome2))
  expect_snapshot_plot("Residuals_plot_outcome3", code = plot_residuals(outcome3))
})


# test functionality of plotting functions (like regex_exclude_indicators, main_title, etc.)

test_that("plotting functions work with regex_exclude_indicators", {

  skip_on_ci()
  skip_on_cran()

  expect_silent(plot_grid(outcome4, regex_exclude_indicators = "cfesisC"))
  expect_silent(plot_grid(outcome4, regex_exclude_indicators = NULL))

  expect_silent(plot_counterfactual(outcome2, regex_exclude_indicators = "cfesisC"))
  expect_silent(plot_counterfactual(outcome3, regex_exclude_indicators = NULL))

  expect_snapshot_plot("plot_grid_outcome4_regex", code = plot_grid(outcome4, regex_exclude_indicators = "cfesisC"))
  expect_snapshot_plot("plot_counterfactual_outcome2_regex", code = plot_counterfactual(outcome2, regex_exclude_indicators = "fesisA"))
})

test_that("plotting cfesis", {

  skip_on_ci()
  skip_on_cran()

  # Prepare Data
  set.seed(1230)
  data("EU_emissions_road")
  data <- EU_emissions_road
  data$lgdp_sq <- data$lgdp^2

  data$transport.emissions_pc <- data$transport.emissions/data$pop
  data$ltransport.emissions_pc <- log(data$transport.emissions_pc)

  data$L1.ltransport.emissions_pc <- NA
  # For each country, shift the values of 'ltransport.emissions_pc' by one position
  for (i in unique(data$country)) {
    # Extract the 'ltransport.emissions_pc' values for the current country
    current_country_values <- data$ltransport.emissions_pc[data$country == i]

    # Shift the values by one position and insert an NA value at the beginning
    shifted_values <- c(NA, current_country_values[-length(current_country_values)])

    # Assign the shifted values to the corresponding rows in 'L1.ltransport.emissions_pc'
    data$L1.ltransport.emissions_pc[data$country == i] <- shifted_values
  }

  # Group specification
  EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
            "France", "United Kingdom", "Ireland", "Italy", "Luxembourg",
            "Netherlands", "Greece", "Portugal", "Sweden")

  # Prepare sample and data
  sample <- EU15
  dat <- data[data$country %in% sample & data$year >= 1995, ]

  # Run
  result <- isatpanel(
    data = dat,
    formula = ltransport.emissions_pc ~ lgdp + lgdp_sq + lpop,
    index = c("country", "year"),
    effect = "twoways",
    iis = TRUE,
    fesis = TRUE,
    tis = TRUE,
    csis = TRUE,
    cfesis = TRUE,
    t.pval = .05,
    print.searchinfo = FALSE,
    plot = FALSE,
  )

  expect_snapshot_plot("plot_grid_cfesis_1", code = plot_grid(result))
  expect_snapshot_plot("plot_grid_cfesis_2", code = plot_grid(result, regex_exclude_indicators = "fesisFinland.2000|Austria|Greece"))
  expect_snapshot_plot("plot_grid_cfesis_3", code = plot_grid(result, regex_exclude_indicators = "fesisFinland.2000|Austria|Greece|Luxembourg|lgdp.csis|lgdp_sq|iis1"))

})

test_that("ggplot checks without snapshotting the plot", {

  p_grid <- plot_grid(outcome4, regex_exclude_indicators = "cfesisC")

  # First check that you really have a plot
  expect_s3_class(p_grid, "gg")

  # Retrieve the underlying list
  class(p_grid) <- "list"

  # Remove the "environment" element which is not predictible
  p_grid$plot_env <- NULL

  expect_true(all(is.na(p_grid$data[p_grid$data$id == "C", "effect"])))

  # check the stability of the underlying list
  expect_snapshot(p_grid)


  p_counter <- plot_counterfactual(outcome2, regex_exclude_indicators = "cfesisC")

  # First check that you really have a plot
  expect_s3_class(p_counter, "gg")

  # Retrieve the underlying list
  class(p_counter) <- "list"

  # Remove the "environment" element which is not predictible
  p_counter$plot_env <- NULL

  expect_true(all(is.na(p_counter$data[p_counter$data$id == "C", "effect"])))

  # check the stability of the underlying list
  expect_snapshot(p_counter)

})

