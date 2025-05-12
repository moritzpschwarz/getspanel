
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


# save(outcome1, file = "outcome1.RData")
# save(outcome2, file = "outcome2.RData")
# save(outcome3, file = "outcome3.RData")
# save(outcome4, file = "outcome4.RData")
# save(outcome5, file = "outcome5.RData")
#
# plot(outcome1) + ggsave("plot_1.pdf", width = 6, height = 4)
# plot(outcome2) + ggsave("plot_2.pdf", width = 6, height = 4)
# plot(outcome3) + ggsave("plot_3.pdf", width = 6, height = 4)
# plot(outcome4) + ggsave("plot_4.pdf", width = 6, height = 4)
#
