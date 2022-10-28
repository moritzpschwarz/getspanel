
load("_snaps/3-snapshot_isatpanel/outcomes.RData")
load("_snaps/3-snapshot_isatpanel/outcome2.RData")
load("_snaps/3-snapshot_isatpanel/outcome3.RData")
load("_snaps/3-snapshot_isatpanel/outcome4.RData")
load("_snaps/3-snapshot_isatpanel/outcome5.RData")

# save_png <- function(code, width = 1000, height = 600) {
#   path <- tempfile(fileext = ".png")
#   grDevices::png(path, width = width, height = height)
#   on.exit(dev.off())
#   code
#
#   path
# }


# You'd then also provide a helper that skips tests where you can't
# be sure of producing exactly the same output
expect_snapshot_plot <- function(name, code) {
  # Other packages might affect results
  skip_if_not_installed("ggplot2", "2.0.0")
  # Or maybe the output is different on some operation systems
  skip_on_ci()
  # You'll need to carefully think about and experiment with these skips

  name <- paste0(name, ".png")

  # Announce the file before touching `code`. This way, if `code`
  # unexpectedly fails or skips, testthat will not auto-delete the
  # corresponding snapshot file.
  announce_snapshot_file(name = name)

  # To use expect_snapshot_file() you'll typically need to start by writing
  # a helper function that creates a file from your code, returning a path
  save_png <- function(code, width = 400, height = 400) {
    path <- tempfile(fileext = ".png")
    png(path, width = width, height = height)
    on.exit(dev.off())
    code

    path
  }

  path <- save_png(code)
  expect_snapshot_file(path, name)
}


test_that("Standard Plot",{
  skip_on_ci()
  skip_on_cran()
  expect_snapshot_plot("Standard_plot_outcome1.png", plot(outcomes$outcome1))
  expect_snapshot_plot("Standard_plot_outcome2.png", plot(outcomes$outcome2))
  expect_snapshot_plot("Standard_plot_outcome3.png", plot(outcomes$outcome3))
  expect_snapshot_plot("Standard_plot_outcome4.png", plot(outcomes$outcome4))
  expect_snapshot_plot("Standard_plot_outcome5.png", plot(outcomes$outcome5))
})


test_that("Grid Plot",{
  skip_on_ci()
  skip_on_cran()
  expect_snapshot_plot("Grid_plot_outcome1.png", plot_grid(outcomes$outcome1))
  expect_snapshot_plot("Grid_plot_outcome2.png", plot_grid(outcomes$outcome2))
  expect_snapshot_plot("Grid_plot_outcome3.png", plot_grid(outcomes$outcome3))
  expect_snapshot_plot("Grid_plot_outcome4.png", plot_grid(outcomes$outcome4))
  expect_snapshot_plot("Grid_plot_outcome5.png", plot_grid(outcomes$outcome5))
})

test_that("Residuals Plot",{
  skip_on_ci()
  skip_on_cran()
  expect_snapshot_plot("Residuals_plot_outcome1.png", plot_residuals(outcomes$outcome1))
  expect_snapshot_plot("Residuals_plot_outcome2.png", plot_residuals(outcomes$outcome2))
  expect_snapshot_plot("Residuals_plot_outcome3.png", plot_residuals(outcomes$outcome3))
  expect_snapshot_plot("Residuals_plot_outcome4.png", plot_residuals(outcomes$outcome4))
  expect_snapshot_plot("Residuals_plot_outcome5.png", plot_residuals(outcomes$outcome5))
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
