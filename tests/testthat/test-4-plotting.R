
load("_snaps/3-snapshot_isatpanel/outcomes.RData")
# load("_snaps/3-snapshot_isatpanel/outcome2.RData")
# load("_snaps/3-snapshot_isatpanel/outcome3.RData")
# load("_snaps/3-snapshot_isatpanel/outcome4.RData")
# load("_snaps/3-snapshot_isatpanel/outcome5.RData")

# save_png <- function(code, width = 1000, height = 600) {
#   path <- tempfile(fileext = ".png")
#   grDevices::png(path, width = width, height = height)
#   on.exit(dev.off())
#   code
#
#   path
# }


save_png <- function(a) {
  path <- tempfile(fileext = ".png")
  ggplot2::ggsave(a,filename = path, width = 6, height = 4)
  path
}


test_that("Standard Plot",{
  skip_on_ci()
  skip_on_cran()
  expect_snapshot_file(save_png(plot(outcomes$outcome1)),name = "Standard_plot_outcome1.png")
  expect_snapshot_file(save_png(plot(outcomes$outcome2)),name = "Standard_plot_outcome2.png")
  expect_snapshot_file(save_png(plot(outcomes$outcome3)),name = "Standard_plot_outcome3.png")
  expect_snapshot_file(save_png(plot(outcomes$outcome4)),name = "Standard_plot_outcome4.png")
  expect_snapshot_file(save_png(plot(outcomes$outcome5)),name = "Standard_plot_outcome5.png")
})


test_that("Grid Plot",{
  skip_on_ci()
  skip_on_cran()
  expect_snapshot_file(save_png(plot_grid(outcomes$outcome1)),name = "Grid_plot_outcome1.png")
  expect_snapshot_file(save_png(plot_grid(outcomes$outcome2)),name = "Grid_plot_outcome2.png")
  expect_snapshot_file(save_png(plot_grid(outcomes$outcome3)),name = "Grid_plot_outcome3.png")
  expect_snapshot_file(save_png(plot_grid(outcomes$outcome4)),name = "Grid_plot_outcome4.png")
  expect_snapshot_file(save_png(plot_grid(outcomes$outcome5)),name = "Grid_plot_outcome5.png")
})

test_that("Residuals Plot",{
  skip_on_ci()
  skip_on_cran()
  expect_snapshot_file(save_png(plot_residuals(outcomes$outcome1)),name = "Residuals_plot_outcome1.png")
  expect_snapshot_file(save_png(plot_residuals(outcomes$outcome2)),name = "Residuals_plot_outcome2.png")
  expect_snapshot_file(save_png(plot_residuals(outcomes$outcome3)),name = "Residuals_plot_outcome3.png")
  expect_snapshot_file(save_png(plot_residuals(outcomes$outcome4)),name = "Residuals_plot_outcome4.png")
  expect_snapshot_file(save_png(plot_residuals(outcomes$outcome5)),name = "Residuals_plot_outcome5.png")
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
