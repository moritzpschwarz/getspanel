
load("_snaps/3-snapshot_isatpanel/outcome1.RData")
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


save_png <- function(a) {
  path <- tempfile(fileext = ".png")
  ggplot2::ggsave(a,filename = path, width = 6, height = 4)
  path
}


test_that("Standard Plot",{
  expect_snapshot_file(save_png(plot(outcome1)),name = "Standard_plot_outcome1.png")
  expect_snapshot_file(save_png(plot(outcome2)),name = "Standard_plot_outcome2.png")
  expect_snapshot_file(save_png(plot(outcome3)),name = "Standard_plot_outcome3.png")
  expect_snapshot_file(save_png(plot(outcome4)),name = "Standard_plot_outcome4.png")
  expect_snapshot_file(save_png(plot(outcome5)),name = "Standard_plot_outcome5.png")
})


test_that("Grid Plot",{
  expect_snapshot_file(save_png(plot_grid(outcome1)),name = "Grid_plot_outcome1.png")
  expect_snapshot_file(save_png(plot_grid(outcome2)),name = "Grid_plot_outcome2.png")
  expect_snapshot_file(save_png(plot_grid(outcome3)),name = "Grid_plot_outcome3.png")
  expect_snapshot_file(save_png(plot_grid(outcome4)),name = "Grid_plot_outcome4.png")
  expect_snapshot_file(save_png(plot_grid(outcome5)),name = "Grid_plot_outcome5.png")
})

test_that("Residuals Plot",{
  expect_snapshot_file(save_png(plot_residuals(outcome1)),name = "Residuals_plot_outcome1.png")
  expect_snapshot_file(save_png(plot_residuals(outcome2)),name = "Residuals_plot_outcome2.png")
  expect_snapshot_file(save_png(plot_residuals(outcome3)),name = "Residuals_plot_outcome3.png")
  expect_snapshot_file(save_png(plot_residuals(outcome4)),name = "Residuals_plot_outcome4.png")
  expect_snapshot_file(save_png(plot_residuals(outcome5)),name = "Residuals_plot_outcome5.png")
})

#
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
