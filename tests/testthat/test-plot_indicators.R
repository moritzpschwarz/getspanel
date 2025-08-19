test_that("Test that input parameters work", {
  data <- readRDS(test_path("_fixtures", "isatpanel_with_all_indicators.rds"))

  # Test with different id_list
  p <- plot_indicators(data, id_list = c("Austria", "Germany"))
  expect_true(all(p$data$id %in% c("Austria", "Germany")))

  # Test with sign filter
  p <- plot_indicators(data, sign = "pos")
  expect_true(all(p$data$coef >= 0, na.rm = TRUE))

  # Test that regex_exclude_indicators works
  p <- plot_indicators(data, regex_exclude_indicators = "iis")
  expect_true(all(!grepl("^iis", p$data$name)))
})

test_that("Standard indicator plot", {
  data <- readRDS(test_path("_fixtures", "isatpanel_with_all_indicators.rds"))
  expect_snapshot_plot("Standard_indicators_plot", code = plot_indicators(data))
})