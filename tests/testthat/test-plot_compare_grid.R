test_that("Test that input parameters work", {
  data(compare_models_example, package = "getspanel")

  # Test with different id_list
  p <- plot_compare_grid(compare_models_example, id_list = c("Austria", "Germany"))
  expect_true(all(p$data$id %in% c("Austria", "Germany")))

  # Test with different mod_list
  p <- plot_compare_grid(compare_models_example, mod_list = compare_models_example$model[[1]])
  expect_true(all(p$data$model == compare_models_example$model[[1]]))

  # Test with sign filter
  # can't directly test for coefficients, but pos. coefs lead to pos. effects
  p <- plot_compare_grid(compare_models_example, sign = "pos")
  expect_true(all(p$data$effect >= 0, na.rm = TRUE))

  # Test with panel grouping
  p1 <- plot_compare_grid(compare_models_example, panel = "model")
  p2 <- plot_compare_grid(compare_models_example)
  expect_equal(p1$data$model, p2$data$id)
  expect_equal(p1$data$id, p2$data$model)
  expect_equal(p1$data$effect, p2$data$effect)

  # Test with renamed columns
  tmp <- compare_models_example
  names(tmp)[names(tmp) == "is"] <- "col1"
  names(tmp)[names(tmp) == "model"] <- "col2"
  p1 <- plot_compare_grid(tmp, is_col = "col1", model_col = "col2")
  p2 <- plot_compare_grid(compare_models_example)
  expect_true(all.equal(p1$data, p2$data))
})

test_that("Testing whether include_blanks works", {
  data(compare_models_example, package = "getspanel")

  p <- plot_compare_grid(compare_models_example, include_blanks = FALSE)
  ids <- unique(p$data$id)
  models <- unique(p$data$model)

  # Check that all id/model combinations have some data (no blank rows)
  for (id in ids) {
    for (model in models) {
      subset_data <- p$data[p$data$id == id & p$data$model == model, ]
      expect_true(all(!is.na(subset_data$effect)))
    }
  }
})

test_that("Test that regex_exclude_indicators works", {
  data(compare_models_example, package = "getspanel")

  # The plot data doesn't include the indicators names anymore,
  # so we can't directly test for excluded indicators in the plot data
  # Instead, we can use snapshot testing to ensure consistency
  p <- plot_compare_grid(compare_models_example, regex_exclude_indicators = "iis")

  # First check that you really have a plot
  expect_s3_class(p, "gg")
  # Retrieve the underlying list
  class(p) <- "list"
  # Remove the "environment" element which is not predictible
  p$plot_env <- NULL
  expect_snapshot(p)

  p <- plot_compare_grid(compare_models_example, regex_exclude_indicators = "fesis")
  # First check that you really have a plot
  expect_s3_class(p, "gg")
  # Retrieve the underlying list
  class(p) <- "list"
  # Remove the "environment" element which is not predictible
  p$plot_env <- NULL
  expect_snapshot(p)
})

test_that("Testing whether plot_compare_grid produces the same data as plot_grid", {
  data(compare_models_example, package = "getspanel")

  p_grid <- plot_grid(compare_models_example$is[[1]])
  data_grid <- p_grid$data[, c("id", "time", "effect")]
  # 'id is a factor in the grid plot, but character in the compare plot
  # convert to character for simplicity
  data_grid$id <- as.character(data_grid$id)

  p_compare <- plot_compare_grid(compare_models_example, mod_list = compare_models_example$model[[1]], panel = "model")
  data_compare <- p_compare$data[, c("model", "time", "effect")]
  names(data_compare)[1] <- "id"

  expect_true(all.equal(data_grid, data_compare))
})

test_that("Standard compare plot", {
  data(compare_models_example, package = "getspanel")
  expect_snapshot_plot("Standard_compare_plot", code = plot_compare_grid(compare_models_example))
})