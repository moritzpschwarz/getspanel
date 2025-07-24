test_that("Testing whether regex_exclude_indicators works", {
  data <- readRDS(test_path("_fixtures", "isatpanel_with_all_indicators.rds"))
  res <- get_indicators(data, format = "list", regex_exclude_indicators = "^fesis")
  expect_null(res$fesis)
  res <- get_indicators(data, format = "table", regex_exclude_indicators = "^fesis")
  expect_equal(nrow(res[res$type == "FESIS", ]), 0)
  res <- get_indicators(data, format = "table", regex_exclude_indicators = "csis|iis|Austria")
  expect_equal(nrow(res[res$id == "Austria", ]), 0)
})

test_that("Testing whether list and table contain the same information", {
  data <- readRDS(test_path("_fixtures", "isatpanel_with_all_indicators.rds"))
  # Have to exclude csis since they are not supposed to be the same
  res_list <- get_indicators(data, format = "list", regex_exclude_indicators = "csis")
  res_table <- get_indicators(data, format = "table", regex_exclude_indicators = "csis")

  res_list <- do.call(rbind, res_list)

  expect_equal(nrow(res_list), nrow(res_table))
  expect_equal(colnames(res_list), colnames(res_table))

  expect_true(all(res_list$id %in% res_table$id))
  expect_true(all(res_list$name %in% res_table$name))
})

test_that("Testing whether problematic names are handled correctly", {
  data1 <- readRDS(test_path("_fixtures", "isatpanel_with_problematic_names.rds"))
  data2 <- readRDS(test_path("_fixtures", "isatpanel_with_all_indicators.rds"))
  res1 <- get_indicators(data1, format = "table")
  res2 <- get_indicators(data2, format = "table")
  res1 <- res1[order(res1$coef), ]
  res2 <- res2[order(res2$coef), ]
  expect_equal(nrow(res1), nrow(res2))
  expect_equal(res1$coef, res2$coef)
})
