##########################
######### load pandata_simulated
###########################
data("pandata_simulated")

pandata_simulated <- pandata_simulated[pandata_simulated$year>1979,]


test_that("Testing whether Date inputs work", {

  date_df <- pandata_simulated
  date_df$date <- rep(seq.Date(from = as.Date("2000-01-01"), to = as.Date("2005-01-01"), by = "quarter"),4)

  expect_silent(a <- isatpanel(data = date_df,formula = gdp~temp, index = c("country","date"),fesis = TRUE, print.searchinfo = FALSE))


})


test_that("Testing whether character inputs work for groups", {
  group_df <- pandata_simulated
  group_df$country <- rep(c("A","B","C","D"),each = 21)

  # wrong index
  expect_error(a <- isatpanel(data = group_df,formula = gdp~temp, index = c("country","date"),fesis = TRUE, print.searchinfo = FALSE),
               regexp = "The values for 'index' not found as column names in the 'data' argument. Can only name columns that exist.")

  expect_silent(a <- isatpanel(data = group_df,formula = gdp~temp, index = c("country","year"),fesis = TRUE, print.searchinfo = FALSE))
})
