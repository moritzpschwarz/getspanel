##########################
######### load pandata_simulated
###########################
data("pandata_simulated")

pandata_simulated <- pandata_simulated[pandata_simulated$year>1979,]


test_that("Testing whether Date inputs work", {
  date_df <- pandata_simulated
  date_df$date <- rep(seq.Date(from = as.Date("1980-01-01"), to = as.Date("2000-01-01"), by = "year"),4)

  expect_message(a <- isatpanel(data = date_df,formula = gdp~temp, index = c("country","date"),fesis = TRUE))


})


test_that("Testing whether character inputs work for groups", {
  group_df <- pandata_simulated
  group_df$country <- rep(c("A","B","C","D"),each = 21)


  expect_message(a <- isatpanel(data = date_df,formula = gdp~temp, index = c("country","date"),fesis = TRUE))


})
