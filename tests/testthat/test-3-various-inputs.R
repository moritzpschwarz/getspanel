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

test_that("Testing whether a single unit results in the appropriate error message", {

  single_unit_df <- pandata_simulated[pandata_simulated$country == 1,]

  expect_error(a <- isatpanel(data = single_unit_df,formula = gdp~temp, index = c("country","year"),fesis = TRUE, print.searchinfo = FALSE),
               regexp = "Only one unique value in the id variable. This approach needs panel data i.e. more than one unit. Please check the data.")
})


rm(pandata_simulated)
data("pandata_simulated")
# write a few tests that check whether fesis, csis, cfesis work with date indicators
# this means creating a test_that environment
# then try to plot them
test_that("Testing whether Date inputs work for fesis, csis, cfesis", {

  date_df <- pandata_simulated
  date_df$date <- rep(seq.Date(from = as.Date("2000-01-01"), length.out = nrow(pandata_simulated)/4, by = "quarter"),4)

  expect_silent(a <- isatpanel(data = date_df,formula = gdp~temp, index = c("country","date"),fesis = TRUE, print.searchinfo = FALSE))
  # check that the time of get_indicators(a) is 2024-04-01, 2008-07-01, and 2009-01-01
  expect_equal(get_indicators(a)$fesis$time, structure(c(19814, 14061, 14245), class = "Date"))

  expect_silent(a <- isatpanel(data = date_df,formula = gdp~temp, index = c("country","date"),csis = TRUE, print.searchinfo = FALSE, t.pval = 0.01))
  expect_equal(get_indicators(a)$csis$time, structure(c(16071, 16709), class = "Date"))

  expect_silent(a <- isatpanel(data = date_df,formula = gdp~temp, index = c("country","date"),cfesis = TRUE, print.searchinfo = FALSE))
  expect_equal(get_indicators(a)$cfesis, structure(list(id = c("1", "2"),
                                                        time = structure(c(14061, 14061), class = "Date"),
                                                        name = c("temp.cfesis1.2008-07-01", "temp.cfesis2.2008-07-01"),
                                                        type = c("CFESIS", "CFESIS"),
                                                        variable = c("temp", "temp"),
                                                        coef = c(2.04766983, 1.83899355),
                                                        sd = c(0.104133999884309, 0.0965675977853382)),
                                                   row.names = as.integer(1:2),
                                                   class = "data.frame"))

  expect_silent(plot(a))
})

