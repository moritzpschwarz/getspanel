# devtools::install_github(repo = "moritzpschwarz/getspanel")
#library(getspanel)
#library(tidyverse) # needed for the plots
library(fixest)
library(lfe)

data("pandata_simulated")

pandata_simulated <- pandata_simulated[pandata_simulated$year > 1980,]


# Case 1: No FE

test_that("Case 1: No FE", {
  #summary(lm(gdp~temp,pandata_simulated))
  #fixest::feols(gdp~temp, pandata_simulated)

  expect_silent(isatpanel(data = pandata_simulated,
                          formula = gdp ~ temp,
                          index = c("country","year"),
                          effect = "none",
                          csis = TRUE,
                          print.searchinfo = FALSE))

  expect_silent(isatpanel(data = pandata_simulated,
                          formula = gdp ~ temp,
                          index = c("country","year"),
                          effect = "none",
                          engine = "fixest",
                          csis = TRUE,
                          print.searchinfo = FALSE))

  expect_silent(isatpanel(data = pandata_simulated,
                          formula = gdp ~ temp,
                          index = c("country","year"),
                          effect = "none",
                          engine = "felm",
                          csis = TRUE,
                          print.searchinfo = FALSE))
})


# Case 2: Indv FE

test_that("Case 2: Indv FE", {

  # lm(gdp~temp + as.factor(country),data = pandata_simulated) %>% summary
  # lm(gdp~temp+ as.factor(country)-1,pandata_simulated) %>% summary
  #
  # fixest::feols(gdp~temp|country, pandata_simulated)
  # fixest::feols(gdp~temp-1|country, pandata_simulated)


  expect_silent(isatpanel(data = pandata_simulated,
                          formula = gdp ~ temp,
                          index = c("country","year"),
                          effect = "individual",
                          csis = TRUE,
                          print.searchinfo = FALSE))

  expect_silent(isatpanel(data = pandata_simulated,
                          formula = gdp ~ temp,
                          index = c("country","year"),
                          effect = "individual",
                          engine = "fixest",
                          csis = TRUE,
                          print.searchinfo = FALSE))

  expect_silent(isatpanel(data = pandata_simulated,
                          formula = gdp ~ temp,
                          index = c("country","year"),
                          effect = "individual",
                          engine = "felm",
                          csis = TRUE,
                          print.searchinfo = FALSE))


})


# Case 3: Time FE

test_that("Case 3: Time FE", {
  # lm(gdp~temp + as.factor(year),pandata_simulated) %>% summary
  # lm(gdp~temp+ as.factor(year)-1,pandata_simulated) %>% summary
  #
  # feols(gdp~temp|year, pandata_simulated)
  # feols(gdp~temp-1|year, pandata_simulated)

  expect_silent(isatpanel(data = pandata_simulated,
                          formula = gdp ~ temp,
                          index = c("country","year"),
                          effect = "time",
                          csis = TRUE,
                          print.searchinfo = FALSE))

  expect_silent(isatpanel(data = pandata_simulated,
                          formula = gdp ~ temp,
                          index = c("country","year"),
                          effect = "time",
                          engine = "fixest",
                          csis = TRUE,
                          print.searchinfo = FALSE))

  expect_silent(isatpanel(data = pandata_simulated,
                          formula = gdp ~ temp,
                          index = c("country","year"),
                          effect = "time",
                          engine = "felm",
                          csis = TRUE,
                          print.searchinfo = FALSE))

})



# Case 4: Two-way FE

test_that("Case 4: Two-way FE", {
  # lm(gdp~temp + as.factor(year) + as.factor(country),pandata_simulated) %>% summary
  # lm(gdp~temp+ as.factor(year) + as.factor(country)-1,pandata_simulated) %>% summary
  #
  # feols(gdp~temp|year + country, pandata_simulated)
  # feols(gdp~temp-1|year + country, pandata_simulated)

  expect_silent(isatpanel(data = pandata_simulated,
                          formula = gdp ~ temp,
                          index = c("country","year"),
                          effect = "twoways",
                          csis = TRUE,
                          print.searchinfo = FALSE))

  expect_silent(isatpanel(data = pandata_simulated,
                          formula = gdp ~ temp,
                          index = c("country","year"),
                          effect = "twoways",
                          engine = "fixest",
                          csis = TRUE,
                          print.searchinfo = FALSE))

  expect_silent(isatpanel(data = pandata_simulated,
                          formula = gdp ~ temp,
                          index = c("country","year"),
                          effect = "twoways",
                          engine = "felm",
                          csis = TRUE,
                          print.searchinfo = FALSE))

})






