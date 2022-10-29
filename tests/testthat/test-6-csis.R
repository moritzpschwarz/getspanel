
test_that("Test that csis works",{

  skip_on_cran()

  data <- pandata_simulated[pandata_simulated$year > 1979, ]

  expect_silent(is1 <- isatpanel(data = data, gdp ~ temp + I(temp^2), index = c("country","year"),
                                  effect="twoways",iis=TRUE,fesis=TRUE,
                                  csis = TRUE, t.pval=0.01, engine = "felm", print.searchinfo = FALSE))

  expect_silent(is2 <- isatpanel(data = data, gdp ~ temp + I(temp^2), index = c("country","year"),
                                  effect="twoways",iis=TRUE,fesis=TRUE,
                                  csis = TRUE,t.pval=0.01,engine = "fixest", print.searchinfo = FALSE))

  expect_silent(is3 <- isatpanel(data = data, gdp ~ temp + I(temp^2), index = c("country","year"),
                                  effect="twoways",iis=TRUE,fesis=TRUE,
                                  csis = TRUE,t.pval=0.01,engine = NULL, print.searchinfo = FALSE))


})
