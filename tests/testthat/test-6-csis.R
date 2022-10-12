
test_that("Test that csis works",{

  data <- pandata_simulated

  expect_message(is1 <- isatpanel(data = data, gdp ~ temp + I(temp^2), index = c("country","year"),
                                  effect="twoways",iis=TRUE,fesis=TRUE, csis = TRUE,t.pval=0.01,engine = "felm"))

  expect_message(is2 <- isatpanel(data = data, gdp ~ temp + I(temp^2), index = c("country","year"),
                                  effect="twoways",iis=TRUE,fesis=TRUE, csis = TRUE,t.pval=0.01,engine = "fixest"))

  expect_message(is3 <- isatpanel(data = data, gdp ~ temp + I(temp^2), index = c("country","year"),
                                  effect="twoways",iis=TRUE,fesis=TRUE, csis = TRUE,t.pval=0.01,engine = NULL))


})
