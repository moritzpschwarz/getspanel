

save_file <- function(a){
  path <- tempfile(fileext = ".RData")
  save(a, file = path)
  path
}

test_that( "Creating five isatpanel objects for snapshot tests" , {

  data <- pandata_simulated
  # data <- data[data$year>1979,]

  outcome1 <- isatpanel(data = data, gdp ~ temp, index = c("country","year"),effect="twoways",iis=TRUE,
                        fesis=TRUE, csis = TRUE , t.pval=0.01, engine = "fixest")

  outcome2 <- isatpanel(data = data, gdp ~ temp, index = c("country","year"),effect="twoways",
                        iis=FALSE, fesis=FALSE, csis = TRUE, sis = FALSE, t.pval=0.01,engine = "fixest")

  outcome3 <- isatpanel(data = data, gdp ~ temp, index = c("country","year"),effect="twoways",
                        iis=FALSE, cfesis=TRUE, t.pval=0.01,engine = "fixest")


  data$temp_2 <- data$temp^2
  outcome4 <- isatpanel(data = data, gdp ~ temp + temp_2, index = c("country","year"),effect="twoways",
                        iis=FALSE, csis=TRUE, t.pval=0.1,engine = "fixest")

  outcome5 <- isatpanel(data = data, gdp ~ temp + temp_2, index = c("country","year"),effect="twoways",
                        iis=FALSE, csis=TRUE, cfesis = TRUE ,t.pval=0.05)



  expect_snapshot_file(save_file(outcome1), name = "outcome1.RData")
  expect_snapshot_file(save_file(outcome2), name = "outcome2.RData")
  expect_snapshot_file(save_file(outcome3), name = "outcome3.RData")
  expect_snapshot_file(save_file(outcome4), name = "outcome4.RData")
  expect_snapshot_file(save_file(outcome5), name = "outcome5.RData")

})

