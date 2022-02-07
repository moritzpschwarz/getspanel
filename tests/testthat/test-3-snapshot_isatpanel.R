


test_that("Creating five isatpanel objects for snapshot tests", {

  data <- pandata_simulated
  #data <- data[data$year>1979,]

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

  save_file <- function(outcomes){
    path <- tempfile(fileext = ".RData")
    save(outcomes, file = path)
    #outcomes
    path
  }


  outcome1$isatpanel.result$time.started <- NULL
  outcome2$isatpanel.result$time.started <- NULL
  outcome3$isatpanel.result$time.started <- NULL
  outcome4$isatpanel.result$time.started <- NULL
  outcome5$isatpanel.result$time.started <- NULL

  outcome1$isatpanel.result$time.finished <- NULL
  outcome2$isatpanel.result$time.finished <- NULL
  outcome3$isatpanel.result$time.finished <- NULL
  outcome4$isatpanel.result$time.finished <- NULL
  outcome5$isatpanel.result$time.finished <- NULL

  outcomes <- list()
  outcomes$outcome1 <- outcome1
  outcomes$outcome2 <- outcome2
  outcomes$outcome3 <- outcome3
  outcomes$outcome4 <- outcome4
  outcomes$outcome5 <- outcome5

  expect_snapshot_file(save_file(outcomes), name = "outcomes.RData")

  # expect_snapshot_file(save_file(outcome2), name = "outcome2.RData")
  # expect_snapshot_file(save_file(outcome3), name = "outcome3.RData")
  # expect_snapshot_file(save_file(outcome4), name = "outcome4.RData")
  # expect_snapshot_file(save_file(outcome5), name = "outcome5.RData")

})

