###
# Generate Three Unit Example
###
set.seed(123)
# Generate some random data for the two control countries
xA <- rnorm(50, mean = 100)
xB <- rnorm(50, mean = 30)
xC <- rnorm(50, mean = 70)

epA <- rnorm(50, sd = 0.2)
epB <- rnorm(50, sd = 0.2)
epC <- rnorm(50, sd = 0.2)

trend <- 1951:2000
trendbreak <- c(rep(0,19),1:31) # impose a trendbreak from 1975

yA <- 10 + 0.5 * xA + 0.2 * trend - 0.3 * trendbreak + epA
yB <- 0.5 * xB + 0.1 * trend + epB
yC <- 0.5 * xC + 0.1 + epC

trial_df <- data.frame(year = rep(1951:2000,3),
                       id = c(rep("A",50),rep("B",50),rep("C",50)),
                       x = c(xA, xB, xC),
                       y = c(yA,yB,yC))


# Introduce a step shift in A from 40
trial_df_step <- trial_df
trial_df_step$y[40:50] <- trial_df_step$y[40:50]*1.02




tis_test1 <- c(1960:1990)
tis_test2 <- list(A = 1951:1960, B = 1985:1990, C = 1951:2000)
tis_test3 <- list(A = 1951:2000, B = NULL, C = 1951:2000)

# error expected
tis_test4 <- c(1945:1955)
tis_test5 <- list(1951:2000,NULL,1951:2000)
tis_test6 <- "A"
tis_test7 <- list(A = c(1950:2000),B = c(1951:2000))
tis_test8 <- list(A = 1951:2000, B = 1955:1990, D = 1951:2000)
tis_test9 <- list(A = c(1950:2000),B = NULL,C = c(1951:2000))


test_that("Subsetting and restricting indicators by the time dimension",{

  # isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), tis = TRUE, print.searchinfo = FALSE) # normal version
  expect_silent(isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), tis = TRUE, print.searchinfo = FALSE,tis_time = tis_test1))
  expect_silent(isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), tis = TRUE, print.searchinfo = FALSE,tis_time = tis_test2))
  expect_silent(isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), tis = TRUE, print.searchinfo = FALSE,tis_time = tis_test3))
  # showing that a lot is mopped up with time FE
  expect_silent(isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), tis = TRUE, print.searchinfo = FALSE, tis_time = tis_test3, effect = "individual"))

  expect_error(isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), tis = TRUE, print.searchinfo = FALSE,
                         tis_time = tis_test4),
               regexp = "Some or all time periods in 'tis_time' not found in the data. Please check the input under 'tis_time'.")

  expect_error(isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), tis = TRUE, print.searchinfo = FALSE,
                         tis_time = tis_test5),
               regexp = "All elements of 'tis_time' must be named to be able to be matched to the id's")

  expect_error(isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), tis = TRUE, print.searchinfo = FALSE,
                         tis_time = tis_test6),
               regexp = "must either be in the format of the time dimension or a list with one element per id")

  expect_error(isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), tis = TRUE, print.searchinfo = FALSE,
                         tis_time = tis_test7),
               regexp = "When providing 'tis_time' as a list, the number of elements must be equal to the number of id's.")

  expect_error(isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), tis = TRUE, print.searchinfo = FALSE, tis_time = tis_test8),
               regexp = "All named elements of 'tis_time' must be an id in the data. The following ids are not in the data: D")

  expect_error(isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), tis = TRUE, print.searchinfo = FALSE, tis_time = tis_test9),
               regexp = "Some or all time periods in 'tis_time' not found in the data. Please check the input under 'tis_time'.")

})




test_that("Subsetting and restricting indicators in fesis and cfesis",{

  expect_silent(isatpanel(trial_df, formula = y ~ x, index = c("id","year"), print.searchinfo = FALSE, fesis_time = tis_test1, fesis = TRUE))
  expect_silent(isatpanel(trial_df, formula = y ~ x, index = c("id","year"), print.searchinfo = FALSE, fesis_time = tis_test2, fesis = TRUE))
  #expect_silent(isatpanel(trial_df, formula = y ~ x, index = c("id","year"), print.searchinfo = FALSE, cfesis_time = tis_test1, cfesis = TRUE))
  #expect_silent(isatpanel(trial_df, formula = y ~ x, index = c("id","year"), print.searchinfo = FALSE, cfesis_time = tis_test2, cfesis = TRUE))

})

trial_df_date <- trial_df_step
trial_df_date$year <- rep(seq.Date(from = as.Date("2000-01-01"), length.out = 50, by = "month"),3)

tis_test1 <- seq.Date(from = as.Date("2001-01-01"), length.out = 20, by = "month")

test_that("Subsetting and restricting indicators by the time dimension (using dates)",{

  expect_silent(isatpanel(trial_df_date, formula = y ~ x, index = c("id","year"), tis = TRUE, print.searchinfo = FALSE,tis_time = tis_test1))

})






