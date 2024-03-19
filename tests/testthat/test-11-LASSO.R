
# # Single TS Example
# # Generate some random data
# set.seed(123)
# x <- rnorm(50, mean = 100)
# ep <- rnorm(50, sd = 0.2)
# trend <- 1951:2000
# trendbreak <- c(rep(0,24),1:26) # impose a trendbreak from 1975
# y <- 10 + 0.5 * x + 0.1 * trend - 0.2 * trendbreak + ep
# df <- data.frame(id = "A",
#                  year = 1951:2000,
#                  y = y,
#                  x = x,
#                  trend = trend,
#                  trendbreak = trendbreak,
#                  ep = ep)
#
# # Show a model without considering the trendbreak
# #gets::arx(y = y, mc = TRUE, mxreg = df[,c("x","year")], plot = TRUE)
#
# # Running TIS
# #gets::isat(y = y, mc = TRUE, mxreg = df[,c("x","year")], sis = FALSE, tis = TRUE, plot = TRUE)
#
#
# # show this result as well using two break indicators
# # this first one always starts 0,0,1,2...
# num_trend <- trend - 1950
# num_trend_break <- rep(0,50)
# num_trend_break[25:50] <- 1:26
#
# # this one works just like MIS or csis/cfesis in getspanel
# # the original trend would have been 1,2,3,4
# # the MIS result for this would be 0,0,3,4
# full_trend_break <- 1:50
# full_trend_break[1:24] <- 0
#
# df$full_trend_break <- full_trend_break
# df$num_trend_break <- num_trend_break
#
# # the MIS approach obviously does not produce the right result
# #gets::arx(y = y, mc = TRUE, mxreg = df[,c("x","year","full_trend_break")], plot = TRUE)
#
# # while the TIS approach works
# #gets::arx(y = y, mc = TRUE, mxreg = df[,c("x","year","num_trend_break")], plot = TRUE)

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
trial_df_step$y[40:50] <- trial_df_step$y[40:50]*1.025

test_that("LASSO works", {

  # expect_silent(m1 <- isatpanel(trial_df, formula = y ~ x, index = c("id","year"), tis = TRUE, plot = FALSE, print.searchinfo = FALSE))
  # expect_type(get_indicators(m1), type = "list")
  # expect_identical(names(get_indicators(m1)), "tis")
  # expect_identical(get_indicators(m1)$tis$name, c("tisA.1952", "tisA.1970", "tisC.1956"))

  expect_error(isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), fesis = TRUE, plot = TRUE, print.searchinfo = TRUE,
                         lasso_opts = "test", effect = "twoways"), regexp = "can only be supplied when 'engine' is set to 'lasso'")

  expect_error(isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), fesis = TRUE, plot = TRUE, print.searchinfo = TRUE,
                         lasso_opts = "test", effect = "twoways", engine = "lasso"), regexp = "must be a list and can only take the elements")


  expect_error(isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), fesis = TRUE, plot = TRUE, print.searchinfo = TRUE,
                         lasso_opts = list(standardize = FALSE,
                                           nfolds = 100,
                                           adaptive = TRUE,
                                           test = 100), effect = "twoways", engine = "lasso"), regexp = "must be a list and can only take the elements")

  expect_silent(isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), fesis = TRUE, plot = TRUE, print.searchinfo = TRUE,
                          lasso_opts = list(standardize = FALSE,
                                            nfolds = 10), effect = "twoways", engine = "lasso"))




  isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), plot = TRUE, print.searchinfo = TRUE,effect = "twoways", engine = "lasso",
            fesis = TRUE, tis = TRUE,
            lasso_opts = list(standardize = FALSE,
                              nfolds = 50))

  isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), plot = TRUE, print.searchinfo = TRUE,effect = "twoways", engine = "lasso",
            fesis = TRUE, tis = FALSE,
            lasso_opts = list(standardize = FALSE,
                              nfolds = 50))


  isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), plot = TRUE, print.searchinfo = TRUE,effect = "twoways", engine = "lasso",
            fesis = TRUE, tis = FALSE, iis = TRUE,
            lasso_opts = list(standardize = FALSE,
                              nfolds = 10))

  isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), plot = TRUE, print.searchinfo = TRUE,effect = "twoways", engine = "lasso",
            fesis = TRUE, tis = TRUE, iis = TRUE,
            lasso_opts = list(standardize = TRUE,
                              adaptive = TRUE,
                              nfolds = 10))


  isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), fesis = TRUE, plot = TRUE, print.searchinfo = TRUE, engine = "lasso", effect = "twoways")
  isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), fesis = TRUE, tis = TRUE, plot = TRUE, print.searchinfo = TRUE, engine = "lasso", effect = "twoways")
  isatpanel(trial_df_step, formula = y ~ x, index = c("id","year"), fesis = TRUE, tis = TRUE, iis = TRUE, plot = TRUE, print.searchinfo = TRUE, engine = "lasso", effect = "twoways")

})

