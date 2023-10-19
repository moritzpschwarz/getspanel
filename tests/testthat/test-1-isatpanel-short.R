
##########################
######### load pandata_simulated
###########################
data("pandata_simulated")

# par(mfrow=c(2,2))
# plot(pandata_simulated$gdp[pandata_simulated$country==1], type="l", main="Country 1 (Break at t=35)")
# plot(pandata_simulated$gdp[pandata_simulated$country==2], type="l", main="Country 2 (Break at t=35)")
# plot(pandata_simulated$gdp[pandata_simulated$country==3], type="l", main="Country 3 (No Break)")
# plot(pandata_simulated$gdp[pandata_simulated$country==4], type="l", main="Country 4 (No Break)")

pandata_simulated <- pandata_simulated[pandata_simulated$year>1979,]

# Normal testing

# isatpanel(data = pandata_simulated,formula = gdp~temp, index = c("country","year"), fesis=TRUE)
# newmethod <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE)
# newmethod_ar <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE, ar = 1)
# newmethod_cfesis <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE, cfesis = TRUE, ar = 1)


# Unit testing
test_that("Initial Tests Isatpanel on simulated data",{

  expect_message(isatpanel(data = pandata_simulated,formula = gdp~temp, index = c("country","year"),fesis = TRUE))

  #newmethod <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE)

  expect_message(isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2),
                           index = c("country","year"),fesis=TRUE, ar = 1))

})



test_that("Isatpanel Test that missing values are removed",{
  data_w_missing <- pandata_simulated
  data_w_missing <- data_w_missing[data_w_missing$year>1979,]
  data_w_missing$temp <- ifelse(data_w_missing$country == 2, NA, data_w_missing$temp)

  expect_silent(a <- isatpanel(data = data_w_missing,formula = gdp~temp,
                               index = c("country","year"),fesis = TRUE, print.searchinfo = FALSE))
  expect_output(print(a))
  expect_true(a$isatpanel.result$n==63) # 63 because 3 * 21
})



test_that("Test the cfesis and csis arguments",{

  expect_silent(isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE, cfesis = TRUE, ar = 1, print.searchinfo = FALSE))
  expect_silent(newmethod_cfesis_sub <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE, cfesis = TRUE,cfesis_id = c("2","3"), ar = 1, print.searchinfo = FALSE))

})


#newmethod_cfesis <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE, ar = 1)




test_that("Standard Error Options using fixest",{
  skip_on_cran()
  expect_silent(result <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE, ar = 1, print.searchinfo=FALSE,engine = "fixest"))
  expect_silent(result <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE, ar = 1, print.searchinfo=FALSE,engine = "fixest", cluster = "individual"))
})



test_that("Check unbalanced panel",{
  # Create an unbalanced panel (deleting first 20 years of Country 3)
  unbalanced_panel <- subset(pandata_simulated,country %in% c(1,2,4) | (country==3 & year > 1920))
  expect_silent(result <- isatpanel(
    data = unbalanced_panel,
    formula = gdp ~ temp + I(temp ^ 2),
    index = c("country", "year"),
    fesis = TRUE,
    ar = 1,
    print.searchinfo = FALSE
  ))
})






###############################################################
############ Different Break Specifications (corresponding to the 4 cases in the paper) #################
##############################################################




test_that("Simple Fixest Test",{
  skip_on_cran()
  expect_silent(isatpanel(
    y = pandata_simulated$gdp,
    id = pandata_simulated$country,
    time = pandata_simulated$year,
    mxreg = pandata_simulated$temp,
    effect = "twoways",
    iis = FALSE,
    fesis = TRUE,
    t.pval = 0.01,
    engine = "fixest",
    cluster = "individual",
    print.searchinfo = FALSE
  ))
})

test_that("Simple Default Test",{
  skip_on_cran()
  expect_silent(isatpanel(
    y = pandata_simulated$gdp,
    id = pandata_simulated$country,
    time = pandata_simulated$year,
    mxreg = pandata_simulated$temp,
    effect = "twoways",
    iis = FALSE,
    fesis = TRUE,
    t.pval = 0.01,
    cluster = "none",
    print.searchinfo = FALSE
  ))
})


test_that("Simple Default Test with AR1",{
  skip_on_cran()
  expect_silent(isatpanel(
    y = pandata_simulated$gdp,
    id = pandata_simulated$country,
    time = pandata_simulated$year,
    mxreg = pandata_simulated$temp,
    effect = "twoways",
    iis = FALSE,
    fesis = TRUE,
    t.pval = 0.01,
    #cluster = "individual",
    ar = 1,
    print.searchinfo = FALSE
  ))

  expect_silent(
    isatpanel(
      y = pandata_simulated$gdp,
      id = pandata_simulated$country,
      time = pandata_simulated$year,
      mxreg = pandata_simulated$temp,
      effect = "twoways",
      iis = FALSE,
      fesis = TRUE,
      t.pval = 0.01,
      engine = "fixest",
      cluster = "individual",
      ar = 1,
      print.searchinfo = FALSE
    ))
})




test_that("Test that estimates of IIS are equal across methods and including perfectly linear terms", {
  skip_on_cran()
  pandata_simulated$int_rate <- rep(rnorm(21),4)
  pandata_simulated <- pandata_simulated[pandata_simulated$year>1979,]

  aa <- isatpanel(data = pandata_simulated,formula = gdp~temp,
                  index = c("country","year"),fesis = FALSE, iis = TRUE, effect = c("twoways"), print.searchinfo = FALSE)

  bb <- isatpanel(data = pandata_simulated,formula = gdp~temp + int_rate,
                  index = c("country","year"),fesis = FALSE, iis = TRUE, effect = c("twoways"), print.searchinfo = FALSE)

  cc <- isatpanel(data = pandata_simulated,formula = gdp~temp + int_rate,
                  index = c("country","year"),fesis = FALSE, iis = TRUE, effect = c("twoways"), engine = "fixest", print.searchinfo = FALSE)

  dd <- isatpanel(data = pandata_simulated,formula = gdp~temp + int_rate,
                  index = c("country","year"),fesis = FALSE, iis = TRUE, effect = c("twoways"), engine = "felm", print.searchinfo = FALSE)

  ee <- isatpanel(data = pandata_simulated,formula = gdp~temp + int_rate,
                  index = c("country","year"),fesis = FALSE, iis = TRUE, effect = c("twoways"), engine = "plm", print.searchinfo = FALSE)

  expect_true(identical(round(coef(aa$isatpanel.result)["temp"],7),
                        round(coef(bb$isatpanel.result)["temp"],7),
                        round(coef(cc$isatpanel.result)["temp"],7),
                        round(coef(dd$isatpanel.result)["temp"],7),
                        round(coef(ee$isatpanel.result)["temp"],7)))
})




test_that("Test that estimates of FESIS are equal across methods", {
  skip_on_cran()
  data("pandata_simulated")

  aa <- isatpanel(data = pandata_simulated,formula = gdp~temp,
                  index = c("country","year"), fesis = TRUE, effect = c("twoways"), print.searchinfo = FALSE)

  bb <- isatpanel(data = pandata_simulated,formula = gdp~temp,
                  index = c("country","year"), fesis = TRUE, effect = c("twoways"), engine = "fixest", print.searchinfo = FALSE)

  cc <- isatpanel(data = pandata_simulated,formula = gdp~temp,
                  index = c("country","year"), fesis = TRUE, effect = c("twoways"), engine = "felm", print.searchinfo = FALSE)

  dd <- isatpanel(data = pandata_simulated,formula = gdp~temp,
                  index = c("country","year"), fesis = TRUE, effect = c("twoways"), engine = "plm", print.searchinfo = FALSE)

  expect_true(identical(round(coef(aa$isatpanel.result)["temp"],7),
                        round(coef(bb$isatpanel.result)["temp"],7),
                        round(coef(cc$isatpanel.result)["temp"],7),
                        round(coef(dd$isatpanel.result)["temp"],7)))
})


test_that("Test that estimates of FESIS are equal across methods", {
  #skip_on_ci()
  skip_on_cran()
  data("pandata_simulated")
  pandata_simulated <- pandata_simulated[pandata_simulated$year>1979,]
  aa <- isatpanel(data = pandata_simulated,formula = gdp~temp,
                  index = c("country","year"), fesis = TRUE, effect = c("twoways"), print.searchinfo = FALSE)

  bb <- isatpanel(data = pandata_simulated,formula = gdp~temp,
                  index = c("country","year"), fesis = TRUE, effect = c("twoways"), print.searchinfo = FALSE, engine = "fixest")

  expect_true(identical(round(coef(aa$isatpanel.result)["temp"],7),
                        round(coef(bb$isatpanel.result)["temp"],7)))
})



#
#
# is1 <- isatpanel(y=pandata_simulated$gdp,
#                  id=pandata_simulated$country,
#                  time=pandata_simulated$year,
#                  mxreg=pandata_simulated$temp,
#                  mxbreak=c(pandata_simulated$const),
#                  break.method="both",
#                  effect="twoways",
#                  iis=FALSE,
#                  t.pval=0.01,
#                  engine = "felm",
#                  cluster = "individual")
#
#
#
#
# ##########################################################
# ###### 1: Allowing for breaks in individual fixed effects
# #
# is1 <- isatpanel(y=pandata_simulated$gdp,
#                  id=pandata_simulated$country,
#                  time=pandata_simulated$year,
#                  mxreg=pandata_simulated$temp,
#                  mxbreak=c(pandata_simulated$const),
#                  break.method="both",
#                  effect="twoways",
#                  iis=FALSE,
#                  t.pval=0.01)
#
#
#
# is1_felm <- isatpanel(y=pandata_simulated$gdp,
#                       id=pandata_simulated$country,
#                       time=pandata_simulated$year,
#                       mxreg=pandata_simulated$temp,
#                       mxbreak=c(pandata_simulated$const),
#                       break.method="both",
#                       effect="twoways",
#                       iis=FALSE,
#                       t.pval=0.01,
#                       engine = "felm",
#                       cluster = "individual")
#
#
#
# is1_felm_standardSE <- isatpanel(y=pandata_simulated$gdp,
#                                  id=pandata_simulated$country,
#                                  time=pandata_simulated$year,
#                                  mxreg=pandata_simulated$temp,
#                                  mxbreak=c(pandata_simulated$const),
#                                  break.method="both",
#                                  effect="twoways",
#                                  iis=FALSE,
#                                  t.pval=0.01,
#                                  engine = "felm",
#                                  cluster = "none")
#
#
#
# is1_fixest <- isatpanel(y=pandata_simulated$gdp,
#                         id=pandata_simulated$country,
#                         time=pandata_simulated$year,
#                         mxreg=pandata_simulated$temp,
#                         mxbreak=c(pandata_simulated$const),
#                         break.method="both",
#                         effect="twoways",
#                         iis=FALSE,
#                         t.pval=0.01,
#                         engine = "fixest",
#                         cluster = "individual")
#
#
# is1_fixest_standardSE <- isatpanel(y=pandata_simulated$gdp,
#                                    id=pandata_simulated$country,
#                                    time=pandata_simulated$year,
#                                    mxreg=pandata_simulated$temp,
#                                    mxbreak=c(pandata_simulated$const),
#                                    break.method="both",
#                                    effect="twoways",
#                                    iis=FALSE,
#                                    t.pval=0.01,
#                                    engine = "fixest",
#                                    cluster = "none")
#
#
#
# is1_fixest_timeclus <- isatpanel(y=pandata_simulated$gdp,
#                                  id=pandata_simulated$country,
#                                  time=pandata_simulated$year,
#                                  mxreg=pandata_simulated$temp,
#                                  mxbreak=c(pandata_simulated$const),
#                                  break.method="both",
#                                  effect="twoways",
#                                  iis=FALSE,
#                                  t.pval=0.01,
#                                  engine = "fixest",
#                                  cluster = "time")
# #
# ##################################################################
# ###### 2: Allowing for breaks in time (common across individuals)
#
# is2 <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "time",
#   effect = "twoways",
#   iis = FALSE,
#   t.pval = 0.01
# )
#
#
# is2_felm <-isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "time",
#   effect = "twoways",
#   iis = FALSE,
#   t.pval = 0.01,
#   engine = "felm",
#   cluster = "individual")
#
# is2_felm_standardSE <-isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "time",
#   effect = "twoways",
#   iis = FALSE,
#   t.pval = 0.01,
#   engine = "felm",
#   cluster = "none")
#
# is2_fixest <-isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "time",
#   effect = "twoways",
#   iis = FALSE,
#   t.pval = 0.01,
#   engine = "fixest",
#   cluster = "individual")
#
# ##########################################################################
# ###### 3: Allowing for breaks across units (heterogeneity of coefficients)
#
# is3 <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "individual",
#   effect = "twoways",
#   iis = FALSE,
#   t.pval = 0.01
# )
#
#
# is3_felm <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "individual",
#   effect = "twoways",
#   iis = FALSE,
#   t.pval = 0.01,
#   engine= "felm",
#   cluster="individual"
# )
#
# is3_felm_standardSE <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "individual",
#   effect = "twoways",
#   iis = FALSE,
#   t.pval = 0.01,
#   engine= "felm",
#   cluster="0"
# )
#
#
# is3_fixest <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "individual",
#   effect = "twoways",
#   iis = FALSE,
#   t.pval = 0.01,
#   engine= "fixest",
#   cluster="individual"
# )
#
# #####################################################
# ###### 3.1: Allowing for breaks across units and time
#
# is3.1 <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "both",
#   effect = "twoways",
#   iis = FALSE,
#   t.pval = 0.01
# )
#
#
#
# is3.1_felm <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "both",
#   effect = "twoways",
#   iis = FALSE,
#   t.pval = 0.01,
#   engine = "felm",
#   cluster="individual"
# )
#
# is3.1_felm_standardSE <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "both",
#   effect = "twoways",
#   iis = FALSE,
#   t.pval = 0.01,
#   engine = "felm",
#   cluster="0"
# )
#
#
# is3.1_fixest <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "both",
#   effect = "twoways",
#   iis = FALSE,
#   t.pval = 0.01,
#   engine = "fixest",
#   cluster="individual"
# )
# ## interpreting the output:
# # two breaks are detected at t=35 for unit id=1 and 2, estimates break magnitudes are given by the coefficients.
#
# ####################################
# ##### 4: Allowing for outliers
#
# is4 <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = NULL,
#   break.method = "both",
#   effect = "twoways",
#   iis = TRUE,
#   t.pval = 0.01
# )
#
#
# is4_felm <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = NULL,
#   break.method = "both",
#   effect = "twoways",
#   iis = TRUE,
#   t.pval = 0.01,
#   engine = "felm",
#   cluster = "individual"
# )
#
# is4_felm_standardSE <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = NULL,
#   break.method = "both",
#   effect = "twoways",
#   iis = TRUE,
#   t.pval = 0.01,
#   engine = "felm",
#   cluster = "none"
# )
#
# is4_fixest <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = NULL,
#   break.method = "both",
#   effect = "twoways",
#   iis = TRUE,
#   t.pval = 0.01,
#   engine = "fixest",
#   cluster = "individual"
# )
#
# ######################################################################
# ############ Different Fixed Effect Specifications #################
# #####################################################################
#
# ######### allowing for breaks over time and units (not- common across units)
# #### Individual Fixed effects only:
#
# is3.1.1 <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "both",
#   effect = "individual",
#   iis = FALSE
# )
#
#
# is3.1.1_felm <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "both",
#   effect = "individual",
#   iis = FALSE,
#   engine="felm",
#   cluster="individual"
#
# )
#
# is3.1.1_felm_standardSE <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "both",
#   effect = "individual",
#   iis = FALSE,
#   engine="felm",
#   cluster="0"
#
# )
#
# is3.1.1_fixest <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "both",
#   effect = "individual",
#   iis = FALSE,
#   engine="fixest",
#   cluster="individual"
#
# )
#
# #### Time Fixed effects only:
#
# is3.1.2 <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "both",
#   effect = "time",
#   iis = FALSE
# )
#
#
# is3.1.2_felm <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "both",
#   effect = "time",
#   iis = FALSE,
#   engine = "felm",
#   cluster="individual"
# )
#
# is3.1.2_felm_standardSE <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "both",
#   effect = "time",
#   iis = FALSE,
#   engine = "felm",
#   cluster="0"
# )
#
#
#
#
# #source(here("code","internal","getsFun.R"))
# is3.1.2_fixest <- isatpanel(
#   y = pandata_simulated$gdp,
#   id = pandata_simulated$country,
#   time = pandata_simulated$year,
#   mxreg = pandata_simulated$temp,
#   mxbreak = c(pandata_simulated$temp),
#   break.method = "both",
#   effect = "time",
#   iis = FALSE,
#   engine = "fixest",
#   cluster="time"
# )
#
#
#
# ######################################################################
# ########## Running in Parallel
# ######################################################################
# #
# # library(parallel)
# # detectCores()
# #
# # ###### allow for breaks over time and units (not- common across units) in parallel
# # is3.1.p <- isatpanel(
# #   y = pandata_simulated$gdp,
# #   id = pandata_simulated$country,
# #   time = pandata_simulated$year,
# #   mxreg = pandata_simulated$temp,
# #   mxbreak = c(pandata_simulated$temp),
# #   break.method = "both",
# #   effect = "twoways",
# #   iis = FALSE,
# #   parallel.options = 2
# # )
# #
# #
# #
#
#
#
#
#
#
#
#
