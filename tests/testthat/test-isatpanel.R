test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


##########################
######### load pandata_simulated
###########################
data("pandata_simulated")

par(mfrow=c(2,2))
plot(pandata_simulated$gdp[pandata_simulated$country==1], type="l", main="Country 1 (Break at t=35)")
plot(pandata_simulated$gdp[pandata_simulated$country==2], type="l", main="Country 2 (Break at t=35)")
plot(pandata_simulated$gdp[pandata_simulated$country==3], type="l", main="Country 3 (No Break)")
plot(pandata_simulated$gdp[pandata_simulated$country==4], type="l", main="Country 4 (No Break)")



# Normal testing

isatpanel(data = pandata_simulated,formula = gdp~temp, index = c("country","year"))
newmethod <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE)
newmethod_ar <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE, ar = 1)
newmethod_cfesis <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE, cfesis = TRUE, ar = 1)


# Unit testing
test_that("Initial Tests Isatpanel on simulated data",{

  expect_output(isatpanel(data = pandata_simulated,formula = gdp~temp, index = c("country","year")))

  newmethod <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE)

  newmethod_ar <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE, ar = 1)





})


test_that("Test the cfesis and csis arguments",{

  newmethod_cfesis <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE, cfesis = TRUE, ar = 1)
  newmethod_cfesis_sub <- isatpanel(data = pandata_simulated,formula = gdp~temp + I(temp^2), index = c("country","year"),fesis=TRUE, cfesis = TRUE,cfesis_id = c("2","3"), ar = 1)

})








###############################################################
############ Different Break Specifications (corresponding to the 4 cases in the paper) #################
##############################################################

##########################################################
###### 1: Allowing for breaks in individual fixed effects
#
is1 <- isatpanel(y=pandata_simulated$gdp,
                 id=pandata_simulated$country,
                 time=pandata_simulated$year,
                 mxreg=pandata_simulated$temp,
                 mxbreak=c(pandata_simulated$const),
                 break.method="both",
                 effect="twoways",
                 iis=FALSE,
                 t.pval=0.01)



is1_felm <- isatpanel(y=pandata_simulated$gdp,
                      id=pandata_simulated$country,
                      time=pandata_simulated$year,
                      mxreg=pandata_simulated$temp,
                      mxbreak=c(pandata_simulated$const),
                      break.method="both",
                      effect="twoways",
                      iis=FALSE,
                      t.pval=0.01,
                      engine = "felm",
                      cluster = "individual")



is1_felm_standardSE <- isatpanel(y=pandata_simulated$gdp,
                                 id=pandata_simulated$country,
                                 time=pandata_simulated$year,
                                 mxreg=pandata_simulated$temp,
                                 mxbreak=c(pandata_simulated$const),
                                 break.method="both",
                                 effect="twoways",
                                 iis=FALSE,
                                 t.pval=0.01,
                                 engine = "felm",
                                 cluster = "none")



is1_fixest <- isatpanel(y=pandata_simulated$gdp,
                        id=pandata_simulated$country,
                        time=pandata_simulated$year,
                        mxreg=pandata_simulated$temp,
                        mxbreak=c(pandata_simulated$const),
                        break.method="both",
                        effect="twoways",
                        iis=FALSE,
                        t.pval=0.01,
                        engine = "fixest",
                        cluster = "individual")


is1_fixest_standardSE <- isatpanel(y=pandata_simulated$gdp,
                                   id=pandata_simulated$country,
                                   time=pandata_simulated$year,
                                   mxreg=pandata_simulated$temp,
                                   mxbreak=c(pandata_simulated$const),
                                   break.method="both",
                                   effect="twoways",
                                   iis=FALSE,
                                   t.pval=0.01,
                                   engine = "fixest",
                                   cluster = "none")



is1_fixest_timeclus <- isatpanel(y=pandata_simulated$gdp,
                                 id=pandata_simulated$country,
                                 time=pandata_simulated$year,
                                 mxreg=pandata_simulated$temp,
                                 mxbreak=c(pandata_simulated$const),
                                 break.method="both",
                                 effect="twoways",
                                 iis=FALSE,
                                 t.pval=0.01,
                                 engine = "fixest",
                                 cluster = "time")
#
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
