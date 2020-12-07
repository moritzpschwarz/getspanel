# rm(list = ls())
# library(gets)
 library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(gridExtra)
# library(data.table)

set.seed(123)

data(co2driverseu)
data <- data[-1]

data$lgdp_sq <- data$lgdp^2

data$transport.emissions_pc <- data$transport.emissions/data$pop
data$ltransport.emissions_pc <- log(data$transport.emissions_pc)
data$growthtransport.emissions_pc <- log(data$transport.emissions_pc/exp(data$ltransport.emissions_pc))

EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg",
          "Netherlands", "Greece", "Portugal", "Sweden")
EU31 <- c(EU15, "Croatia", "Bulgaria", "Cyprus","Czech Republic", "Estonia",
          "Hungary", "Lithuania", "Latvia", "Malta", "Poland", "Romania",
          "Slovak Republic", "Slovenia", "Switzerland", "Iceland",
          "Norway")


###### Analysis:


# Specify parameters:
syear <- 1995
runit <- "Austria"


dat <- dplyr::filter(data, country %in% EU15, year>=syear)
controls <- data.frame(dat %>% dplyr::select(lgdp,lgdp_sq))
# y=dat$ltransport.emissions_pc
# id=dat$country
# time=dat$year
# mxreg=controls
# mxbreak=c(dat$const)
# break.method="both"



# Specify control variables:
test_that("test",{
  expect_silent(controls <- data.frame(dat %>% dplyr::select(lgdp,lgdp_sq)))
})


is_default_ar1 <- isatpanel(
  y=dat$ltransport.emissions_pc,
  id=dat$country,
  time=dat$year,
  mxreg=controls,
  fesis = TRUE,
  effect="twoways",
  iis=TRUE,
  t.pval=0.01,
  ar=1)



is_default_ar1_cfesiseu15 <- isatpanel(
  y=dat$ltransport.emissions_pc,
  id=dat$country,
  time=dat$year,
  mxreg=controls,
  fesis = TRUE,
  effect="twoways",
  iis=TRUE,
  cfesis = TRUE,
  cfesis_id = c("Italy"),
  #cfesis_id = c("Croatia", "Bulgaria", "Cyprus", "Czech Republic", "Estonia", "Hungary", "Lithuania", "Latvia", "Malta", "Poland", "Romania", "Slovak Republic", "Slovenia"),
  t.pval=0.01,
  ar=1)




is_default <- isatpanel(
  y=dat$ltransport.emissions_pc,
  id=dat$country,
  time=dat$year,
  mxreg=controls,
  fesis = TRUE,
  effect="twoways",
  iis=TRUE,
  t.pval=0.01)



is_default_csis <- isatpanel(
  y=dat$ltransport.emissions_pc,
  id=dat$country,
  time=dat$year,
  mxreg=controls,
  #fesis = TRUE,
  csis = TRUE,
  effect="twoways",
  iis=TRUE,
  t.pval=0.01)



# Break analysis:
is_default <- isatpanel(
  y=dat$ltransport.emissions_pc,
  id=dat$country,
  time=dat$year,
  mxreg=controls,
  mxbreak=c(dat$const),
  break.method="both",
  effect="twoways",
  #engine = "fixest",
  iis=TRUE,
  t.pval=0.01)


is_fixest_nocluster <- isatpanel(
  y=dat$ltransport.emissions_pc,
  id=dat$country,
  time=dat$year,
  mxreg=controls,
  mxbreak=c(dat$const),
  break.method="both",
  effect="twoways",
  engine = "fixest",
  cluster = "none",
  iis=TRUE,
  t.pval=0.01)


is_fixest <- isatpanel(
  y=dat$ltransport.emissions_pc,
  id=dat$country,
  time=dat$year,
  mxreg=controls,
  mxbreak=c(dat$const),
  break.method="both",
  effect="twoways",
  engine = "fixest",
  iis=TRUE,
  t.pval=0.01)


# is_felm <- isatpanel(
#   y=dat$ltransport.emissions_pc,
#   id=dat$country,
#   time=dat$year,
#   mxreg=controls,
#   mxbreak=c(dat$const),
#   break.method="both",
#   effect="twoways",
#   engine = "felm",
#   iis=TRUE,
#   t.pval=0.01)



is_fixest_nocluster
df <- is_fixest_nocluster$inputdata
indicators <- is_fixest_nocluster$isatpanel.result$aux$mX
indicators <- indicators[,!colnames(indicators) %in% names(df)]
df <- cbind(df,indicators)

form <- as.formula(paste0("y ~ ",paste0(names(df %>% select(-time,-id,-y,-mxbreak)),collapse = " + ")))

plm(formula = form,data = df,effect = "twoways",model = "within",index = c("id","time")) -> plm_obj

purtest(df %>% select(-mxbreak,-id,-time))












############
# EU 31


is_default_ar1_cfesiseu15 <- isatpanel(
  y=dat$ltransport.emissions_pc,
  id=dat$country,
  time=dat$year,
  mxreg=controls,
  fesis = TRUE,
  effect="twoways",
  iis=TRUE,
  cfesis = TRUE,
  cfesis_id = c("Croatia"),
  #cfesis_id = c("Croatia", "Bulgaria", "Cyprus", "Czech Republic", "Estonia", "Hungary", "Lithuania", "Latvia", "Malta", "Poland", "Romania", "Slovak Republic", "Slovenia"),
  t.pval=0.01,
  ar=1)

