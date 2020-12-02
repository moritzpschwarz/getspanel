# rm(list = ls())
# library(gets)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(gridExtra)
# library(data.table)

set.seed(123)

data <- read.csv("data-raw/CO2DriversEU_dataset_v1.csv")
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


dat <- filter(data, country %in% EU15, year>=syear)

# Specify control variables:
controls <- data.frame(dat %>% select(lgdp,lgdp_sq))
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


is_felm <- isatpanel(
  y=dat$ltransport.emissions_pc,
  id=dat$country,
  time=dat$year,
  mxreg=controls,
  mxbreak=c(dat$const),
  break.method="both",
  effect="twoways",
  engine = "felm",
  iis=TRUE,
  t.pval=0.01)




is_default$inputdata
plm::purtest()

