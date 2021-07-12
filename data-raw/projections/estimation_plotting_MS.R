# ################# Partial Replication Code: Pretis, Schwarz, Tang, Haustein, Allen 2018
#
#
# rm(list = ls())  #clear the workspace
#
# setwd('C:/Users/Felix/OneDrive/Documents/Projects/HAPPI15Impacts')
# #global.climate <- read.csv ("./for replication/data/PSTHA_2018_rep_data.csv")
# global.climate <- read.csv ("./data/estimation data/globalclimatedataset_merged_revised.csv")
#
#
# iso <- as.character(global.climate$iso)
#
# ####Variables
# temp <- global.climate$UDel_mean
# temp2 <- global.climate$UDel_mean_2
# temp_max <- global.climate$UDel_max
# temp_min <- global.climate$UDel_min
# temp_var <-  global.climate$temp_cell_var
#
# precip_popweight <- (global.climate$UDel_prcp_mean)
# precip_popweight2 <- global.climate$UDel_prcp_mean_2
# precip_min <- global.climate$UDel_prcp_min
# precip_max <- global.climate$UDel_prcp_max
# precip_var <- global.climate$prcp_cell_var
#
# gdp_pc <- global.climate$gdpCAP_wdi
#
# temp_gdp <- gdp_pc * temp
# temp2_gdp <- gdp_pc * temp2
#
# temp_loggdp <- log(gdp_pc) * temp
# temp2_loggdp <- log(gdp_pc) * temp2
#
# year <- global.climate$year
# country <- global.climate$iso
#
# ###Differentiating Between Agricultural and Non-Agr.
# growthagr <- global.climate$AgrGDPgrowth
# growthnonagr <- global.climate$NonAgrGDPgrowth
#
#
# ###########################################
#
# growthwdi <- global.climate$growthWDI
# year <- as.ts(global.climate$year)
# iso_idi <- as.numeric(as.factor(global.climate$iso))
#
#
# ###generate lags
# #install.packages("plyr")
# library(plyr)
# lg <- function(x){
#   c(NA, x[1:(length(x)-1)])
# }
#
#
# global.climate.lag <- ddply(global.climate, ~iso, transform, L1.growthWDI = lg(growthWDI))
# global.climate.lag <- ddply(global.climate.lag, ~iso, transform, L1.growthagr = lg(AgrGDPgrowth))
#
# L1.growthwdi<- global.climate.lag$L1.growthWDI
# L1.growthagr <- global.climate.lag$L1.growthagr
#
# global.climate.lag <- ddply(global.climate.lag, ~iso, transform, L1.gdp_pc = lg(gdpCAP_wdi))
#
# L1.gdp_pc <- global.climate.lag$L1.gdp_pc
#
# temp_L1.loggdp <- log(L1.gdp_pc) * temp
# temp2_L1.loggdp <- log(L1.gdp_pc) * temp2
#
# ##############
# time <- global.climate$time
# time2 <- time*time
#
# trend.start <- which(names(global.climate)=="X_yi_isoXtim_2")
# trend.end <- NCOL(global.climate)
# yearly_ours <- global.climate[, trend.start:trend.end]
# #install.packages("dummies")
# library(dummies)
#
# dum_year <- dummy(year)
# dum_id <- dummy(iso_idi)
#
# reg.dat <- data.frame(cbind(year, iso_idi, growthwdi, L1.growthwdi, growthagr, L1.growthagr, growthnonagr, precip_popweight, precip_popweight2, temp,
#                             temp2,  temp_max, temp_min, temp_var, precip_min, precip_max,
#                             precip_var, time, time2, dum_year, dum_id, as.matrix(yearly_ours)))
#
# ##########################
# ####1) Model M1: Extended model with lags but without indicators
# ##########################
#
# drops <- c("X", "L1.growthagr",  "iso_idi", "year", "growthagr", "growthnonagr")
#
# mxy <- reg.dat[ , !(names(reg.dat) %in% drops)]
# mxy.com <- mxy[complete.cases(mxy),]
# m1 <- lm (growthwdi ~ temp + temp2 + precip_popweight + precip_popweight2 +., data = mxy.com)
# summary(m1)
#
# #### with adaptation interaction
#
# reg.dat <- data.frame(cbind(year, iso_idi, growthwdi, L1.growthwdi, growthagr, L1.growthagr, growthnonagr, precip_popweight, precip_popweight2, temp,
#                             temp2, temp_loggdp, temp2_loggdp, temp_gdp, temp2_gdp, time, time2, dum_year, dum_id, as.matrix(yearly_ours)))
#
# reg.dat.lag <- data.frame(cbind(year, iso_idi, growthwdi, L1.growthwdi, growthagr, L1.growthagr, growthnonagr, precip_popweight, precip_popweight2, temp,
#                                 temp2, temp_L1.loggdp, temp2_L1.loggdp, temp_gdp, temp2_gdp, time, time2, dum_year, dum_id, as.matrix(yearly_ours)))
#
# reg.dat$index <- seq(1:NROW(reg.dat))
# ctry_index <- reg.dat[,c("iso_idi", "year", "index")]
# ctry_index <- cbind(ctry_index, country)
#
# reg.dat.lag$index <- seq(1:NROW(reg.dat.lag))
# ctry_index.lag <- reg.dat.lag[,c("iso_idi", "year", "index")]
# ctry_index.lag <- cbind(ctry_index.lag, country)
#
#
#
# drops <- c("X", "L1.growthagr",  "iso_idi", "year", "growthagr", "growthnonagr", "temp_gdp", "temp2_gdp", "L1.growthwdi")
# drops.lag <- c("X", "L1.growthagr",  "iso_idi", "year", "growthagr", "growthnonagr", "temp_gdp", "temp2_gdp", "L1.growthwdi")
#
#
# mxy <- reg.dat[ , !(names(reg.dat) %in% drops)]
# mxy.lag <- reg.dat.lag[ , !(names(reg.dat.lag) %in% drops)]
#
# incomp.drop <- which(!complete.cases(mxy))
# mxy.com <- mxy[complete.cases(mxy),]
#
# incomp.drop.lag <- which(!complete.cases(mxy.lag))
# mxy.com.lag <- mxy.lag[complete.cases(mxy.lag),]
#
# ctry_index.com <- ctry_index[complete.cases(mxy),]
# ctry_index.com.lag <- ctry_index.lag[complete.cases(mxy.lag),]
#
#
# m2 <- lm (growthwdi ~ temp + temp2 + precip_popweight + precip_popweight2 +., data = mxy.com)
# summary(m2)
#
# m2.lag <- lm (growthwdi ~ temp + temp2 + precip_popweight + precip_popweight2 +., data = mxy.com.lag)
# summary(m2.lag)
#
#
# library(gets)
#
# na.coefs <- names(coefficients(m2))[which(is.na(coefficients(m2)))]
# mx.com.drop <- mxy.com[ , !(names(mxy.com) %in% na.coefs)]
#
# mxy.com.dep <- mx.com.drop[,"growthwdi"]
# mxy.com.reg <- mx.com.drop[,-which(colnames(mx.com.drop)=="growthwdi")]
#
# na.coefs.lag <- names(coefficients(m2.lag))[which(is.na(coefficients(m2.lag)))]
# mx.com.drop.lag <- mxy.com.lag[ , !(names(mxy.com.lag) %in% na.coefs.lag)]
#
# mxy.com.dep.lag <- mx.com.drop.lag[,"growthwdi"]
# mxy.com.reg.lag <- mx.com.drop.lag[,-which(colnames(mx.com.drop.lag)=="growthwdi")]
#
# m1.arx <- arx(y=mxy.com.dep, mxreg=mxy.com.reg, mc=TRUE)
# m1.arx.lag <- arx(y=mxy.com.dep.lag, mxreg=mxy.com.reg.lag, mc=TRUE)
#
# coefficients(m1.arx.lag)[1:5]
# coefficients(m2.lag)[1:5]
#
# start.time <- Sys.time() #takes a looooong time. Around 6hrs on my laptop.
#
# m1.isat <- isat(y=mxy.com.dep, mxreg=mxy.com.reg, mc=TRUE, iis=TRUE, t.pval=0.01, sis=FALSE, max.block.size = 2, parallel.options = 7)
#
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
#
# ##### save image to harddisk so I don't have to run it again
# harddiskpath <- "D:/Documents/Isat Beta Testing/"
# save.image(file= paste(harddiskpath, "isat_app_damage_v4.RData", sep=""))
# load(file= paste(harddiskpath, "isat_app_damage_v2.RData", sep=""))
#
# start.time <- Sys.time()
#
# m1.isat.lag <- isat(y=mxy.com.dep.lag, mxreg=mxy.com.reg.lag, mc=TRUE, iis=TRUE, t.pval=0.01, sis=FALSE, max.block.size = 2, parallel.options = 7)
#
# end.time <- Sys.time()
#
# time.taken <- end.time - start.time
# time.taken
#
# harddiskpath <- "D:/Documents/Isat Beta Testing/"
# save.image(file= paste(harddiskpath, "isat_app_damage_v6.RData", sep=""))
# load(file= paste(harddiskpath, "isat_app_damage_v5.RData", sep=""))
#



library(here)
library(tidyverse)
library(gets)
library(getspanel)
library(xtable)
library(broom)

rm(list=ls())
load(here("data-raw/projections/m2.RData"))
load(here("data-raw/projections/m2.isat.RData"))
load(here("data-raw/projections/am2.RData"))
load(here("data-raw/projections/am2.isat.RData"))
load(here("data-raw/projections/am2_L1.RData"))
load(here("data-raw/projections/am2.isat_L1.RData"))



dat <- vroom::vroom(file = here("data-raw/projections/damage_curve_country_dataset_timetrends_updated02-19.csv"))
m2_data <- vroom::vroom(file = here("data-raw/projections/m2_data.csv"))
am2_data <- vroom::vroom(file = here("data-raw/projections/am2_data.csv"))
am2_L1_data <- vroom::vroom(file = here("data-raw/projections/am2_L1_data.csv"))

# # Estimate M1 -------------------------------------------------------------
#
# dat %>%
#   select(iso, year, diff.ln_gdp_cap, temp, temp_2, prcp, prcp_2 , starts_with(c("year_","time_", "iso_"))) %>%
#   select(-iso,-year) %>%
#   lm(diff.ln_gdp_cap~.-1,data = .) -> m1
#
# dat %>%
#   select(iso, year, diff.ln_gdp_cap, contains(c("temp","prcp"), ignore.case = FALSE), -contains("diff"),diff.ln_gdp_cap, starts_with(c("year_","time_", "iso_"))) %>%
#   select(-iso,-year) %>%
#   lm(diff.ln_gdp_cap~.-1,data = .) -> m1_L1
#
#
# # Process M1 --------------------------------------------------------------
#
#
# m1 %>%
#   tidy %>%
#   filter(is.na(estimate)) %>%
#   pull(term) -> m1_drop
#
# m1_L1 %>%
#   tidy %>%
#   filter(is.na(estimate)) %>%
#   pull(term) -> m1_L1_drop
#
#
#
# # Estimate M2 -------------------------------------------------------------
#
# dat %>%
#   select(iso, year, diff.ln_gdp_cap, temp, temp_2, prcp, prcp_2 , starts_with(c("year_","time_", "iso_"))) %>%
#   select(-all_of(m1_drop)) %>%
#   drop_na -> m2_data
#
# dat %>%
#   select(iso, year, diff.ln_gdp_cap, contains(c("temp","prcp"), ignore.case = FALSE), -contains("diff"),diff.ln_gdp_cap, starts_with(c("year_","time_", "iso_"))) %>%
#   select(-all_of(m1_L1_drop)) %>%
#   drop_na -> m2_L1_data



# Start of Felix' code ----------------------------------------------------

rel.coef <- c("temp", "temp_2")
rel.coef.adapt <- c("temp", "temp_2", "temp_int","temp_2_int")
rel.coef.adapt.L1 <- rel.coef.adapt
#rel.coef <- c("temp", "temp2", "temp_loggdp", "temp2_loggdp")
#rel.coef.lag <- c("temp", "temp2", "temp_L1.loggdp", "temp2_L1.loggdp")


dist1 <- getspanel::distorttest(m2.isat,  coef=rel.coef)
dist1.adapt <- getspanel::distorttest(am2.isat,  coef=rel.coef.adapt)
dist1.adapt.L1 <- getspanel::distorttest(am2.isat_L1,  coef=rel.coef.adapt)

coefficients(dist1.adapt.L1$ols)[1:10]
coefficients(am2_L1)[1:10]

coefficients(dist1.adapt.L1$ols)[rel.coef.adapt]

outliertest(m2.isat)
outliertest(am2.isat_L1)
outliertest(am2.isat)

########## Manually Create a results table
# Commented out my Moritz because done in a different way
#
# m1.ols <- dist1$ols
# m1.iis <- dist1$iis
#
# coef.index <- c(1, 4, 7, 10)
# se.index <- coef.index+1
# nround <- 5
#
# sum.index <- c(13, 14, 15, 17)
#
#
# stars.ols <-rep("", length(rel.coef))
# stars.ols[ m1.ols$mean.results[rel.coef, "p-value"] < 0.05] <- "*"
# stars.ols[ m1.ols$mean.results[rel.coef, "p-value"] < 0.01] <- "**"
#
# stars.iis <-rep("", length(rel.coef))
# stars.iis[ m1.iis$mean.results[rel.coef, "p-value"] < 0.05] <- "*"
# stars.iis[ m1.iis$mean.results[rel.coef, "p-value"] < 0.01] <- "**"
#
#
# res.tab <- data.frame(matrix(NA, nrow=18, ncol=4))
# names(res.tab) <- c("Var", "OLS", "IIS", "Diff")
# res.tab$Var[coef.index] <- rel.coef
# res.tab$OLS[coef.index] <- paste(round(coefficients(m1.ols)[rel.coef], nround), stars.ols, sep="")
# res.tab$IIS[coef.index] <- paste(round(coefficients(m1.iis)[rel.coef], nround), stars.iis, sep="")
# res.tab$Diff[coef.index] <- round(coefficients(m1.iis)[rel.coef]-coefficients(m1.ols)[rel.coef], nround)
#
# res.tab$OLS[se.index] <- paste("(", round(m1.ols$mean.results[rel.coef, "std.error"], nround), ")", sep="")
# res.tab$IIS[se.index] <- paste("(", round(m1.iis$mean.results[rel.coef, "std.error"], nround) , ")", sep="")
#
# res.tab$Var[sum.index] <- c("n", "L", "nOutl", "Dist")
# res.tab$OLS[sum.index[1:3]] <- c(length(m1.ols$aux$y), round(logLik(m1.ols), 3), "")
# res.tab$IIS[sum.index[1:3]] <- c(length(m1.iis$aux$y), round(logLik(m1.iis), 3), length(m1.iis$ISnames))
# res.tab$Diff[sum.index[4]] <- paste( round(dist1$statistic, 3), " (df=", NROW(dist1$coef.diff) , ")", sep="")
# res.tab$Diff[sum.index[4]+1] <- paste("[p=", round(dist1$p.value, 4), "]", sep="")
#
# res.tab[is.na(res.tab)] <- ""
# names(res.tab) <- c("", "OLS", "IIS", "Diff")
#
# str(res.tab)
#
# print(xtable(res.tab, type = "latex"),
#       file = here("data-raw","projections","out","results1.tex"), include.rownames=FALSE)


########## Manually Create a results table for lag model
# Commented out my Moritz because done in a different way
# m1.ols.lag <- dist1.lag$ols
# m1.iis.lag <- dist1.lag$iis
#
# coef.index <- c(1, 4, 7, 10)
# se.index <- coef.index+1
# nround <- 5
#
# sum.index <- c(13, 14, 15, 17)
#
#
# stars.ols <-rep("", length(rel.coef.lag))
# stars.ols[ m1.ols.lag$mean.results[rel.coef.lag, "p-value"] < 0.05] <- "*"
# stars.ols[ m1.ols.lag$mean.results[rel.coef.lag, "p-value"] < 0.01] <- "**"
#
# stars.iis <-rep("", length(rel.coef.lag))
# stars.iis[ m1.iis.lag$mean.results[rel.coef.lag, "p-value"] < 0.05] <- "*"
# stars.iis[ m1.iis.lag$mean.results[rel.coef.lag, "p-value"] < 0.01] <- "**"
#
#
# res.tab.lag <- data.frame(matrix(NA, nrow=18, ncol=4))
# names(res.tab.lag) <- c("Var", "OLS", "IIS", "Diff")
# res.tab.lag$Var[coef.index] <- rel.coef
# res.tab.lag$OLS[coef.index] <- paste(round(coefficients(m1.ols.lag)[rel.coef.lag], nround), stars.ols, sep="")
# res.tab.lag$IIS[coef.index] <- paste(round(coefficients(m1.iis.lag)[rel.coef.lag], nround), stars.iis, sep="")
# res.tab.lag$Diff[coef.index] <- round(coefficients(m1.iis.lag)[rel.coef.lag]-coefficients(m1.ols.lag)[rel.coef.lag], nround)
#
# #temp_L1
# #temp2_L1.loggdp
#
# res.tab.lag$OLS[se.index] <- paste("(", round(m1.ols.lag$mean.results[rel.coef.lag, "std.error"], nround), ")", sep="")
# res.tab.lag$IIS[se.index] <- paste("(", round(m1.iis.lag$mean.results[rel.coef.lag, "std.error"], nround) , ")", sep="")
#
# res.tab.lag$Var[sum.index] <- c("n", "L", "nOutl", "Dist")
# res.tab.lag$OLS[sum.index[1:3]] <- c(length(m1.ols.lag$aux$y), round(logLik(m1.ols.lag), 3), "")
# res.tab.lag$IIS[sum.index[1:3]] <- c(length(m1.iis.lag$aux$y), round(logLik(m1.iis.lag), 3), length(m1.iis.lag$ISnames))
# res.tab.lag$Diff[sum.index[4]] <- paste( round(dist1.lag$statistic, 3), " (df=", NROW(dist1.lag$coef.diff) , ")", sep="")
# res.tab.lag$Diff[sum.index[4]+1] <- paste("[p=", round(dist1.lag$p.value, 4), "]", sep="")
#
# res.tab.lag[is.na(res.tab.lag)] <- ""
# names(res.tab.lag) <- c("", "OLS", "IIS", "Diff")
#
# str(res.tab.lag)
#
#
# print(xtable(res.tab.lag, type = "latex"),
#       file = here("data-raw","projections","out","results1_lag.tex"), include.rownames=FALSE)
#



############# Recover Years and Countries for Plotting ###############
#ctry_index.com$is.index <- seq(1:NROW(ctry_index.com)) # changed moritz

ctry_index.com <- m2_data

ctry_index.com$is.index <- seq(1:nrow(m2.isat$aux$mX))
ctry_index.com$iis <- 0
ctry_index.com$iis_sign <- 0

ctry_index.com.adapt <- am2_data
ctry_index.com.adapt$is.index <- seq(1:NROW(ctry_index.com.adapt))
ctry_index.com.adapt$iis <- 0
ctry_index.com.adapt$iis_sign <- 0

ctry_index.com.adapt.L1 <- am2_L1_data
ctry_index.com.adapt.L1$is.index <- seq(1:NROW(ctry_index.com.adapt.L1))
ctry_index.com.adapt.L1$iis <- 0
ctry_index.com.adapt.L1$iis_sign <- 0


isdat1 <- isatdates(m2.isat)
ctry_index.com$iis[ctry_index.com$is.index %in% isdat1$iis$index] <- 1

ctry_index.com$iis_sign[ctry_index.com$is.index %in% isdat1$iis$index] <- 1
ctry_index.com$iis_sign[isdat1$iis$index] <- isdat1$iis$coef/abs(isdat1$iis$coef)

ctry_index.com$iis_lev <- 0
ctry_index.com$iis_lev[ctry_index.com$is.index %in% isdat1$iis$index] <- 1
ctry_index.com$iis_lev[isdat1$iis$index] <- isdat1$iis$coef


isdat1.adapt <- isatdates(am2.isat)
ctry_index.com.adapt$iis[ctry_index.com.adapt$is.index %in% isdat1.adapt$iis$index] <- 1

ctry_index.com.adapt$iis_sign[ctry_index.com.adapt$is.index %in% isdat1$iis$index] <- 1
ctry_index.com.adapt$iis_sign[isdat1.adapt$iis$index] <- isdat1.adapt$iis$coef/abs(isdat1.adapt$iis$coef)

ctry_index.com.adapt$iis_lev <- 0
ctry_index.com.adapt$iis_lev[ctry_index.com.adapt$is.index %in% isdat1.adapt$iis$index] <- 1
ctry_index.com.adapt$iis_lev[isdat1.adapt$iis$index] <- isdat1.adapt$iis$coef


isdat1.adapt.L1 <- isatdates(am2.isat_L1)
ctry_index.com.adapt.L1$iis[ctry_index.com.adapt.L1$is.index %in% isdat1.adapt.L1$iis$index] <- 1

ctry_index.com.adapt.L1$iis_sign[ctry_index.com.adapt.L1$is.index %in% isdat1$iis$index] <- 1
ctry_index.com.adapt.L1$iis_sign[isdat1.adapt.L1$iis$index] <- isdat1.adapt.L1$iis$coef/abs(isdat1.adapt.L1$iis$coef)

ctry_index.com.adapt.L1$iis_lev <- 0
ctry_index.com.adapt.L1$iis_lev[ctry_index.com.adapt.L1$is.index %in% isdat1.adapt.L1$iis$index] <- 1
ctry_index.com.adapt.L1$iis_lev[isdat1.adapt.L1$iis$index] <- isdat1.adapt.L1$iis$coef


######

ctry_outl <- aggregate(ctry_index.com$iis, by=list(ctry_index.com$iso), FUN=sum)
names(ctry_outl) <- c("iso", "outl")
year_outl <- aggregate(ctry_index.com$iis, by=list(ctry_index.com$year), FUN=sum)
names(year_outl) <- c("year", "outl")

ctry_outl.adapt <- aggregate(ctry_index.com.adapt$iis, by=list(ctry_index.com.adapt$iso), FUN=sum)
names(ctry_outl.adapt) <- c("iso", "outl")
year_outl.adapt <- aggregate(ctry_index.com.adapt$iis, by=list(ctry_index.com.adapt$year), FUN=sum)
names(year_outl.adapt) <- c("year", "outl")


ctry_outl.adapt.L1 <- aggregate(ctry_index.com.adapt.L1$iis, by=list(ctry_index.com.adapt.L1$iso), FUN=sum)
names(ctry_outl.adapt.L1) <- c("iso", "outl")
year_outl.adapt.L1 <- aggregate(ctry_index.com.adapt.L1$iis, by=list(ctry_index.com.adapt.L1$year), FUN=sum)
names(year_outl.adapt.L1) <- c("year", "outl")

###compute by year but negative and positve
ctry_index.com$iis_pos <- 0
ctry_index.com$iis_neg <- 0
ctry_index.com$iis_pos[ctry_index.com$iis_sign>0] <- 1
ctry_index.com$iis_neg[ctry_index.com$iis_sign<0] <- 1

year_outl_pos <- aggregate(ctry_index.com$iis_pos, by=list(ctry_index.com$year), FUN=sum)
names(year_outl_pos) <- c("year", "outl_pos")

year_outl_neg <- aggregate(ctry_index.com$iis_neg, by=list(ctry_index.com$year), FUN=sum)
names(year_outl_neg) <- c("year", "outl_neg")

##
ctry_index.com.adapt$iis_pos <- 0
ctry_index.com.adapt$iis_neg <- 0
ctry_index.com.adapt$iis_pos[ctry_index.com.adapt$iis_sign>0] <- 1
ctry_index.com.adapt$iis_neg[ctry_index.com.adapt$iis_sign<0] <- 1

year_outl_pos.adapt <- aggregate(ctry_index.com.adapt$iis_pos, by=list(ctry_index.com.adapt$year), FUN=sum)
names(year_outl_pos.adapt) <- c("year", "outl_pos")

year_outl_neg.adapt <- aggregate(ctry_index.com.adapt$iis_neg, by=list(ctry_index.com.adapt$year), FUN=sum)
names(year_outl_neg.adapt) <- c("year", "outl_neg")

##
ctry_index.com.adapt.L1$iis_pos <- 0
ctry_index.com.adapt.L1$iis_neg <- 0
ctry_index.com.adapt.L1$iis_pos[ctry_index.com.adapt.L1$iis_sign>0] <- 1
ctry_index.com.adapt.L1$iis_neg[ctry_index.com.adapt.L1$iis_sign<0] <- 1

year_outl_pos.adapt.L1 <- aggregate(ctry_index.com.adapt.L1$iis_pos, by=list(ctry_index.com.adapt.L1$year), FUN=sum)
names(year_outl_pos.adapt.L1) <- c("year", "outl_pos")

year_outl_neg.adapt.L1 <- aggregate(ctry_index.com.adapt.L1$iis_neg, by=list(ctry_index.com.adapt.L1$year), FUN=sum)
names(year_outl_neg.adapt.L1) <- c("year", "outl_neg")



#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/year_hist.pdf", height=7, width=8)
pdf(here("data-raw/projections/out/year_hist.pdf"), height=7, width=8)

par(mfrow=c(1,1))
plot(year_outl$year,year_outl$outl, type="h", lwd=3, col="gray75", ylab="#Outlying Countries", xlab="Year")

dev.off()


#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/year_hist_lag.pdf", height=7, width=8)
pdf(here("data-raw/projections/out/year_hist_adapt.pdf"), height=7, width=8)
par(mfrow=c(1,1))
plot(year_outl.adapt$year,year_outl.adapt$outl, type="h", lwd=3, col="gray75", ylab="#Outlying Countries", xlab="Year")

dev.off()


pdf(here("data-raw/projections/out/year_hist_adapt.L1.pdf"), height=7, width=8)
par(mfrow=c(1,1))
plot(year_outl.adapt.L1$year,year_outl.adapt.L1$outl, type="h", lwd=3, col="gray75", ylab="#Outlying Countries", xlab="Year")

dev.off()


#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/year_hist_sign.pdf", height=5, width=11)
pdf(here("data-raw/projections/out/year_hist_sign.pdf"), height=5, width=11)
par(mfrow=c(1,1))
plot(year_outl$year, year_outl_neg$outl_neg*(-1), type="h", ylim=c(-12, 12), xlim=c(1963, 2012), lwd=3, col="blue", yaxt="n", ylab="#Outlying Countries", xlab="Year")
lines(year_outl$year, year_outl_pos$outl_pos, type="h", lwd=3, col="red")
axis(2, at=c(-10, -5, 0, 5), labels=c("10 (Neg.)", "5 (Neg.)", "0", "5 (Pos.)"))
abline(h=0, lty=2, col="gray35")
abline(h=c(-10, -5, 5), lty=3, col="gray75")

dev.off()



#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/year_hist_sign_lag.pdf", height=5, width=11)
pdf(here("data-raw/projections/out/year_hist_sign.adapt.pdf"), height=5, width=11)
par(mfrow=c(1,1))
plot(year_outl.adapt$year, year_outl_neg.adapt$outl_neg*(-1), type="h", ylim=c(-12, 12), xlim=c(1963, 2012), lwd=3, col="blue", yaxt="n", ylab="#Outlying Countries", xlab="Year")
lines(year_outl.adapt$year, year_outl_pos.adapt$outl_pos, type="h", lwd=3, col="red")
axis(2, at=c(-10, -5, 0, 5), labels=c("10 (Neg.)", "5 (Neg.)", "0", "5 (Pos.)"))
abline(h=0, lty=2, col="gray35")
abline(h=c(-10, -5, 5), lty=3, col="gray75")

dev.off()


#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/year_hist_sign.adapt.L1.pdf", height=5, width=11)
pdf(here("data-raw/projections/out/year_hist_sign.adapt.L1.pdf"), height=5, width=11)
par(mfrow=c(1,1))
plot(year_outl.adapt.L1$year, year_outl_neg.adapt.L1$outl_neg*(-1), type="h", ylim=c(-12, 12), xlim=c(1963, 2012), lwd=3, col="blue", yaxt="n", ylab="#Outlying Countries", xlab="Year")
lines(year_outl.adapt.L1$year, year_outl_pos.adapt.L1$outl_pos, type="h", lwd=3, col="red")
axis(2, at=c(-10, -5, 0, 5), labels=c("10 (Neg.)", "5 (Neg.)", "0", "5 (Pos.)"))
abline(h=0, lty=2, col="gray35")
abline(h=c(-10, -5, 5), lty=3, col="gray75")

dev.off()

#install.packages("rworldmap")
library(rworldmap)

max(ctry_outl$outl)
max(ctry_outl.adapt$outl)
max(ctry_outl.adapt.L1$outl)

#join to a coarse resolution map
cols <- c("gray85",
          "#fdd49e",
          "#fdbb84",
          "#fc8d59",
          "#ef6548",
          "#d7301f",
          "#b30000",
          "#7f0000")

col_fun <- colorRampPalette(cols)

spdf <- joinCountryData2Map(ctry_outl, joinCode="ISO3", nameJoinColumn="iso")
spdf.adapt <- joinCountryData2Map(ctry_outl.adapt, joinCode="ISO3", nameJoinColumn="iso")
spdf.adapt.L1 <- joinCountryData2Map(ctry_outl.adapt.L1, joinCode="ISO3", nameJoinColumn="iso")

#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/ctry_map.pdf", height=10, width=11)
pdf(here("data-raw/projections/out/ctry_map.pdf"), height=4, width=7)

scale <- 0:max(unique(spdf$outl)[!is.na(unique(spdf$outl))])
cur_cols <- col_fun(length(0:max(unique(spdf$outl)[!is.na(unique(spdf$outl))]))-1)

map1 <- mapCountryData(spdf[spdf$SOV_A3 != "ATA",],
                       nameColumnToPlot="outl",
                       addLegend=FALSE,
                       #catMethod=c(0,  2, 4, 6, 8, 10, 12),
                       catMethod=scale,
                       colourPalette=cur_cols,
                       mapTitle = "")

do.call(addMapLegend
        ,c(map1
           ,legendLabels="all"
           ,horizontal=TRUE
           ,legendWidth=0.5
           ,legendIntervals="data"
           , legendMar = 4))


dev.off()





#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/ctry_map_lag.pdf", height=10, width=11)
pdf(here("data-raw/projections/out/ctry_map_adapt.pdf"), height=4, width=7)

scale <- 0:max(unique(spdf.adapt$outl)[!is.na(unique(spdf.adapt$outl))])
cur_cols <- col_fun(length(0:max(unique(spdf.adapt$outl)[!is.na(unique(spdf.adapt$outl))]))-1)

map1 <- mapCountryData(spdf.adapt[spdf.adapt$SOV_A3 != "ATA",],
                       nameColumnToPlot="outl",
                       addLegend=FALSE,
                       #catMethod=c(0,  2, 4, 6, 8, 10, 12),
                       catMethod=scale,
                       colourPalette=cur_cols,
                       mapTitle = "")
do.call(addMapLegend
        ,c(map1
           ,legendLabels="all"
           ,horizontal=TRUE
           ,legendWidth=0.5
           ,legendIntervals="data"
           , legendMar = 4))


dev.off()


pdf(here("data-raw/projections/out/ctry_map_adapt.L1.pdf"), height=4, width=7)

scale <- 0:max(unique(spdf.adapt.L1$outl)[!is.na(unique(spdf.adapt.L1$outl))])
cur_cols <- col_fun(length(0:max(unique(spdf.adapt.L1$outl)[!is.na(unique(spdf.adapt.L1$outl))]))-1)

map1 <- mapCountryData(spdf.adapt.L1[spdf.adapt.L1$SOV_A3 != "ATA",],
                       nameColumnToPlot="outl",
                       addLegend=FALSE,
                       #catMethod=c(0,  2, 4, 6, 8, 10, 12),
                       catMethod=scale,
                       colourPalette=cur_cols,
                       mapTitle = "")
do.call(addMapLegend
        ,c(map1
           ,legendLabels="all"
           ,horizontal=TRUE
           ,legendWidth=0.5
           ,legendIntervals="data"
           , legendMar = 4))


dev.off()

#install.packages("countrycode")
library(countrycode)

ctry_index.com$continent <- countrycode(sourcevar = ctry_index.com$iso,
                            origin = "iso3c",
                            destination = "continent")

# ctry_index.com$continent[ctry_index.com$iso=="ZAR"] <- "Africa"
# ctry_index.com$continent[ctry_index.com$iso=="ROM"] <- "Europe"

ctry_index.com.adapt$continent <- countrycode(sourcevar = ctry_index.com.adapt$iso,
                                        origin = "iso3c",
                                        destination = "continent")

# ctry_index.com.adapt$continent[ctry_index.com.adapt$iso=="ZAR"] <- "Africa"
# ctry_index.com.adapt$continent[ctry_index.com.adapt$iso=="ROM"] <- "Europe"


ctry_index.com.adapt.L1$continent <- countrycode(sourcevar = ctry_index.com.adapt.L1$iso,
                                              origin = "iso3c",
                                              destination = "continent")

# ctry_index.com.adapt.L1$continent[ctry_index.com.adapt.L1$iso=="ZAR"] <- "Africa"
# ctry_index.com.adapt.L1$continent[ctry_index.com.adapt.L1$iso=="ROM"] <- "Europe"

##################### Heatmap no lag ################

#install.packages("reshape2")
library(reshape2)

#heatmap(data, Colv = NA, Rowv = NA, scale="column")
ctry_index_wide <- dcast(ctry_index.com, iso + continent ~ year, value.var="iis_sign")
ctry_index_wide

ctry_index_wide_lev <- dcast(ctry_index.com, iso + continent ~ year, value.var="iis_lev")
ctry_index_wide_lev

ctry_index_wide.ord <- ctry_index_wide[order(ctry_index_wide$continent),]
ctry_index_wide.ord_lev <- ctry_index_wide_lev[order(ctry_index_wide_lev$continent),]


ctry_index_wide.ord$index <- seq(1:NROW(ctry_index_wide.ord))
ctry_index_wide.ord_lev$index <- seq(1:NROW(ctry_index_wide.ord_lev))


ctry_index_wide.mat <- as.matrix(ctry_index_wide.ord[,3:NCOL(ctry_index_wide)])
row.names(ctry_index_wide.mat) <- ctry_index_wide.ord$iso

ctry_index_wide.mat_lev <- as.matrix(ctry_index_wide.ord_lev[,3:NCOL(ctry_index_wide_lev)])
row.names(ctry_index_wide.mat_lev) <- ctry_index_wide.ord_lev$iso


heatmap(ctry_index_wide.mat, scale="none", Rowv=NA, Colv = NA, col=c("blue", "gray85",  "red"))

# library(devtools)
#install.packages("devtools")

#install_github("jokergoo/ComplexHeatmap")
# https://jokergoo.github.io/ComplexHeatmap-reference/book/

#install.packages("S4Vectors")
# BiocInstaller:biocLite("S4Vectors")# to re-install S4Vectors.
#
# Use library(DESeq2)

#install.packages("digest")
#remove.packages("digest")

library(ComplexHeatmap)


colors_lev = c("blue", "gray78", "red")
colors = structure(c("blue", "gray78", "red"), names = c("-1", "0", "1"))

mode(ctry_index_wide.mat) = "character"

#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/heat1.pdf", height=10, width=8)
pdf(here("data-raw/projections/out/heat1.pdf"), height=10, width=8)

Heatmap(ctry_index_wide.mat,
        row_split = ctry_index_wide.ord$continent,
        column_order =  sort(colnames(ctry_index_wide.mat)),
        cluster_rows = FALSE,
        col=colors, na_col="white",
        row_names_gp = gpar(fontsize = 4),
        row_gap = unit(3, "mm"),
        border = TRUE,
        rect_gp = gpar(col = "white", lwd = 1.3),
        column_names_gp = gpar(fontsize = 7),
        name = "Outliers",
        column_title = "Detected Outliers (IIS, p=0.01)")

dev.off()

Heatmap(ctry_index_wide.mat_lev,
        row_split = ctry_index_wide.ord_lev$continent,
        column_order =  sort(colnames(ctry_index_wide.mat_lev)),
        cluster_rows = FALSE,
        col=colors_lev, na_col="white",
        row_names_gp = gpar(fontsize = 4),
        row_gap = unit(3, "mm"),
        border = TRUE,
        rect_gp = gpar(col = "white", lwd = 0.8),
        column_names_gp = gpar(fontsize = 7),
        name = "Outliers")



##################### Heatmap Adapt ################

#install.packages("reshape2")
library(reshape2)

#heatmap(data, Colv = NA, Rowv = NA, scale="column")
ctry_index_wide.adapt <- dcast(ctry_index.com.adapt, iso + continent ~ year, value.var="iis_sign")
ctry_index_wide.adapt

ctry_index_wide_lev.adapt <- dcast(ctry_index.com.adapt, iso + continent ~ year, value.var="iis_lev")
ctry_index_wide_lev.adapt

ctry_index_wide.ord.adapt <- ctry_index_wide.adapt[order(ctry_index_wide.adapt$continent),]
ctry_index_wide.ord_lev.adapt <- ctry_index_wide_lev.adapt[order(ctry_index_wide_lev.adapt$continent),]
#ctry_index_wide$`2007`

ctry_index_wide.ord.adapt$index <- seq(1:NROW(ctry_index_wide.ord.adapt))
ctry_index_wide.ord_lev.adapt$index <- seq(1:NROW(ctry_index_wide.ord_lev.adapt))


ctry_index_wide.mat.adapt <- as.matrix(ctry_index_wide.ord.adapt[,3:NCOL(ctry_index_wide.adapt)])
row.names(ctry_index_wide.mat.adapt) <- ctry_index_wide.ord.adapt$iso

ctry_index_wide.mat_lev.adapt <- as.matrix(ctry_index_wide.ord_lev.adapt[,3:NCOL(ctry_index_wide_lev.adapt)])
row.names(ctry_index_wide.mat_lev.adapt) <- ctry_index_wide.ord_lev.adapt$iso


heatmap(ctry_index_wide.mat.adapt, scale="none", Rowv=NA, Colv = NA, col=c("blue", "gray85",  "red"))






colors_lev = c("blue", "gray78", "red")
colors = structure(c("blue", "gray78", "red"), names = c("-1", "0", "1"))

mode(ctry_index_wide.mat.adapt) = "character"

#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/heat1_lag.pdf", height=10, width=8)
pdf(here("data-raw/projections/out/heat1_adapt.pdf"), height=10, width=8)


Heatmap(ctry_index_wide.mat.adapt,
        row_split = ctry_index_wide.ord.adapt$continent,
        column_order =  sort(colnames(ctry_index_wide.mat.adapt)),
        cluster_rows = FALSE,
        col=colors, na_col="white",
        row_names_gp = gpar(fontsize = 4),
        row_gap = unit(3, "mm"),
        border = TRUE,
        rect_gp = gpar(col = "white", lwd = 1.3),
        column_names_gp = gpar(fontsize = 7),
        name = "Outliers",
        column_title = "Detected Outliers (IIS, p=0.01)")

dev.off()


##################### Heatmap Adapt Lag ################

#install.packages("reshape2")
library(reshape2)

#heatmap(data, Colv = NA, Rowv = NA, scale="column")
ctry_index_wide.adapt.L1 <- dcast(ctry_index.com.adapt.L1, iso + continent ~ year, value.var="iis_sign")
ctry_index_wide.adapt.L1

ctry_index_wide_lev.adapt.L1 <- dcast(ctry_index.com.adapt.L1, iso + continent ~ year, value.var="iis_lev")
ctry_index_wide_lev.adapt.L1

ctry_index_wide.ord.adapt.L1 <- ctry_index_wide.adapt.L1[order(ctry_index_wide.adapt.L1$continent),]
ctry_index_wide.ord_lev.adapt.L1 <- ctry_index_wide_lev.adapt.L1[order(ctry_index_wide_lev.adapt.L1$continent),]
#ctry_index_wide$`2007`

ctry_index_wide.ord.adapt.L1$index <- seq(1:NROW(ctry_index_wide.ord.adapt.L1))
ctry_index_wide.ord_lev.adapt.L1$index <- seq(1:NROW(ctry_index_wide.ord_lev.adapt.L1))


ctry_index_wide.mat.adapt.L1 <- as.matrix(ctry_index_wide.ord.adapt.L1[,3:NCOL(ctry_index_wide.adapt.L1)])
row.names(ctry_index_wide.mat.adapt.L1) <- ctry_index_wide.ord.adapt.L1$iso

ctry_index_wide.mat_lev.adapt.L1 <- as.matrix(ctry_index_wide.ord_lev.adapt.L1[,3:NCOL(ctry_index_wide_lev.adapt.L1)])
row.names(ctry_index_wide.mat_lev.adapt.L1) <- ctry_index_wide.ord_lev.adapt.L1$iso


heatmap(ctry_index_wide.mat.adapt.L1, scale="none", Rowv=NA, Colv = NA, col=c("blue", "gray85",  "red"))






colors_lev = c("blue", "gray78", "red")
colors = structure(c("blue", "gray78", "red"), names = c("-1", "0", "1"))

mode(ctry_index_wide.mat.adapt.L1) = "character"

#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/heat1_lag.pdf", height=10, width=8)
pdf(here("data-raw/projections/out/heat1_adapt.L1.pdf"), height=10, width=8)


Heatmap(ctry_index_wide.mat.adapt.L1,
        row_split = ctry_index_wide.ord.adapt.L1$continent,
        column_order =  sort(colnames(ctry_index_wide.mat.adapt.L1)),
        cluster_rows = FALSE,
        col=colors, na_col="white",
        row_names_gp = gpar(fontsize = 4),
        row_gap = unit(3, "mm"),
        border = TRUE,
        rect_gp = gpar(col = "white", lwd = 1.3),
        column_names_gp = gpar(fontsize = 7),
        name = "Outliers",
        column_title = "Detected Outliers (IIS, p=0.01)")

dev.off()


##### Compare Projected Impacts

coef_m2 <- coefficients(m2)
coef_m2.isat <- coefficients(m2.isat)
coef_am2.adapt <- coefficients(am2)
coef_am2.adapt.L1 <- coefficients(am2_L1)
coef_am2.isat.adapt <- coefficients(am2.isat)
coef_am2.isat.adapt.L1 <- coefficients(am2.isat_L1)

#coef_m2.lag <- coefficients(m1.arx.lag)
#coef_m2.lag <- coefficients(dist1.lag$ols)[rel.coef.lag]

qlow <- 0.25
qmed <- 0.5
qup <- 0.75

temp <- am2_data$temp
ln_gdp_pc <- am2_data$ln_gdp_cap

loginc_lower <- quantile(ln_gdp_pc, probs=qlow, na.rm=TRUE)
loginc_med <- quantile(ln_gdp_pc, probs=qmed, na.rm=TRUE)
loginc_upper <- quantile(ln_gdp_pc, probs=qup, na.rm=TRUE)


#quantile(log(L1.gdp_pc), probs=qlow, na.rm=TRUE)

temp_lower <- temp[ln_gdp_pc < loginc_lower]

temp_mid <- temp[ loginc_lower < ln_gdp_pc & ln_gdp_pc < loginc_upper]

temp_upper <- temp[ ln_gdp_pc > loginc_upper]



temp_seq <- seq(-5, 30, 1)

# imp_lower <- (coef_m2['temp']+coef_m2['temp_int']*loginc_lower)*temp_seq + (coef_m2['temp_2']+coef_m2['temp_2_int']*loginc_lower)*temp_seq^2
imp_med <- coef_m2['temp']*temp_seq + coef_m2['temp_2']*temp_seq^2
# imp_upper <- (coef_m2['temp']+coef_m2['temp_int']*loginc_upper)*temp_seq + (coef_m2['temp_2']+coef_m2['temp_2_int']*loginc_upper)*temp_seq^2

imp_lower.adapt <- (coef_am2.adapt['temp']+coef_am2.adapt['temp_int']*loginc_lower)*temp_seq + (coef_am2.adapt['temp_2']+coef_am2.adapt['temp_2_int']*loginc_lower)*temp_seq^2
imp_med.adapt <- (coef_am2.adapt['temp']+coef_am2.adapt['temp_int']*loginc_med)*temp_seq + (coef_am2.adapt['temp_2']+coef_am2.adapt['temp_2_int']*loginc_med)*temp_seq^2
imp_upper.adapt <- (coef_am2.adapt['temp']+coef_am2.adapt['temp_int']*loginc_upper)*temp_seq + (coef_am2.adapt['temp_2']+coef_am2.adapt['temp_2_int']*loginc_upper)*temp_seq^2

imp_lower.adapt.L1 <- (coef_am2.adapt.L1['temp']+coef_am2.adapt.L1['temp_int']*loginc_lower)*temp_seq + (coef_am2.adapt.L1['temp_2']+coef_am2.adapt.L1['temp_2_int']*loginc_lower)*temp_seq^2
imp_med.adapt.L1 <- (coef_am2.adapt.L1['temp']+coef_am2.adapt.L1['temp_int']*loginc_med)*temp_seq + (coef_am2.adapt.L1['temp_2']+coef_am2.adapt.L1['temp_2_int']*loginc_med)*temp_seq^2
imp_upper.adapt.L1 <- (coef_am2.adapt.L1['temp']+coef_am2.adapt.L1['temp_int']*loginc_upper)*temp_seq + (coef_am2.adapt.L1['temp_2']+coef_am2.adapt.L1['temp_2_int']*loginc_upper)*temp_seq^2



# imp_lower_isat <- (coef_m2.isat['temp']+coef_m2.isat['temp_int']*loginc_lower)*temp_seq + (coef_m2.isat['temp_2']+coef_m2.isat['temp_2_int']*loginc_lower)*temp_seq^2
imp_med_isat <- coef_m2.isat['temp']*temp_seq + coef_m2.isat['temp_2']*temp_seq^2
# imp_upper_isat <- (coef_m2.isat['temp']+coef_m2.isat['temp_int']*loginc_upper)*temp_seq + (coef_m2.isat['temp_2']+coef_m2.isat['temp_2_int']*loginc_upper)*temp_seq^2

imp_lower_isat.adapt <- (coef_am2.isat.adapt['temp']+coef_am2.isat.adapt['temp_int']*loginc_lower)*temp_seq + (coef_am2.isat.adapt['temp_2']+coef_am2.isat.adapt['temp_2_int']*loginc_lower)*temp_seq^2
imp_med_isat.adapt <- (coef_am2.isat.adapt['temp']+coef_am2.isat.adapt['temp_int']*loginc_med)*temp_seq + (coef_am2.isat.adapt['temp_2']+coef_am2.isat.adapt['temp_2_int']*loginc_med)*temp_seq^2
imp_upper_isat.adapt <- (coef_am2.isat.adapt['temp']+coef_am2.isat.adapt['temp_int']*loginc_upper)*temp_seq + (coef_am2.isat.adapt['temp_2']+coef_am2.isat.adapt['temp_2_int']*loginc_upper)*temp_seq^2


imp_lower_isat.adapt.L1 <- (coef_am2.isat.adapt.L1['temp']+coef_am2.isat.adapt.L1['temp_int']*loginc_lower)*temp_seq + (coef_am2.isat.adapt.L1['temp_2']+coef_am2.isat.adapt.L1['temp_2_int']*loginc_lower)*temp_seq^2
imp_med_isat.adapt.L1 <- (coef_am2.isat.adapt.L1['temp']+coef_am2.isat.adapt.L1['temp_int']*loginc_med)*temp_seq + (coef_am2.isat.adapt.L1['temp_2']+coef_am2.isat.adapt.L1['temp_2_int']*loginc_med)*temp_seq^2
imp_upper_isat.adapt.L1 <- (coef_am2.isat.adapt.L1['temp']+coef_am2.isat.adapt.L1['temp_int']*loginc_upper)*temp_seq + (coef_am2.isat.adapt.L1['temp_2']+coef_am2.isat.adapt.L1['temp_2_int']*loginc_upper)*temp_seq^2



# imp_lower_sc <- imp_lower - max(imp_lower)
imp_med_sc <- imp_med - max(imp_med)
# imp_upper_sc <- imp_upper - max(imp_upper)

imp_lower_sc.adapt <- imp_lower.adapt - max(imp_lower.adapt)
imp_med_sc.adapt <- imp_med.adapt - max(imp_med.adapt)
imp_upper_sc.adapt <- imp_upper.adapt - max(imp_upper.adapt)

imp_lower_sc.adapt.L1 <- imp_lower.adapt.L1 - max(imp_lower.adapt.L1)
imp_med_sc.adapt.L1 <- imp_med.adapt.L1 - max(imp_med.adapt.L1)
imp_upper_sc.adapt.L1 <- imp_upper.adapt.L1 - max(imp_upper.adapt.L1)


# imp_lower_sc_isat <- imp_lower_isat - max(imp_lower_isat)
imp_med_sc_isat <- imp_med_isat - max(imp_med_isat)
# imp_upper_sc_isat <- imp_upper_isat - max(imp_upper_isat)

imp_lower_sc_isat.adapt <- imp_lower_isat.adapt - max(imp_lower_isat.adapt)
imp_med_sc_isat.adapt <- imp_med_isat.adapt - max(imp_med_isat.adapt)
imp_upper_sc_isat.adapt <- imp_upper_isat.adapt - max(imp_upper_isat.adapt)


imp_lower_sc_isat.adapt.L1 <- imp_lower_isat.adapt.L1 - max(imp_lower_isat.adapt.L1)
imp_med_sc_isat.adapt.L1 <- imp_med_isat.adapt.L1 - max(imp_med_isat.adapt.L1)
imp_upper_sc_isat.adapt.L1 <- imp_upper_isat.adapt.L1 - max(imp_upper_isat.adapt.L1)


#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/isat_adapt_impacts.pdf", height=8, width=12)
# pdf(here("data-raw/projections/out/isat_adapt_impacts.pdf"), height=8, width=12)
# par(mfrow=c(1,3))
#
# plot(temp_seq, imp_lower_sc, type="l", ylim=c(-0.3, 0.01), col="blue", main="Income-Based Adaptation (Lower)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
# lines(temp_seq, imp_upper_sc, col="gray75", lwd=1)
# lines(temp_seq, imp_med_sc, col="gray75", lwd=1)
#
# text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
# text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
# text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)
#
# lines(temp_seq, imp_lower_sc_isat, col="blue", lwd=5, lty=4)
# lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
# lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)
#
#
# plot(temp_seq, imp_med_sc, type="l", ylim=c(-0.3, 0.01), col="#238b45", main="Income-Based Adaptation (Middle)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
# lines(temp_seq, imp_upper_sc, col="gray75", lwd=1)
# lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
#
# text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
# text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
# text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)
#
# lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
# lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
# lines(temp_seq, imp_med_sc_isat, col="#238b45", lwd=5, lty=4)
#
# plot(temp_seq, imp_upper_sc, type="l", ylim=c(-0.3, 0.01), col="red", main="Income-Based Adaptation (Upper)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
# lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
# lines(temp_seq, imp_med_sc, col="gray75", lwd=1)
#
# text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
# text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
# text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)
#
# lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
# lines(temp_seq, imp_upper_sc_isat, col="red", lwd=5, lty=4)
# lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)
#
# dev.off()


#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/isat_adapt_impacts_lag.pdf", height=8, width=12)
pdf(here("data-raw/projections/out/isat_adapt_impacts_adapt.pdf"), height=8, width=12)

par(mfrow=c(1,3))

plot(temp_seq, imp_lower_sc.adapt, type="l", ylim=c(-0.3, 0.01), col="blue", main="Income-Based Adaptation (Lower)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
lines(temp_seq, imp_upper_sc.adapt, col="gray75", lwd=1)
lines(temp_seq, imp_med_sc.adapt, col="gray75", lwd=1)

text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

lines(temp_seq, imp_lower_sc_isat.adapt, col="blue", lwd=5, lty=4)
lines(temp_seq, imp_upper_sc_isat.adapt, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_med_sc_isat.adapt, col="gray75", lwd=1, lty=2)


plot(temp_seq, imp_med_sc.adapt, type="l", ylim=c(-0.3, 0.01), col="#238b45", main="Income-Based Adaptation (Middle)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
lines(temp_seq, imp_upper_sc.adapt, col="gray75", lwd=1)
lines(temp_seq, imp_lower_sc.adapt, col="gray75", lwd=1)

text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

lines(temp_seq, imp_lower_sc_isat.adapt, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat.adapt, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_med_sc_isat.adapt, col="#238b45", lwd=5, lty=4)

plot(temp_seq, imp_upper_sc.adapt, type="l", ylim=c(-0.3, 0.01), col="red", main="Income-Based Adaptation (Upper)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
lines(temp_seq, imp_lower_sc.adapt, col="gray75", lwd=1)
lines(temp_seq, imp_med_sc.adapt, col="gray75", lwd=1)

text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

lines(temp_seq, imp_lower_sc_isat.adapt, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat.adapt, col="red", lwd=5, lty=4)
lines(temp_seq, imp_med_sc_isat.adapt, col="gray75", lwd=1, lty=2)


dev.off()



pdf(here("data-raw/projections/out/isat_adapt_impacts_adapt.L1.pdf"), height=8, width=12)

par(mfrow=c(1,3))

plot(temp_seq, imp_lower_sc.adapt.L1, type="l", ylim=c(-0.3, 0.01), col="blue", main="Income-Based Adaptation (Lower)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
lines(temp_seq, imp_upper_sc.adapt.L1, col="gray75", lwd=1)
lines(temp_seq, imp_med_sc.adapt.L1, col="gray75", lwd=1)

text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

lines(temp_seq, imp_lower_sc_isat.adapt.L1, col="blue", lwd=5, lty=4)
lines(temp_seq, imp_upper_sc_isat.adapt.L1, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_med_sc_isat.adapt.L1, col="gray75", lwd=1, lty=2)


plot(temp_seq, imp_med_sc.adapt.L1, type="l", ylim=c(-0.3, 0.01), col="#238b45", main="Income-Based Adaptation (Middle)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
lines(temp_seq, imp_upper_sc.adapt.L1, col="gray75", lwd=1)
lines(temp_seq, imp_lower_sc.adapt.L1, col="gray75", lwd=1)

text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

lines(temp_seq, imp_lower_sc_isat.adapt.L1, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat.adapt.L1, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_med_sc_isat.adapt.L1, col="#238b45", lwd=5, lty=4)

plot(temp_seq, imp_upper_sc.adapt.L1, type="l", ylim=c(-0.3, 0.01), col="red", main="Income-Based Adaptation (Upper)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
lines(temp_seq, imp_lower_sc.adapt.L1, col="gray75", lwd=1)
lines(temp_seq, imp_med_sc.adapt.L1, col="gray75", lwd=1)

text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

lines(temp_seq, imp_lower_sc_isat.adapt.L1, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat.adapt.L1, col="red", lwd=5, lty=4)
lines(temp_seq, imp_med_sc_isat.adapt.L1, col="gray75", lwd=1, lty=2)


dev.off()


##########################################
####### Comparing Coefficients ##############

############## Plot Different Coefficients

iis.coef <- dist1$iis$mean.results[rel.coef,"coef"]
iis.se <- dist1$iis$mean.results[rel.coef,"std.error"]

iis.coef.adapt <- dist1.adapt$iis$mean.results[rel.coef.adapt,"coef"]
iis.se.adapt <- dist1.adapt$iis$mean.results[rel.coef.adapt,"std.error"]

iis.coef.adapt.L1 <- dist1.adapt.L1$iis$mean.results[rel.coef.adapt.L1,"coef"]
iis.se.adapt.L1 <- dist1.adapt.L1$iis$mean.results[rel.coef.adapt.L1,"std.error"]


ols.coef <- dist1$ols$mean.results[rel.coef,"coef"]
ols.se <- dist1$ols$mean.results[rel.coef,"std.error"]

ols.coef.adapt <- dist1.adapt$ols$mean.results[rel.coef.adapt,"coef"]
ols.se.adapt <- dist1.adapt$ols$mean.results[rel.coef.adapt,"std.error"]

ols.coef.adapt.L1 <- dist1.adapt.L1$ols$mean.results[rel.coef.adapt.L1,"coef"]
ols.se.adapt.L1 <- dist1.adapt.L1$ols$mean.results[rel.coef.adapt.L1,"std.error"]

coef.plot <- data.frame(matrix(NA, nrow=NROW(rel.coef.adapt)), ncol=2)
names(coef.plot) <- c("coef", "iis.coef")

coef.plot$coef <- rel.coef.adapt

# coef.plot$ols.coef <- ols.coef
# coef.plot$ols.se <- ols.se
# coef.plot$iis.coef <- iis.coef
# coef.plot$iis.se <- iis.se

coef.plot$iis.coef.adapt <- iis.coef.adapt
coef.plot$iis.se.adapt <- iis.se.adapt
coef.plot$ols.coef.adapt <- ols.coef.adapt
coef.plot$ols.se.adapt <- ols.se.adapt

coef.plot$iis.coef.adapt.L1 <- iis.coef.adapt.L1
coef.plot$iis.se.adapt.L1 <- iis.se.adapt.L1
coef.plot$ols.coef.adapt.L1 <- ols.coef.adapt.L1
coef.plot$ols.se.adapt.L1 <- ols.se.adapt.L1


# coef.plot$iis.ci.p025 <- coef.plot$iis.coef - 1.96*coef.plot$iis.se
# coef.plot$iis.ci.p975 <- coef.plot$iis.coef + 1.96*coef.plot$iis.se

coef.plot$iis.ci.p025.adapt <- coef.plot$iis.coef.adapt - 1.96*coef.plot$iis.se.adapt
coef.plot$iis.ci.p975.adapt <- coef.plot$iis.coef.adapt + 1.96*coef.plot$iis.se.adapt

coef.plot$iis.ci.p025.adapt.L1 <- coef.plot$iis.coef.adapt.L1 - 1.96*coef.plot$iis.se.adapt.L1
coef.plot$iis.ci.p975.adapt.L1 <- coef.plot$iis.coef.adapt.L1 + 1.96*coef.plot$iis.se.adapt.L1

# coef.plot$ols.ci.p025 <- coef.plot$ols.coef - 1.96*coef.plot$ols.se
# coef.plot$ols.ci.p975 <- coef.plot$ols.coef + 1.96*coef.plot$ols.se

coef.plot$ols.ci.p025.adapt <- coef.plot$ols.coef.adapt - 1.96*coef.plot$ols.se.adapt
coef.plot$ols.ci.p975.adapt <- coef.plot$ols.coef.adapt + 1.96*coef.plot$ols.se.adapt

coef.plot$ols.ci.p025.adapt.L1 <- coef.plot$ols.coef.adapt.L1 - 1.96*coef.plot$ols.se.adapt.L1
coef.plot$ols.ci.p975.adapt.L1 <- coef.plot$ols.coef.adapt.L1 + 1.96*coef.plot$ols.se.adapt.L1


# coef.plot$iis.ci.p005 <- coef.plot$iis.coef - 2.57*coef.plot$iis.se
# coef.plot$iis.ci.p995 <- coef.plot$iis.coef + 2.57*coef.plot$iis.se

coef.plot$iis.ci.p005.adapt <- coef.plot$iis.coef.adapt - 2.57*coef.plot$iis.se.adapt
coef.plot$iis.ci.p995.adapt <- coef.plot$iis.coef.adapt + 2.57*coef.plot$iis.se.adapt

coef.plot$iis.ci.p005.adapt.L1 <- coef.plot$iis.coef.adapt.L1 - 2.57*coef.plot$iis.se.adapt.L1
coef.plot$iis.ci.p995.adapt.L1 <- coef.plot$iis.coef.adapt.L1 + 2.57*coef.plot$iis.se.adapt.L1


# coef.plot$ols.ci.p005 <- coef.plot$ols.coef - 2.57*coef.plot$ols.se
# coef.plot$ols.ci.p995 <- coef.plot$ols.coef + 2.57*coef.plot$ols.se

coef.plot$ols.ci.p005.adapt <- coef.plot$ols.coef.adapt - 2.57*coef.plot$ols.se.adapt
coef.plot$ols.ci.p995.adapt <- coef.plot$ols.coef.adapt + 2.57*coef.plot$ols.se.adapt

coef.plot$ols.ci.p005.adapt.L1 <- coef.plot$ols.coef.adapt.L1 - 2.57*coef.plot$ols.se.adapt.L1
coef.plot$ols.ci.p995.adapt.L1 <- coef.plot$ols.coef.adapt.L1 + 2.57*coef.plot$ols.se.adapt.L1

coef.plot$plot.index <- 0.5 #seq(1:NROW(rel.coef))

coef.offset <- 0.15


col.trader<- c(228,26,28)/255
col.ci.trader <- rgb(col.trader[1], col.trader[2], col.trader[3],0.7)

col.proc<- c(55,126,184)/255
col.ci.proc <- rgb(col.proc[1], col.proc[2], col.proc[3],0.7)



col.ret <- c(152,78,163)/255
col.ci.ret <- rgb(col.ret[1], col.ret[2], col.ret[3],0.5)

xlim_temp <- c(min(temp_seq), max(temp_seq))

#par()

ols.col <- col.ci.proc
iis.col <-  col.ci.trader

#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/coef_dist_eff_v1.pdf", height=9, width=12)
pdf(here("data-raw/projections/out/coef_dist_eff.adapt.pdf"), height=9, width=12)

#layout.matrix <- matrix(c(1, 1, 1, 2,2,2 ,3, 3, 4), nrow = 9, ncol = 1)
layout.matrix <- matrix(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7), nrow = 2, ncol = 12, byrow = T)

layout.matrix.r1 <- matrix(c(7, 7, 7, 8, 8, 8, 9, 9, 9, 10, 10, 10), nrow = 1, ncol = 12, byrow = T)
layout.matrix.r2 <- matrix(c(7, 7, 7, 8, 8, 8, 9, 9, 9, 10, 10, 10), nrow = 1, ncol = 12, byrow = T)
layout.matrix.r3 <- matrix(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3), nrow = 1, ncol = 12, byrow = T)
layout.matrix.r4 <- matrix(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3), nrow = 1, ncol = 12, byrow = T)
layout.matrix.r5 <- matrix(c(4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6), nrow = 1, ncol = 12, byrow = T)



layout.matrix <- rbind(layout.matrix.r1, layout.matrix.r2, layout.matrix.r3, layout.matrix.r4, layout.matrix.r5)

par(mar = c(3,4.5,3,0), oma = c(5,4,0.5,0.5), las =1)

par(mar = c(2,0.3,3,0))
layout(mat = layout.matrix) # Widths of the two columns


plot(temp_seq, imp_lower_sc.adapt, type="l", ylim=c(-0.3, 0.01) , xlim=xlim_temp, col=ols.col, main="Income-Based Adaptation (Lower)", xlab="Deg. C", ylab="Delta log(GDPpc)", lwd=5)
#lines(temp_seq, imp_upper_sc.adapt, col="gray75", lwd=1)
#lines(temp_seq, imp_med_sc.adapt, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
#text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
#text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("Income Quantile: ", qlow, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)

lines(temp_seq, imp_lower_sc_isat.adapt, col=iis.col, lwd=5, lty=5)
#lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
#lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)



plot(temp_seq, imp_med_sc.adapt, type="l", ylim=c(-0.3, 0.01), col=ols.col, xlim=xlim_temp, main="Income-Based Adaptation (Middle)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
#lines(temp_seq, imp_upper_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
#text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("Income Quantile: ", qmed, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)
#text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

legend(x=-4, y=-0.15, legend=c("OLS", "Robust IIS"),
       col=c(ols.col, iis.col), lty=c(1, 5), lwd=c(3,3), cex=1.4, bg="transparent", bty = "n", y.intersp=0.7)


#lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
#lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_med_sc_isat.adapt, col=iis.col, lwd=5, lty=5)




plot(temp_seq, imp_upper_sc.adapt, type="l", ylim=c(-0.3, 0.01), xlim=xlim_temp, col=ols.col, main="Income-Based Adaptation (Upper)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
#lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_med_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
text(label=paste("Income Quantile:", qup, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)
# text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
# text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

#lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat.adapt, col=iis.col, lwd=5, lty=5)
#lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)




hist(temp_mid, xlim=xlim_temp, breaks=15,  col="gray85", main="Temp. Distribution in Income Range")
hist(temp_lower, xlim=xlim_temp, breaks=15, main="", yaxt="n", col="gray85")
hist(temp_upper, xlim=xlim_temp, breaks=15, main="", yaxt="n", col="gray85")

#par(mar = c(4,4,2,0), oma = c(3,3.5,1.5,1.5), las =1, mfrow=c(1, NROW(rel.coef)))


for (i in 1: NROW(rel.coef.adapt)){
  #i <- 1
  par(mar = c(2,4,3,0))

  yint <- max(max(abs(coef.plot$iis.ci.p975.adapt[i]),  abs(coef.plot$iis.ci.p025.adapt[i]) ), max(abs(coef.plot$ols.ci.p975.adapt[i]),  abs(coef.plot$ols.ci.p025.adapt[i]) ))
  yscale <- 1.15
  barwidth <- 0.003

  ylim <- c(-yint*yscale, yint*yscale)

  ylab <- ""
  if (i==1){
    ylab="Estimated Coefficient"
  }

  plot(coef.plot$plot.index[i], coef.plot$ols.coef.adapt[i], ylim=ylim,  xlim=c(0.45,0.7),  main=coef.plot$coef[i], pch=19, cex=3,   ylab=ylab, xlab="", xaxt="n", col=ols.col)
  points(coef.plot$plot.index[i]+coef.offset, coef.plot$iis.coef.adapt[i], pch=19, cex=3,  col=iis.col)
  rect(ybottom=coef.plot$ols.ci.p025.adapt[i], ytop=coef.plot$ols.ci.p975.adapt[i], xleft=coef.plot$plot.index[i]-barwidth, xright=coef.plot$plot.index[i]+barwidth,  density = NULL, border = ols.col, col=ols.col)
  rect(ybottom=coef.plot$iis.ci.p025.adapt[i], ytop=coef.plot$iis.ci.p975.adapt[i], xleft=coef.plot$plot.index[i]-barwidth+coef.offset, xright=coef.plot$plot.index[i]+barwidth+coef.offset,  density = NULL, border = iis.col, col=iis.col)
  axis(1, at=c(coef.plot$plot.index[i], coef.plot$plot.index[i]+coef.offset), labels=c("OLS", "Robust IIS"), cex.axis=1.3)
  if (i==1){
    legend(x=0.45, y=0.065, legend=c("OLS", "Robust IIS"),
           col=c(ols.col, iis.col), lty=c(1, 1), lwd=c(4,4), cex=1.1, bg="transparent", bty = "n", y.intersp=0.7)

  }
  abline(h=0, lty=2, col="gray25", lwd=1.5)

}

dev.off()


#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/coef_dist_eff_v1.pdf", height=9, width=12)
pdf(here("data-raw/projections/out/coef_dist_eff.adapt.L1.pdf"), height=9, width=12)

#layout.matrix <- matrix(c(1, 1, 1, 2,2,2 ,3, 3, 4), nrow = 9, ncol = 1)
layout.matrix <- matrix(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7), nrow = 2, ncol = 12, byrow = T)

layout.matrix.r1 <- matrix(c(7, 7, 7, 8, 8, 8, 9, 9, 9, 10, 10, 10), nrow = 1, ncol = 12, byrow = T)
layout.matrix.r2 <- matrix(c(7, 7, 7, 8, 8, 8, 9, 9, 9, 10, 10, 10), nrow = 1, ncol = 12, byrow = T)
layout.matrix.r3 <- matrix(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3), nrow = 1, ncol = 12, byrow = T)
layout.matrix.r4 <- matrix(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3), nrow = 1, ncol = 12, byrow = T)
layout.matrix.r5 <- matrix(c(4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6), nrow = 1, ncol = 12, byrow = T)



layout.matrix <- rbind(layout.matrix.r1, layout.matrix.r2, layout.matrix.r3, layout.matrix.r4, layout.matrix.r5)

par(mar = c(3,4.5,3,0), oma = c(5,4,0.5,0.5), las =1)

par(mar = c(2,0.3,3,0))
layout(mat = layout.matrix) # Widths of the two columns


plot(temp_seq, imp_lower_sc.adapt.L1, type="l", ylim=c(-0.3, 0.01) , xlim=xlim_temp, col=ols.col, main="Income-Based Adaptation (Lower)", xlab="Deg. C", ylab="Delta log(GDPpc)", lwd=5)
#lines(temp_seq, imp_upper_sc.adapt.L1, col="gray75", lwd=1)
#lines(temp_seq, imp_med_sc.adapt.L1, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
#text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
#text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("Income Quantile: ", qlow, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)

lines(temp_seq, imp_lower_sc_isat.adapt.L1, col=iis.col, lwd=5, lty=5)
#lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
#lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)



plot(temp_seq, imp_med_sc.adapt.L1, type="l", ylim=c(-0.3, 0.01), col=ols.col, xlim=xlim_temp, main="Income-Based Adaptation (Middle)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
#lines(temp_seq, imp_upper_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
#text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("Income Quantile: ", qmed, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)
#text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

legend(x=-4, y=-0.15, legend=c("OLS", "Robust IIS"),
       col=c(ols.col, iis.col), lty=c(1, 5), lwd=c(3,3), cex=1.4, bg="transparent", bty = "n", y.intersp=0.7)


#lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
#lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_med_sc_isat.adapt.L1, col=iis.col, lwd=5, lty=5)




plot(temp_seq, imp_upper_sc.adapt.L1, type="l", ylim=c(-0.3, 0.01), xlim=xlim_temp, col=ols.col, main="Income-Based Adaptation (Upper)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
#lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_med_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
text(label=paste("Income Quantile:", qup, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)
# text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
# text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

#lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat.adapt.L1, col=iis.col, lwd=5, lty=5)
#lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)




hist(temp_mid, xlim=xlim_temp, breaks=15,  col="gray85", main="Temp. Distribution in Income Range")
hist(temp_lower, xlim=xlim_temp, breaks=15, main="", yaxt="n", col="gray85")
hist(temp_upper, xlim=xlim_temp, breaks=15, main="", yaxt="n", col="gray85")

#par(mar = c(4,4,2,0), oma = c(3,3.5,1.5,1.5), las =1, mfrow=c(1, NROW(rel.coef)))


for (i in 1: NROW(rel.coef.adapt.L1)){
  #i <- 1
  par(mar = c(2,4,3,0))

  yint <- max(max(abs(coef.plot$iis.ci.p975.adapt.L1[i]),  abs(coef.plot$iis.ci.p025.adapt.L1[i]) ), max(abs(coef.plot$ols.ci.p975.adapt.L1[i]),  abs(coef.plot$ols.ci.p025.adapt.L1[i]) ))
  yscale <- 1.15
  barwidth <- 0.003

  ylim <- c(-yint*yscale, yint*yscale)

  ylab <- ""
  if (i==1){
    ylab="Estimated Coefficient"
  }

  plot(coef.plot$plot.index[i], coef.plot$ols.coef.adapt.L1[i], ylim=ylim,  xlim=c(0.45,0.7),  main=coef.plot$coef[i], pch=19, cex=3,   ylab=ylab, xlab="", xaxt="n", col=ols.col)
  points(coef.plot$plot.index[i]+coef.offset, coef.plot$iis.coef.adapt.L1[i], pch=19, cex=3,  col=iis.col)
  rect(ybottom=coef.plot$ols.ci.p025.adapt.L1[i], ytop=coef.plot$ols.ci.p975.adapt.L1[i], xleft=coef.plot$plot.index[i]-barwidth, xright=coef.plot$plot.index[i]+barwidth,  density = NULL, border = ols.col, col=ols.col)
  rect(ybottom=coef.plot$iis.ci.p025.adapt.L1[i], ytop=coef.plot$iis.ci.p975.adapt.L1[i], xleft=coef.plot$plot.index[i]-barwidth+coef.offset, xright=coef.plot$plot.index[i]+barwidth+coef.offset,  density = NULL, border = iis.col, col=iis.col)
  axis(1, at=c(coef.plot$plot.index[i], coef.plot$plot.index[i]+coef.offset), labels=c("OLS", "Robust IIS"), cex.axis=1.3)
  if (i==1){
    legend(x=0.45, y=0.065, legend=c("OLS", "Robust IIS"),
           col=c(ols.col, iis.col), lty=c(1, 1), lwd=c(4,4), cex=1.1, bg="transparent", bty = "n", y.intersp=0.7)

  }
  abline(h=0, lty=2, col="gray25", lwd=1.5)

}

dev.off()


############# Split into two




#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/coef_v2.pdf", height=5, width=10)
pdf(here("data-raw/projections/out/coef.adapt.pdf"), height=5, width=10)

par(mfrow=c(1,4))
par(mar = c(3,4.5,3,0), oma = c(5,4,0.5,0.5), las =1)


for (i in 1: NROW(rel.coef.adapt)){
  #i <- 1
  par(mar = c(2,4,3,0))

  yint <- max(max(abs(coef.plot$iis.ci.p975.adapt[i]),  abs(coef.plot$iis.ci.p025.adapt[i]) ), max(abs(coef.plot$ols.ci.p975.adapt[i]),  abs(coef.plot$ols.ci.p025.adapt[i]) ))
  yscale <- 1.15
  barwidth <- 0.003

  ylim <- c(-yint*yscale, yint*yscale)

  ylab <- ""
  if (i==1){
    ylab="Estimated Coefficient"
  }

  plot(coef.plot$plot.index[i], coef.plot$ols.coef.adapt[i], ylim=ylim,  xlim=c(0.45,0.7),  main=coef.plot$coef[i], pch=19, cex=3,   ylab=ylab, xlab="", xaxt="n", col=ols.col)
  points(coef.plot$plot.index[i]+coef.offset, coef.plot$iis.coef.adapt[i], pch=19, cex=3,  col=iis.col)
  rect(ybottom=coef.plot$ols.ci.p025.adapt[i], ytop=coef.plot$ols.ci.p975.adapt[i], xleft=coef.plot$plot.index[i]-barwidth, xright=coef.plot$plot.index[i]+barwidth,  density = NULL, border = ols.col, col=ols.col)
  rect(ybottom=coef.plot$iis.ci.p025.adapt[i], ytop=coef.plot$iis.ci.p975.adapt[i], xleft=coef.plot$plot.index[i]-barwidth+coef.offset, xright=coef.plot$plot.index[i]+barwidth+coef.offset,  density = NULL, border = iis.col, col=iis.col)
  axis(1, at=c(coef.plot$plot.index[i], coef.plot$plot.index[i]+coef.offset), labels=c("OLS", "Robust IIS"), cex.axis=1.3)
  if (i==1){
    legend(x=0.45, y=0.065, legend=c("OLS", "Robust IIS"),
           col=c(ols.col, iis.col), lty=c(1, 1), lwd=c(4,4), cex=1.1, bg="transparent", bty = "n", y.intersp=0.7)

  }
  abline(h=0, lty=2, col="gray25", lwd=1.5)

}

dev.off()


#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/coef_v2.pdf", height=5, width=10)
pdf(here("data-raw/projections/out/coef.adapt.L1.pdf"), height=5, width=10)

par(mfrow=c(1,4))
par(mar = c(3,4.5,3,0), oma = c(5,4,0.5,0.5), las =1)


for (i in 1: NROW(rel.coef.adapt.L1)){
  #i <- 1
  par(mar = c(2,4,3,0))

  yint <- max(max(abs(coef.plot$iis.ci.p975.adapt.L1[i]),  abs(coef.plot$iis.ci.p025.adapt.L1[i]) ), max(abs(coef.plot$ols.ci.p975.adapt.L1[i]),  abs(coef.plot$ols.ci.p025.adapt.L1[i]) ))
  yscale <- 1.15
  barwidth <- 0.003

  ylim <- c(-yint*yscale, yint*yscale)

  ylab <- ""
  if (i==1){
    ylab="Estimated Coefficient"
  }

  plot(coef.plot$plot.index[i], coef.plot$ols.coef.adapt.L1[i], ylim=ylim,  xlim=c(0.45,0.7),  main=coef.plot$coef[i], pch=19, cex=3,   ylab=ylab, xlab="", xaxt="n", col=ols.col)
  points(coef.plot$plot.index[i]+coef.offset, coef.plot$iis.coef.adapt.L1[i], pch=19, cex=3,  col=iis.col)
  rect(ybottom=coef.plot$ols.ci.p025.adapt.L1[i], ytop=coef.plot$ols.ci.p975.adapt.L1[i], xleft=coef.plot$plot.index[i]-barwidth, xright=coef.plot$plot.index[i]+barwidth,  density = NULL, border = ols.col, col=ols.col)
  rect(ybottom=coef.plot$iis.ci.p025.adapt.L1[i], ytop=coef.plot$iis.ci.p975.adapt.L1[i], xleft=coef.plot$plot.index[i]-barwidth+coef.offset, xright=coef.plot$plot.index[i]+barwidth+coef.offset,  density = NULL, border = iis.col, col=iis.col)
  axis(1, at=c(coef.plot$plot.index[i], coef.plot$plot.index[i]+coef.offset), labels=c("OLS", "Robust IIS"), cex.axis=1.3)
  if (i==1){
    legend(x=0.45, y=0.065, legend=c("OLS", "Robust IIS"),
           col=c(ols.col, iis.col), lty=c(1, 1), lwd=c(4,4), cex=1.1, bg="transparent", bty = "n", y.intersp=0.7)

  }
  abline(h=0, lty=2, col="gray25", lwd=1.5)

}

dev.off()



########### Impact function

#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/eff_v1.pdf", height=8, width=12)
pdf(here("data-raw/projections/out/eff.adapt.v2.pdf"), height=8, width=12)

layout.matrix.r1 <- matrix(c(1, 2, 3, 4), nrow = 1, ncol = 4, byrow = T)
layout.matrix.r2 <- matrix(c(1, 2, 3, 4), nrow = 1, ncol = 4, byrow = T)
layout.matrix.r3 <- matrix(c(5, 6, 7, 8), nrow = 1, ncol = 4, byrow = T)

layout.matrix <- rbind(layout.matrix.r1, layout.matrix.r2, layout.matrix.r3)

#par(mar = c(3,4.5,3,0), oma = c(5,4,0.5,0.5), las =1)
par(mar = c(3,4.5,3,0), oma = c(2,2,0.5,0.5), las =1)

par(mar = c(2,4.5,3,0))
#par(mar = c(2,0.3,3,0))
layout(mat = layout.matrix) # Widths of the two columns


plot(temp_seq, imp_med_sc, type = "l", ylim=c(-0.3, 0.01) , xlim=xlim_temp, col=ols.col, main="Base Model without Adaptation", xlab="Deg. C", ylab="Delta log(GDPpc)", lwd=5)
abline(h=0, col="gray35", lty=2)
text(label="Across Incomes", cex=1.6, col="Gray35", x=10, y=-0.27)
lines(temp_seq, imp_med_sc_isat, col=iis.col, lwd=5, lty=5)



#par(mfrow=c(1,1))

plot(temp_seq, imp_lower_sc.adapt, type="l", ylim=c(-0.3, 0.01) , xlim=xlim_temp, col=ols.col, main="Income-Based Adaptation (Lower)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
#lines(temp_seq, imp_upper_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_med_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
#text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
#text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("Income Quantile: ", qlow, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)

lines(temp_seq, imp_lower_sc_isat.adapt, col=iis.col, lwd=5, lty=5)
#lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
#lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)



plot(temp_seq, imp_med_sc.adapt, type="l", ylim=c(-0.3, 0.01), col=ols.col, xlim=xlim_temp, main="Income-Based Adaptation (Middle)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
#lines(temp_seq, imp_upper_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
#text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("Income Quantile: ", qmed, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)
#text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

legend(x=-4, y=-0.15, legend=c("OLS", "Robust IIS"),
       col=c(ols.col, iis.col), lty=c(1, 5), lwd=c(3,3), cex=1.6, bg="transparent", bty = "n", y.intersp=0.7)


#lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
#lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_med_sc_isat.adapt, col=iis.col, lwd=5, lty=5)




plot(temp_seq, imp_upper_sc.adapt, type="l", ylim=c(-0.3, 0.01), xlim=xlim_temp, col=ols.col, main="Income-Based Adaptation (Upper)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
#lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_med_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
text(label=paste("Income Quantile: ", qup, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)
# text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
# text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

#lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat.adapt, col=iis.col, lwd=5, lty=5)
#lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)


ylim_temp <- c(0,max(table(cut(temp, breaks = 15, labels = FALSE))))
hist(temp, xlim=xlim_temp, ylim =ylim_temp,   breaks=15,  col="gray85", main="Overall Temperature")
hist(temp_mid, xlim=xlim_temp, ylim =ylim_temp, breaks=15,  main = "Temperature (Lower Income)",ylab = "",  yaxt = "n", col="gray85")#, main="Temp. Distribution in Income Range")
hist(temp_lower, xlim=xlim_temp, ylim =ylim_temp, breaks=15, main="Temperature (Middle Income)", ylab = "", yaxt="n", col="gray85")
hist(temp_upper, xlim=xlim_temp, ylim =ylim_temp, breaks=15, main="Temperature (Upper Income)", ylab = "", yaxt="n", col="gray85")

#par(mar = c(4,4,2,0), oma = c(3,3.5,1.5,1.5), las =1, mfrow=c(1, NROW(rel.coef)))

dev.off()




#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/eff_v1.pdf", height=8, width=12)
pdf(here("data-raw/projections/out/eff.adapt.L1.v2.pdf"), height=8, width=12)

layout.matrix.r1 <- matrix(c(1, 2, 3, 4), nrow = 1, ncol = 4, byrow = T)
layout.matrix.r2 <- matrix(c(1, 2, 3, 4), nrow = 1, ncol = 4, byrow = T)
layout.matrix.r3 <- matrix(c(5, 6, 7, 8), nrow = 1, ncol = 4, byrow = T)

layout.matrix <- rbind(layout.matrix.r1, layout.matrix.r2, layout.matrix.r3)

#par(mar = c(3,4.5,3,0), oma = c(5,4,0.5,0.5), las =1)
par(mar = c(3,4.5,3,0), oma = c(2,2,0.5,0.5), las =1)

par(mar = c(2,4.5,3,0))
#par(mar = c(2,0.3,3,0))
layout(mat = layout.matrix) # Widths of the two columns


plot(temp_seq, imp_med_sc, type = "l", ylim=c(-0.3, 0.01) , xlim=xlim_temp, col=ols.col, main="Base Model without Adaptation", xlab="Deg. C", ylab="Delta log(GDPpc)", lwd=5)
abline(h=0, col="gray35", lty=2)
text(label="Across Incomes", cex=1.6, col="Gray35", x=10, y=-0.27)
lines(temp_seq, imp_med_sc_isat, col=iis.col, lwd=5, lty=5)



#par(mfrow=c(1,1))

plot(temp_seq, imp_lower_sc.adapt.L1, type="l", ylim=c(-0.3, 0.01) , xlim=xlim_temp, col=ols.col, main="Income-Based Adaptation (Lower)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
#lines(temp_seq, imp_upper_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_med_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
#text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
#text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("Income Quantile: ", qlow, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)

lines(temp_seq, imp_lower_sc_isat.adapt.L1, col=iis.col, lwd=5, lty=5)
#lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
#lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)



plot(temp_seq, imp_med_sc.adapt.L1, type="l", ylim=c(-0.3, 0.01), col=ols.col, xlim=xlim_temp, main="Income-Based Adaptation (Middle)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
#lines(temp_seq, imp_upper_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
#text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("Income Quantile: ", qmed, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)
#text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

legend(x=-4, y=-0.15, legend=c("OLS", "Robust IIS"),
       col=c(ols.col, iis.col), lty=c(1, 5), lwd=c(3,3), cex=1.6, bg="transparent", bty = "n", y.intersp=0.7)


#lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
#lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_med_sc_isat.adapt.L1, col=iis.col, lwd=5, lty=5)




plot(temp_seq, imp_upper_sc.adapt.L1, type="l", ylim=c(-0.3, 0.01), xlim=xlim_temp, col=ols.col, main="Income-Based Adaptation (Upper)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
#lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_med_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
text(label=paste("Income Quantile: ", qup, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)
# text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
# text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

#lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat.adapt.L1, col=iis.col, lwd=5, lty=5)
#lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)


ylim_temp <- c(0,max(table(cut(temp, breaks = 15, labels = FALSE))))
hist(temp, xlim=xlim_temp, ylim =ylim_temp,   breaks=15,  col="gray85", main="Overall Temperature")
hist(temp_mid, xlim=xlim_temp, ylim =ylim_temp, breaks=15,  main = "Temperature (Lower Income)",ylab = "",  yaxt = "n", col="gray85")#, main="Temp. Distribution in Income Range")
hist(temp_lower, xlim=xlim_temp, ylim =ylim_temp, breaks=15, main="Temperature (Middle Income)", ylab = "", yaxt="n", col="gray85")
hist(temp_upper, xlim=xlim_temp, ylim =ylim_temp, breaks=15, main="Temperature (Upper Income)", ylab = "", yaxt="n", col="gray85")

#par(mar = c(4,4,2,0), oma = c(3,3.5,1.5,1.5), las =1, mfrow=c(1, NROW(rel.coef)))

dev.off()



