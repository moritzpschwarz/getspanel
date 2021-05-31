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
load(here("data-raw/projections/m2_L1.RData"))
load(here("data-raw/projections/m2.isat_L1.RData"))





dat <- vroom::vroom(file = here("data-raw/projections/damage_curve_country_dataset_timetrends_updated02-19.csv"))


# Estimate M1 -------------------------------------------------------------

dat %>%
  select(iso, year, diff.ln_gdp_cap, temp, temp_2, prcp, prcp_2 , starts_with(c("year_","time_", "iso_"))) %>%
  select(-iso,-year) %>%
  lm(diff.ln_gdp_cap~.-1,data = .) -> m1

dat %>%
  select(iso, year, diff.ln_gdp_cap, contains(c("temp","prcp"), ignore.case = FALSE), -contains("diff"),diff.ln_gdp_cap, starts_with(c("year_","time_", "iso_"))) %>%
  select(-iso,-year) %>%
  lm(diff.ln_gdp_cap~.-1,data = .) -> m1_L1


# Process M1 --------------------------------------------------------------


m1 %>%
  tidy %>%
  filter(is.na(estimate)) %>%
  pull(term) -> m1_drop

m1_L1 %>%
  tidy %>%
  filter(is.na(estimate)) %>%
  pull(term) -> m1_L1_drop



# Estimate M2 -------------------------------------------------------------

dat %>%
  select(iso, year, diff.ln_gdp_cap, temp, temp_2, prcp, prcp_2 , starts_with(c("year_","time_", "iso_"))) %>%
  select(-all_of(m1_drop)) %>%
  drop_na -> m2_data

dat %>%
  select(iso, year, diff.ln_gdp_cap, contains(c("temp","prcp"), ignore.case = FALSE), -contains("diff"),diff.ln_gdp_cap, starts_with(c("year_","time_", "iso_"))) %>%
  select(-all_of(m1_L1_drop)) %>%
  drop_na -> m2_L1_data



# Start of Felix' code ----------------------------------------------------

rel.coef<- c("temp", "temp_2")
rel.coef.lag <- c("temp", "temp_2", "L1.temp","L1.temp_2")

#rel.coef <- c("temp", "temp2", "temp_loggdp", "temp2_loggdp")
#rel.coef.lag <- c("temp", "temp2", "temp_L1.loggdp", "temp2_L1.loggdp")


dist1 <- distorttest(m2.isat,  coef=rel.coef)
dist1.lag <- distorttest(m2.isat_L1,  coef=rel.coef.lag)

coefficients(dist1.lag$ols)[1:10]
coefficients(m2_L1)[1:10]

coefficients(dist1.lag$ols)[rel.coef.lag]

outliertest(m2.isat_L1)
outliertest(m2.isat)

########## Manually Create a results table

m1.ols <- dist1$ols
m1.iis <- dist1$iis

coef.index <- c(1, 4, 7, 10)
se.index <- coef.index+1
nround <- 5

sum.index <- c(13, 14, 15, 17)


stars.ols <-rep("", length(rel.coef))
stars.ols[ m1.ols$mean.results[rel.coef, "p-value"] < 0.05] <- "*"
stars.ols[ m1.ols$mean.results[rel.coef, "p-value"] < 0.01] <- "**"

stars.iis <-rep("", length(rel.coef))
stars.iis[ m1.iis$mean.results[rel.coef, "p-value"] < 0.05] <- "*"
stars.iis[ m1.iis$mean.results[rel.coef, "p-value"] < 0.01] <- "**"


res.tab <- data.frame(matrix(NA, nrow=18, ncol=4))
names(res.tab) <- c("Var", "OLS", "IIS", "Diff")
res.tab$Var[coef.index] <- rel.coef
res.tab$OLS[coef.index] <- paste(round(coefficients(m1.ols)[rel.coef], nround), stars.ols, sep="")
res.tab$IIS[coef.index] <- paste(round(coefficients(m1.iis)[rel.coef], nround), stars.iis, sep="")
res.tab$Diff[coef.index] <- round(coefficients(m1.iis)[rel.coef]-coefficients(m1.ols)[rel.coef], nround)

res.tab$OLS[se.index] <- paste("(", round(m1.ols$mean.results[rel.coef, "std.error"], nround), ")", sep="")
res.tab$IIS[se.index] <- paste("(", round(m1.iis$mean.results[rel.coef, "std.error"], nround) , ")", sep="")

res.tab$Var[sum.index] <- c("n", "L", "nOutl", "Dist")
res.tab$OLS[sum.index[1:3]] <- c(length(m1.ols$aux$y), round(logLik(m1.ols), 3), "")
res.tab$IIS[sum.index[1:3]] <- c(length(m1.iis$aux$y), round(logLik(m1.iis), 3), length(m1.iis$ISnames))
res.tab$Diff[sum.index[4]] <- paste( round(dist1$statistic, 3), " (df=", NROW(dist1$coef.diff) , ")", sep="")
res.tab$Diff[sum.index[4]+1] <- paste("[p=", round(dist1$p.value, 4), "]", sep="")

res.tab[is.na(res.tab)] <- ""
names(res.tab) <- c("", "OLS", "IIS", "Diff")

str(res.tab)

print(xtable(res.tab, type = "latex"),
      file = here("data-raw","projections","out","results1.tex"), include.rownames=FALSE)


########## Manually Create a results table for lag model

m1.ols.lag <- dist1.lag$ols
m1.iis.lag <- dist1.lag$iis

coef.index <- c(1, 4, 7, 10)
se.index <- coef.index+1
nround <- 5

sum.index <- c(13, 14, 15, 17)


stars.ols <-rep("", length(rel.coef.lag))
stars.ols[ m1.ols.lag$mean.results[rel.coef.lag, "p-value"] < 0.05] <- "*"
stars.ols[ m1.ols.lag$mean.results[rel.coef.lag, "p-value"] < 0.01] <- "**"

stars.iis <-rep("", length(rel.coef.lag))
stars.iis[ m1.iis.lag$mean.results[rel.coef.lag, "p-value"] < 0.05] <- "*"
stars.iis[ m1.iis.lag$mean.results[rel.coef.lag, "p-value"] < 0.01] <- "**"


res.tab.lag <- data.frame(matrix(NA, nrow=18, ncol=4))
names(res.tab.lag) <- c("Var", "OLS", "IIS", "Diff")
res.tab.lag$Var[coef.index] <- rel.coef
res.tab.lag$OLS[coef.index] <- paste(round(coefficients(m1.ols.lag)[rel.coef.lag], nround), stars.ols, sep="")
res.tab.lag$IIS[coef.index] <- paste(round(coefficients(m1.iis.lag)[rel.coef.lag], nround), stars.iis, sep="")
res.tab.lag$Diff[coef.index] <- round(coefficients(m1.iis.lag)[rel.coef.lag]-coefficients(m1.ols.lag)[rel.coef.lag], nround)

#temp_L1
#temp2_L1.loggdp

res.tab.lag$OLS[se.index] <- paste("(", round(m1.ols.lag$mean.results[rel.coef.lag, "std.error"], nround), ")", sep="")
res.tab.lag$IIS[se.index] <- paste("(", round(m1.iis.lag$mean.results[rel.coef.lag, "std.error"], nround) , ")", sep="")

res.tab.lag$Var[sum.index] <- c("n", "L", "nOutl", "Dist")
res.tab.lag$OLS[sum.index[1:3]] <- c(length(m1.ols.lag$aux$y), round(logLik(m1.ols.lag), 3), "")
res.tab.lag$IIS[sum.index[1:3]] <- c(length(m1.iis.lag$aux$y), round(logLik(m1.iis.lag), 3), length(m1.iis.lag$ISnames))
res.tab.lag$Diff[sum.index[4]] <- paste( round(dist1.lag$statistic, 3), " (df=", NROW(dist1.lag$coef.diff) , ")", sep="")
res.tab.lag$Diff[sum.index[4]+1] <- paste("[p=", round(dist1.lag$p.value, 4), "]", sep="")

res.tab.lag[is.na(res.tab.lag)] <- ""
names(res.tab.lag) <- c("", "OLS", "IIS", "Diff")

str(res.tab.lag)


print(xtable(res.tab.lag, type = "latex"),
      file = here("data-raw","projections","out","results1_lag.tex"), include.rownames=FALSE)




############# Recover Years and Countries for Plotting ###############
#ctry_index.com$is.index <- seq(1:NROW(ctry_index.com)) # changed moritz

ctry_index.com <- m2_data

ctry_index.com$is.index <- seq(1:nrow(m2.isat$aux$mX))
ctry_index.com$iis <- 0
ctry_index.com$iis_sign <- 0

ctry_index.com.lag <- m2_L1_data

ctry_index.com.lag$is.index <- seq(1:NROW(ctry_index.com.lag))
ctry_index.com.lag$iis <- 0
ctry_index.com.lag$iis_sign <- 0

isdat1 <- isatdates(m2.isat)
ctry_index.com$iis[ctry_index.com$is.index %in% isdat1$iis$index] <- 1

ctry_index.com$iis_sign[ctry_index.com$is.index %in% isdat1$iis$index] <- 1
ctry_index.com$iis_sign[isdat1$iis$index] <- isdat1$iis$coef/abs(isdat1$iis$coef)

ctry_index.com$iis_lev <- 0
ctry_index.com$iis_lev[ctry_index.com$is.index %in% isdat1$iis$index] <- 1
ctry_index.com$iis_lev[isdat1$iis$index] <- isdat1$iis$coef


isdat1.lag <- isatdates(m2.isat_L1)
ctry_index.com.lag$iis[ctry_index.com.lag$is.index %in% isdat1.lag$iis$index] <- 1

ctry_index.com.lag$iis_sign[ctry_index.com.lag$is.index %in% isdat1$iis$index] <- 1
ctry_index.com.lag$iis_sign[isdat1.lag$iis$index] <- isdat1.lag$iis$coef/abs(isdat1.lag$iis$coef)

ctry_index.com.lag$iis_lev <- 0
ctry_index.com.lag$iis_lev[ctry_index.com.lag$is.index %in% isdat1.lag$iis$index] <- 1
ctry_index.com.lag$iis_lev[isdat1.lag$iis$index] <- isdat1.lag$iis$coef


ctry_outl <- aggregate(ctry_index.com$iis, by=list(ctry_index.com$iso), FUN=sum)
names(ctry_outl) <- c("iso", "outl")

year_outl <- aggregate(ctry_index.com$iis, by=list(ctry_index.com$year), FUN=sum)
names(year_outl) <- c("year", "outl")

ctry_outl.lag <- aggregate(ctry_index.com.lag$iis, by=list(ctry_index.com.lag$iso), FUN=sum)
names(ctry_outl.lag) <- c("iso", "outl")

year_outl.lag <- aggregate(ctry_index.com.lag$iis, by=list(ctry_index.com.lag$year), FUN=sum)
names(year_outl.lag) <- c("year", "outl")

###compute by year but negative and positve
ctry_index.com$iis_pos <- 0
ctry_index.com$iis_neg <- 0
ctry_index.com$iis_pos[ctry_index.com$iis_sign>0] <- 1
ctry_index.com$iis_neg[ctry_index.com$iis_sign<0] <- 1

year_outl_pos <- aggregate(ctry_index.com$iis_pos, by=list(ctry_index.com$year), FUN=sum)
names(year_outl_pos) <- c("year", "outl_pos")

year_outl_neg <- aggregate(ctry_index.com$iis_neg, by=list(ctry_index.com$year), FUN=sum)
names(year_outl_neg) <- c("year", "outl_neg")

ctry_outl$iso[ctry_outl$iso=="ROM"] <- "ROU"
ctry_outl$iso[ctry_outl$iso=="ZAR"] <- "COD"

ctry_index.com.lag$iis_pos <- 0
ctry_index.com.lag$iis_neg <- 0
ctry_index.com.lag$iis_pos[ctry_index.com.lag$iis_sign>0] <- 1
ctry_index.com.lag$iis_neg[ctry_index.com.lag$iis_sign<0] <- 1

year_outl_pos.lag <- aggregate(ctry_index.com.lag$iis_pos, by=list(ctry_index.com.lag$year), FUN=sum)
names(year_outl_pos.lag) <- c("year", "outl_pos")

year_outl_neg.lag <- aggregate(ctry_index.com.lag$iis_neg, by=list(ctry_index.com.lag$year), FUN=sum)
names(year_outl_neg.lag) <- c("year", "outl_neg")

ctry_outl.lag$iso[ctry_outl$iso=="ROM"] <- "ROU"
ctry_outl.lag$iso[ctry_outl$iso=="ZAR"] <- "COD"



#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/year_hist.pdf", height=7, width=8)
pdf(here("data-raw/projections/out/year_hist.pdf"), height=7, width=8)

par(mfrow=c(1,1))
plot(year_outl$year,year_outl$outl, type="h", lwd=3, col="gray75", ylab="#Outlying Countries", xlab="Year")

dev.off()


#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/year_hist_lag.pdf", height=7, width=8)
pdf(here("data-raw/projections/out/year_hist_lag.pdf"), height=7, width=8)
par(mfrow=c(1,1))
plot(year_outl.lag$year,year_outl.lag$outl, type="h", lwd=3, col="gray75", ylab="#Outlying Countries", xlab="Year")

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



pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/year_hist_sign_lag.pdf", height=5, width=11)
pdf(here("data-raw/projections/out/year_hist_sign_lag.pdf"), height=5, width=11)
par(mfrow=c(1,1))
plot(year_outl.lag$year, year_outl_neg.lag$outl_neg*(-1), type="h", ylim=c(-12, 12), xlim=c(1963, 2012), lwd=3, col="blue", yaxt="n", ylab="#Outlying Countries", xlab="Year")
lines(year_outl.lag$year, year_outl_pos.lag$outl_pos, type="h", lwd=3, col="red")
axis(2, at=c(-10, -5, 0, 5), labels=c("10 (Neg.)", "5 (Neg.)", "0", "5 (Pos.)"))
abline(h=0, lty=2, col="gray35")
abline(h=c(-10, -5, 5), lty=3, col="gray75")

dev.off()

#install.packages("rworldmap")
library(rworldmap)

max(ctry_outl$outl)
max(ctry_outl.lag$outl)

#join to a coarse resolution map
cols <- c("gray85", "#fdd49e", "#fdbb84",
          "#fc8d59",
          "#ef6548",
          "#d7301f",
          "#b30000",
          "#7f0000")

spdf <- joinCountryData2Map(ctry_outl, joinCode="ISO3", nameJoinColumn="iso")

spdf.lag <- joinCountryData2Map(ctry_outl.lag, joinCode="ISO3", nameJoinColumn="iso")

#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/ctry_map.pdf", height=10, width=11)
pdf(here("data-raw/projections/out/ctry_map.pdf"), height=10, width=11)

map1 <- mapCountryData(spdf, nameColumnToPlot="outl",  addLegend=FALSE, catMethod=c(0, 1, 2, 3, 4, 5, 8, 10, 12), colourPalette=cols)

do.call(addMapLegend
        ,c(map1
           ,legendLabels="all"
           ,horizontal=TRUE
           ,legendWidth=0.5
           ,legendIntervals="data"
           , legendMar = 7))


dev.off()





#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/ctry_map_lag.pdf", height=10, width=11)
pdf(here("data-raw/projections/out/ctry_map_lag.pdf"), height=10, width=11)

map1 <- mapCountryData(spdf.lag, nameColumnToPlot="outl",  addLegend=FALSE, catMethod=c(0, 1, 2, 3, 4, 5, 8, 10, 12), colourPalette=cols)

do.call(addMapLegend
        ,c(map1
           ,legendLabels="all"
           ,horizontal=TRUE
           ,legendWidth=0.5
           ,legendIntervals="data"
           , legendMar = 7))


dev.off()

#install.packages("countrycode")
library(countrycode)

ctry_index.com$iso[ctry_index.com$iso=="ROM"] <- "ROU"
ctry_index.com$iso[ctry_index.com$iso=="ZAR"] <- "COD"

ctry_index.com$continent <- countrycode(sourcevar = ctry_index.com$iso,
                            origin = "iso3c",
                            destination = "continent")

ctry_index.com$continent[ctry_index.com$iso=="ZAR"] <- "Africa"
ctry_index.com$continent[ctry_index.com$iso=="ROM"] <- "Europe"

ctry_index.com.lag$iso[ctry_index.com.lag$iso=="ROM"] <- "ROU"
ctry_index.com.lag$iso[ctry_index.com.lag$iso=="ZAR"] <- "COD"

ctry_index.com.lag$continent <- countrycode(sourcevar = ctry_index.com.lag$iso,
                                        origin = "iso3c",
                                        destination = "continent")

ctry_index.com.lag$continent[ctry_index.com.lag$iso=="ZAR"] <- "Africa"
ctry_index.com.lag$continent[ctry_index.com.lag$iso=="ROM"] <- "Europe"


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



##################### Heatmap with lag ################

#install.packages("reshape2")
library(reshape2)

#heatmap(data, Colv = NA, Rowv = NA, scale="column")
ctry_index_wide.lag <- dcast(ctry_index.com.lag, iso + continent ~ year, value.var="iis_sign")
ctry_index_wide.lag

ctry_index_wide_lev.lag <- dcast(ctry_index.com.lag, iso + continent ~ year, value.var="iis_lev")
ctry_index_wide_lev.lag

ctry_index_wide.ord.lag <- ctry_index_wide.lag[order(ctry_index_wide.lag$continent),]
ctry_index_wide.ord_lev.lag <- ctry_index_wide_lev.lag[order(ctry_index_wide_lev.lag$continent),]
#ctry_index_wide$`2007`

ctry_index_wide.ord.lag$index <- seq(1:NROW(ctry_index_wide.ord.lag))
ctry_index_wide.ord_lev.lag$index <- seq(1:NROW(ctry_index_wide.ord_lev.lag))


ctry_index_wide.mat.lag <- as.matrix(ctry_index_wide.ord.lag[,3:NCOL(ctry_index_wide.lag)])
row.names(ctry_index_wide.mat.lag) <- ctry_index_wide.ord.lag$iso

ctry_index_wide.mat_lev.lag <- as.matrix(ctry_index_wide.ord_lev.lag[,3:NCOL(ctry_index_wide_lev.lag)])
row.names(ctry_index_wide.mat_lev.lag) <- ctry_index_wide.ord_lev.lag$iso


heatmap(ctry_index_wide.mat.lag, scale="none", Rowv=NA, Colv = NA, col=c("blue", "gray85",  "red"))






colors_lev = c("blue", "gray78", "red")
colors = structure(c("blue", "gray78", "red"), names = c("-1", "0", "1"))

mode(ctry_index_wide.mat.lag) = "character"

#pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/heat1_lag.pdf", height=10, width=8)
pdf(here("data-raw/projections/out/heat1_lag.pdf"), height=10, width=8)


Heatmap(ctry_index_wide.mat.lag,
        row_split = ctry_index_wide.ord.lag$continent,
        column_order =  sort(colnames(ctry_index_wide.mat.lag)),
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
coef_m2.isat.lag <- coefficients(m2.isat_L1)

#coef_m2.lag <- coefficients(m1.arx.lag)


coef_m2.lag <- coefficients(dist1.lag$ols)[rel.coef.lag]

qlow <- 0.25
qmed <- 0.5
qup <- 0.75

loginc_lower <- quantile(log(gdp_pc), probs=qlow, na.rm=TRUE)
loginc_med <- quantile(log(gdp_pc), probs=qmed, na.rm=TRUE)
loginc_upper <- quantile(log(gdp_pc), probs=qup, na.rm=TRUE)


#quantile(log(L1.gdp_pc), probs=qlow, na.rm=TRUE)


temp_lower <- temp[log(gdp_pc) < loginc_lower]

temp_mid <- temp[ loginc_lower < log(gdp_pc) & log(gdp_pc) < loginc_upper]

temp_upper <- temp[  log(gdp_pc) > loginc_upper]



temp_seq <- seq(-5, 30, 1)

imp_lower <- (coef_m2['temp']+coef_m2['temp_loggdp']*loginc_lower)*temp_seq + (coef_m2['temp2']+coef_m2['temp2_loggdp']*loginc_lower)*temp_seq^2
imp_med <- (coef_m2['temp']+coef_m2['temp_loggdp']*loginc_med)*temp_seq + (coef_m2['temp2']+coef_m2['temp2_loggdp']*loginc_med)*temp_seq^2
imp_upper <- (coef_m2['temp']+coef_m2['temp_loggdp']*loginc_upper)*temp_seq + (coef_m2['temp2']+coef_m2['temp2_loggdp']*loginc_upper)*temp_seq^2

imp_lower.lag <- (coef_m2.lag['temp']+coef_m2.lag['temp_L1.loggdp']*loginc_lower)*temp_seq + (coef_m2.lag['temp2']+coef_m2.lag['temp2_L1.loggdp']*loginc_lower)*temp_seq^2
imp_med.lag <- (coef_m2.lag['temp']+coef_m2.lag['temp_L1.loggdp']*loginc_med)*temp_seq + (coef_m2.lag['temp2']+coef_m2.lag['temp2_L1.loggdp']*loginc_med)*temp_seq^2
imp_upper.lag <- (coef_m2.lag['temp']+coef_m2.lag['temp_L1.loggdp']*loginc_upper)*temp_seq + (coef_m2.lag['temp2']+coef_m2.lag['temp2_L1.loggdp']*loginc_upper)*temp_seq^2



imp_lower_isat <- (coef_m2.isat['temp']+coef_m2.isat['temp_loggdp']*loginc_lower)*temp_seq + (coef_m2.isat['temp2']+coef_m2.isat['temp2_loggdp']*loginc_lower)*temp_seq^2
imp_med_isat <- (coef_m2.isat['temp']+coef_m2.isat['temp_loggdp']*loginc_med)*temp_seq + (coef_m2.isat['temp2']+coef_m2.isat['temp2_loggdp']*loginc_med)*temp_seq^2
imp_upper_isat <- (coef_m2.isat['temp']+coef_m2.isat['temp_loggdp']*loginc_upper)*temp_seq + (coef_m2.isat['temp2']+coef_m2.isat['temp2_loggdp']*loginc_upper)*temp_seq^2

imp_lower_isat.lag <- (coef_m2.isat.lag['temp']+coef_m2.isat.lag['temp_L1.loggdp']*loginc_lower)*temp_seq + (coef_m2.isat.lag['temp2']+coef_m2.isat.lag['temp2_L1.loggdp']*loginc_lower)*temp_seq^2
imp_med_isat.lag <- (coef_m2.isat.lag['temp']+coef_m2.isat.lag['temp_L1.loggdp']*loginc_med)*temp_seq + (coef_m2.isat.lag['temp2']+coef_m2.isat.lag['temp2_L1.loggdp']*loginc_med)*temp_seq^2
imp_upper_isat.lag <- (coef_m2.isat.lag['temp']+coef_m2.isat.lag['temp_L1.loggdp']*loginc_upper)*temp_seq + (coef_m2.isat.lag['temp2']+coef_m2.isat.lag['temp2_L1.loggdp']*loginc_upper)*temp_seq^2



imp_lower_sc <- imp_lower - max(imp_lower)
imp_med_sc <- imp_med - max(imp_med)
imp_upper_sc <- imp_upper - max(imp_upper)

imp_lower_sc.lag <- imp_lower.lag - max(imp_lower.lag)
imp_med_sc.lag <- imp_med.lag - max(imp_med.lag)
imp_upper_sc.lag <- imp_upper.lag - max(imp_upper.lag)


imp_lower_sc_isat <- imp_lower_isat - max(imp_lower_isat)
imp_med_sc_isat <- imp_med_isat - max(imp_med_isat)
imp_upper_sc_isat <- imp_upper_isat - max(imp_upper_isat)


imp_lower_sc_isat.lag <- imp_lower_isat.lag - max(imp_lower_isat.lag)
imp_med_sc_isat.lag <- imp_med_isat.lag - max(imp_med_isat.lag)
imp_upper_sc_isat.lag <- imp_upper_isat.lag - max(imp_upper_isat.lag)


pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/isat_adapt_impacts.pdf", height=8, width=12)

par(mfrow=c(1,3))

plot(temp_seq, imp_lower_sc, type="l", ylim=c(-0.3, 0.01), col="blue", main="Income-Based Adaptation (Lower)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
lines(temp_seq, imp_upper_sc, col="gray75", lwd=1)
lines(temp_seq, imp_med_sc, col="gray75", lwd=1)

text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

lines(temp_seq, imp_lower_sc_isat, col="blue", lwd=5, lty=4)
lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)


plot(temp_seq, imp_med_sc, type="l", ylim=c(-0.3, 0.01), col="#238b45", main="Income-Based Adaptation (Middle)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
lines(temp_seq, imp_upper_sc, col="gray75", lwd=1)
lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)

text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_med_sc_isat, col="#238b45", lwd=5, lty=4)

plot(temp_seq, imp_upper_sc, type="l", ylim=c(-0.3, 0.01), col="red", main="Income-Based Adaptation (Upper)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
lines(temp_seq, imp_med_sc, col="gray75", lwd=1)

text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat, col="red", lwd=5, lty=4)
lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)

dev.off()



pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/isat_adapt_impacts_lag.pdf", height=8, width=12)

par(mfrow=c(1,3))

plot(temp_seq, imp_lower_sc.lag, type="l", ylim=c(-0.3, 0.01), col="blue", main="Income-Based Adaptation (Lower)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
lines(temp_seq, imp_upper_sc.lag, col="gray75", lwd=1)
lines(temp_seq, imp_med_sc.lag, col="gray75", lwd=1)

text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

lines(temp_seq, imp_lower_sc_isat.lag, col="blue", lwd=5, lty=4)
lines(temp_seq, imp_upper_sc_isat.lag, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_med_sc_isat.lag, col="gray75", lwd=1, lty=2)


plot(temp_seq, imp_med_sc.lag, type="l", ylim=c(-0.3, 0.01), col="#238b45", main="Income-Based Adaptation (Middle)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
lines(temp_seq, imp_upper_sc.lag, col="gray75", lwd=1)
lines(temp_seq, imp_lower_sc.lag, col="gray75", lwd=1)

text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

lines(temp_seq, imp_lower_sc_isat.lag, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat.lag, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_med_sc_isat.lag, col="#238b45", lwd=5, lty=4)

plot(temp_seq, imp_upper_sc.lag, type="l", ylim=c(-0.3, 0.01), col="red", main="Income-Based Adaptation (Upper)", xlab="Temp", ylab="GDP Per Capita Growth", lwd=5)
lines(temp_seq, imp_lower_sc.lag, col="gray75", lwd=1)
lines(temp_seq, imp_med_sc.lag, col="gray75", lwd=1)

text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

lines(temp_seq, imp_lower_sc_isat.lag, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat.lag, col="red", lwd=5, lty=4)
lines(temp_seq, imp_med_sc_isat.lag, col="gray75", lwd=1, lty=2)


dev.off()



##########################################
####### Comparing Coefficients ##############

############## Plot Different Coefficients

iis.coef <- dist1$iis$mean.results[rel.coef,"coef"]
iis.se <- dist1$iis$mean.results[rel.coef,"std.error"]

iis.coef.lag <- dist1.lag$iis$mean.results[rel.coef.lag,"coef"]
iis.se.lag <- dist1.lag$iis$mean.results[rel.coef.lag,"std.error"]

ols.coef <- dist1$ols$mean.results[rel.coef,"coef"]
ols.se <- dist1$ols$mean.results[rel.coef,"std.error"]

ols.coef.lag <- dist1.lag$ols$mean.results[rel.coef.lag,"coef"]
ols.se.lag <- dist1.lag$ols$mean.results[rel.coef.lag,"std.error"]

coef.plot <- data.frame(matrix(NA, nrow=NROW(rel.coef)), ncol=2)
names(coef.plot) <- c("coef", "iis.coef")

coef.plot$coef <- rel.coef
coef.plot$ols.coef <- ols.coef
coef.plot$ols.se <- ols.se
coef.plot$iis.coef <- iis.coef
coef.plot$iis.se <- iis.se
coef.plot$iis.coef.lag <- iis.coef.lag
coef.plot$iis.se.lag <- iis.se.lag
coef.plot$ols.coef.lag <- ols.coef.lag
coef.plot$ols.se.lag <- ols.se.lag

coef.plot$iis.ci.p025 <- coef.plot$iis.coef - 1.96*coef.plot$iis.se
coef.plot$iis.ci.p975 <- coef.plot$iis.coef + 1.96*coef.plot$iis.se

coef.plot$iis.ci.p025.lag <- coef.plot$iis.coef.lag - 1.96*coef.plot$iis.se.lag
coef.plot$iis.ci.p975.lag <- coef.plot$iis.coef.lag + 1.96*coef.plot$iis.se.lag

coef.plot$ols.ci.p025 <- coef.plot$ols.coef - 1.96*coef.plot$ols.se
coef.plot$ols.ci.p975 <- coef.plot$ols.coef + 1.96*coef.plot$ols.se

coef.plot$ols.ci.p025.lag <- coef.plot$ols.coef.lag - 1.96*coef.plot$ols.se.lag
coef.plot$ols.ci.p975.lag <- coef.plot$ols.coef.lag + 1.96*coef.plot$ols.se.lag



coef.plot$iis.ci.p005 <- coef.plot$iis.coef - 2.57*coef.plot$iis.se
coef.plot$iis.ci.p995 <- coef.plot$iis.coef + 2.57*coef.plot$iis.se

coef.plot$iis.ci.p005.lag <- coef.plot$iis.coef.lag - 2.57*coef.plot$iis.se.lag
coef.plot$iis.ci.p995.lag <- coef.plot$iis.coef.lag + 2.57*coef.plot$iis.se.lag

coef.plot$ols.ci.p005 <- coef.plot$ols.coef - 2.57*coef.plot$ols.se
coef.plot$ols.ci.p995 <- coef.plot$ols.coef + 2.57*coef.plot$ols.se

coef.plot$ols.ci.p005.lag <- coef.plot$ols.coef.lag - 2.57*coef.plot$ols.se.lag
coef.plot$ols.ci.p995.lag <- coef.plot$ols.coef.lag + 2.57*coef.plot$ols.se.lag


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

pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/coef_dist_eff_v1.pdf", height=9, width=12)

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


plot(temp_seq, imp_lower_sc, type="l", ylim=c(-0.3, 0.01) , xlim=xlim_temp, col=ols.col, main="Income-Based Adaptation (Lower)", xlab="Deg. C", ylab="Delta log(GDPpc)", lwd=5)
#lines(temp_seq, imp_upper_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_med_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
#text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
#text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("Income Quantile: ", qlow, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)

lines(temp_seq, imp_lower_sc_isat, col=iis.col, lwd=5, lty=5)
#lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
#lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)



plot(temp_seq, imp_med_sc, type="l", ylim=c(-0.3, 0.01), col=ols.col, xlim=xlim_temp, main="Income-Based Adaptation (Middle)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
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
lines(temp_seq, imp_med_sc_isat, col=iis.col, lwd=5, lty=5)




plot(temp_seq, imp_upper_sc, type="l", ylim=c(-0.3, 0.01), xlim=xlim_temp, col=ols.col, main="Income-Based Adaptation (Upper)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
#lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_med_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
text(label=paste("Income Quantile:", qup, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)
# text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
# text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

#lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat, col=iis.col, lwd=5, lty=5)
#lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)




hist(temp_mid, xlim=xlim_temp, breaks=15,  col="gray85", main="Temp. Distribution in Income Range")
hist(temp_lower, xlim=xlim_temp, breaks=15, main="", yaxt="n", col="gray85")
hist(temp_upper, xlim=xlim_temp, breaks=15, main="", yaxt="n", col="gray85")

#par(mar = c(4,4,2,0), oma = c(3,3.5,1.5,1.5), las =1, mfrow=c(1, NROW(rel.coef)))


for (i in 1: NROW(rel.coef)){
  #i <- 1
  par(mar = c(2,4,3,0))

  yint <- max(max(abs(coef.plot$iis.ci.p975[i]),  abs(coef.plot$iis.ci.p025[i]) ), max(abs(coef.plot$ols.ci.p975[i]),  abs(coef.plot$ols.ci.p025[i]) ))
  yscale <- 1.15
  barwidth <- 0.003

  ylim <- c(-yint*yscale, yint*yscale)

  ylab <- ""
  if (i==1){
    ylab="Estimated Coefficient"
  }

  plot(coef.plot$plot.index[i], coef.plot$ols.coef[i], ylim=ylim,  xlim=c(0.45,0.7),  main=coef.plot$coef[i], pch=19, cex=3,   ylab=ylab, xlab="", xaxt="n", col=ols.col)
  points(coef.plot$plot.index[i]+coef.offset, coef.plot$iis.coef[i], pch=19, cex=3,  col=iis.col)
  rect(ybottom=coef.plot$ols.ci.p025[i], ytop=coef.plot$ols.ci.p975[i], xleft=coef.plot$plot.index[i]-barwidth, xright=coef.plot$plot.index[i]+barwidth,  density = NULL, border = ols.col, col=ols.col)
  rect(ybottom=coef.plot$iis.ci.p025[i], ytop=coef.plot$iis.ci.p975[i], xleft=coef.plot$plot.index[i]-barwidth+coef.offset, xright=coef.plot$plot.index[i]+barwidth+coef.offset,  density = NULL, border = iis.col, col=iis.col)
  axis(1, at=c(coef.plot$plot.index[i], coef.plot$plot.index[i]+coef.offset), labels=c("OLS", "Robust IIS"), cex.axis=1.3)
  if (i==1){
    legend(x=0.45, y=0.065, legend=c("OLS", "Robust IIS"),
           col=c(ols.col, iis.col), lty=c(1, 1), lwd=c(4,4), cex=1.1, bg="transparent", bty = "n", y.intersp=0.7)

  }
  abline(h=0, lty=2, col="gray25", lwd=1.5)

}

dev.off()



############# Split into two


pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/coef_v2.pdf", height=5, width=10)


par(mfrow=c(1,4))
par(mar = c(3,4.5,3,0), oma = c(5,4,0.5,0.5), las =1)


for (i in 1: NROW(rel.coef)){
  #i <- 1
  par(mar = c(2,4,3,0))

  yint <- max(max(abs(coef.plot$iis.ci.p975[i]),  abs(coef.plot$iis.ci.p025[i]) ), max(abs(coef.plot$ols.ci.p975[i]),  abs(coef.plot$ols.ci.p025[i]) ))
  yscale <- 1.15
  barwidth <- 0.003

  ylim <- c(-yint*yscale, yint*yscale)

  ylab <- ""
  if (i==1){
    ylab="Estimated Coefficient"
  }

  plot(coef.plot$plot.index[i], coef.plot$ols.coef[i], ylim=ylim,  xlim=c(0.45,0.7),  main=coef.plot$coef[i], pch=19, cex=3,   ylab=ylab, xlab="", xaxt="n", col=ols.col)
  points(coef.plot$plot.index[i]+coef.offset, coef.plot$iis.coef[i], pch=19, cex=3,  col=iis.col)
  rect(ybottom=coef.plot$ols.ci.p025[i], ytop=coef.plot$ols.ci.p975[i], xleft=coef.plot$plot.index[i]-barwidth, xright=coef.plot$plot.index[i]+barwidth,  density = NULL, border = ols.col, col=ols.col)
  rect(ybottom=coef.plot$iis.ci.p025[i], ytop=coef.plot$iis.ci.p975[i], xleft=coef.plot$plot.index[i]-barwidth+coef.offset, xright=coef.plot$plot.index[i]+barwidth+coef.offset,  density = NULL, border = iis.col, col=iis.col)
  axis(1, at=c(coef.plot$plot.index[i], coef.plot$plot.index[i]+coef.offset), labels=c("OLS", "Robust IIS"), cex.axis=1.3)
  if (i==1){
    legend(x=0.45, y=0.065, legend=c("OLS", "Robust IIS"),
           col=c(ols.col, iis.col), lty=c(1, 1), lwd=c(4,4), cex=1.1, bg="transparent", bty = "n", y.intersp=0.7)

  }
  abline(h=0, lty=2, col="gray25", lwd=1.5)

}

dev.off()




############# Split into two


pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/coef_v2_lag.pdf", height=5, width=10)


par(mfrow=c(1,4))
par(mar = c(3,4.5,3,0), oma = c(5,4,0.5,0.5), las =1)


for (i in 1: NROW(rel.coef)){
  #i <- 1
  par(mar = c(2,4,3,0))

  yint <- max(max(abs(coef.plot$iis.ci.p975.lag[i]),  abs(coef.plot$iis.ci.p025.lag[i]) ), max(abs(coef.plot$ols.ci.p975.lag[i]),  abs(coef.plot$ols.ci.p025.lag[i]) ))
  yscale <- 1.15
  barwidth <- 0.003

  ylim <- c(-yint*yscale, yint*yscale)

  ylab <- ""
  if (i==1){
    ylab="Estimated Coefficient"
  }

  plot(coef.plot$plot.index[i], coef.plot$ols.coef.lag[i], ylim=ylim,  xlim=c(0.45,0.7),  main=coef.plot$coef[i], pch=19, cex=3,   ylab=ylab, xlab="", xaxt="n", col=ols.col)
  points(coef.plot$plot.index[i]+coef.offset, coef.plot$iis.coef.lag[i], pch=19, cex=2,  col=iis.col)
  rect(ybottom=coef.plot$ols.ci.p025.lag[i], ytop=coef.plot$ols.ci.p975.lag[i], xleft=coef.plot$plot.index[i]-barwidth, xright=coef.plot$plot.index[i]+barwidth,  density = NULL, border = ols.col, col=ols.col)
  rect(ybottom=coef.plot$iis.ci.p025.lag[i], ytop=coef.plot$iis.ci.p975.lag[i], xleft=coef.plot$plot.index[i]-barwidth+coef.offset, xright=coef.plot$plot.index[i]+barwidth+coef.offset,  density = NULL, border = iis.col, col=iis.col)
  axis(1, at=c(coef.plot$plot.index[i], coef.plot$plot.index[i]+coef.offset), labels=c("OLS", "Robust IIS"), cex.axis=1.3)
  if (i==1){
    legend(x=0.45, y=0.065, legend=c("OLS", "Robust IIS"),
           col=c(ols.col, iis.col), lty=c(1, 1), lwd=c(4,4), cex=1.1, bg="transparent", bty = "n", y.intersp=0.7)

  }
  abline(h=0, lty=2, col="gray25", lwd=1.5)

}

dev.off()



########### Impact function




pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/eff_v1.pdf", height=8, width=12)



layout.matrix.r1 <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3), nrow = 1, ncol = 9, byrow = T)
layout.matrix.r2 <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3), nrow = 1, ncol = 9, byrow = T)
layout.matrix.r3 <- matrix(c(4, 4, 4, 5, 5, 5, 6, 6,6), nrow = 1, ncol = 9, byrow = T)

layout.matrix <- rbind(layout.matrix.r1, layout.matrix.r2, layout.matrix.r3)



par(mar = c(3,4.5,3,0), oma = c(5,4,0.5,0.5), las =1)

par(mar = c(2,0.3,3,0))
layout(mat = layout.matrix) # Widths of the two columns



#par(mfrow=c(1,1))

plot(temp_seq, imp_lower_sc, type="l", ylim=c(-0.3, 0.01) , xlim=xlim_temp, col=ols.col, main="Income-Based Adaptation (Lower)", xlab="Deg. C", ylab="Delta log(GDPpc)", lwd=5)
#lines(temp_seq, imp_upper_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_med_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
#text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
#text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("Income Quantile: ", qlow, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)

lines(temp_seq, imp_lower_sc_isat, col=iis.col, lwd=5, lty=5)
#lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
#lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)



plot(temp_seq, imp_med_sc, type="l", ylim=c(-0.3, 0.01), col=ols.col, xlim=xlim_temp, main="Income-Based Adaptation (Middle)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
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
lines(temp_seq, imp_med_sc_isat, col=iis.col, lwd=5, lty=5)




plot(temp_seq, imp_upper_sc, type="l", ylim=c(-0.3, 0.01), xlim=xlim_temp, col=ols.col, main="Income-Based Adaptation (Upper)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
#lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_med_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
text(label=paste("Income Quantile:", qup, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)
# text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
# text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

#lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat, col=iis.col, lwd=5, lty=5)
#lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)




hist(temp_mid, xlim=xlim_temp, breaks=15,  col="gray85", main="Temp. Distribution in Income Range")
hist(temp_lower, xlim=xlim_temp, breaks=15, main="", yaxt="n", col="gray85")
hist(temp_upper, xlim=xlim_temp, breaks=15, main="", yaxt="n", col="gray85")

#par(mar = c(4,4,2,0), oma = c(3,3.5,1.5,1.5), las =1, mfrow=c(1, NROW(rel.coef)))

dev.off()






pdf("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/application/adaptation damage/eff_v1_lag.pdf", height=6, width=12)

#layout.matrix <- matrix(c(1, 1, 1, 2,2,2 ,3, 3, 4), nrow = 9, ncol = 1)

#layout.matrix <- matrix(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7), nrow = 2, ncol = 12, byrow = T)


layout.matrix.r1 <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3), nrow = 1, ncol = 9, byrow = T)
layout.matrix.r2 <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3), nrow = 1, ncol = 9, byrow = T)
layout.matrix.r3 <- matrix(c(4, 4, 4, 5, 5, 5, 6, 6,6), nrow = 1, ncol = 9, byrow = T)

layout.matrix <- rbind(layout.matrix.r1, layout.matrix.r2, layout.matrix.r3)



par(mar = c(3,4.5,3,0), oma = c(5,4,0.5,0.5), las =1)

par(mar = c(2,0.3,3,0))
layout(mat = layout.matrix) # Widths of the two columns



#par(mfrow=c(1,1))

plot(temp_seq, imp_lower_sc.lag, type="l", ylim=c(-0.3, 0.01) , xlim=xlim_temp, col=ols.col, main="Income-Based Adaptation (Lower)", xlab="Deg. C", ylab="Delta log(GDPpc)", lwd=5)
#lines(temp_seq, imp_upper_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_med_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
#text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
#text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
text(label=paste("Income Quantile: ", qlow, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)

lines(temp_seq, imp_lower_sc_isat.lag, col=iis.col, lwd=5, lty=5)
#lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
#lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)



plot(temp_seq, imp_med_sc.lag, type="l", ylim=c(-0.3, 0.01), col=ols.col, xlim=xlim_temp, main="Income-Based Adaptation (Middle)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
#lines(temp_seq, imp_upper_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
#text(label=paste("q", qup, sep=""), col="red", x=10, y=-0.25)
text(label=paste("Income Quantile: ", qmed, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)
#text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

legend(x=-7, y=-0.02, legend=c("OLS", "Robust IIS"),
       col=c(ols.col, iis.col), lty=c(1, 5), lwd=c(3,3), cex=1.6, bg="transparent", bty = "n", y.intersp=0.7)


#lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
#lines(temp_seq, imp_upper_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_med_sc_isat.lag, col=iis.col, lwd=5, lty=5)




plot(temp_seq, imp_upper_sc.lag, type="l", ylim=c(-0.3, 0.01), xlim=xlim_temp, col=ols.col, main="Income-Based Adaptation (Upper)", xlab="Deg. C", ylab="", yaxt="n", lwd=5)
#lines(temp_seq, imp_lower_sc, col="gray75", lwd=1)
#lines(temp_seq, imp_med_sc, col="gray75", lwd=1)
abline(h=0, col="gray35", lty=2)
text(label=paste("Income Quantile:", qup, sep=""), cex=1.6, col="Gray35", x=10, y=-0.27)
# text(label=paste("q", qmed, sep=""), col="#238b45", x=10, y=-0.27)
# text(label=paste("q", qlow, sep=""), col="blue", x=10, y=-0.29)

#lines(temp_seq, imp_lower_sc_isat, col="gray75", lwd=1, lty=2)
lines(temp_seq, imp_upper_sc_isat.lag, col=iis.col, lwd=5, lty=5)
#lines(temp_seq, imp_med_sc_isat, col="gray75", lwd=1, lty=2)




hist(temp_mid, xlim=xlim_temp, breaks=15,  col="gray85", main="Temp. Distribution in Income Range")
hist(temp_lower, xlim=xlim_temp, breaks=15, main="", yaxt="n", col="gray85")
hist(temp_upper, xlim=xlim_temp, breaks=15, main="", yaxt="n", col="gray85")

#par(mar = c(4,4,2,0), oma = c(3,3.5,1.5,1.5), las =1, mfrow=c(1, NROW(rel.coef)))

dev.off()



