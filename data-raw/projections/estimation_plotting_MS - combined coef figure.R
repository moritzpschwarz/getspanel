

library(here)
library(tidyverse)
library(gets)
library(getspanel)
library(xtable)
library(broom)

rm(list=ls())
load(here("data-raw/projections/m2.RData"))
load(here("data-raw/projections/am2.RData"))
load(here("data-raw/projections/am2_L1.RData"))

dat <- vroom::vroom(file = here("data-raw/projections/damage_curve_country_dataset_timetrends_updated02-19.csv"))
m2_data <- vroom::vroom(file = here("data-raw/projections/m2_data.csv"))
am2_data <- vroom::vroom(file = here("data-raw/projections/am2_data.csv"))
am2_L1_data <- vroom::vroom(file = here("data-raw/projections/am2_L1_data.csv"))



#pdf(here(paste0("data-raw/projections/out/",est_version,"coef.adapt.pdf")), height=5, width=10)

overall_list <- list()
for(est_version in c(0.05, 0.01, 0.001)){
  load(here(paste0("data-raw/projections/m2.isat.",est_version,".RData")))
  load(here(paste0("data-raw/projections/am2.isat.",est_version,".RData")))
  load(here(paste0("data-raw/projections/am2.isat_L1.",est_version,".RData")))

  rel.coef <- c("temp", "temp_2")
  rel.coef.adapt <- c("temp", "temp_2", "temp_int","temp_2_int")
  rel.coef.adapt.L1 <- rel.coef.adapt
  #rel.coef <- c("temp", "temp2", "temp_loggdp", "temp2_loggdp")
  #rel.coef.lag <- c("temp", "temp2", "temp_L1.loggdp", "temp2_L1.loggdp")


  dist1 <- getspanel::distorttest(m2.isat,  coef=rel.coef)
  dist1.adapt <- getspanel::distorttest(am2.isat,  coef=rel.coef.adapt)
  dist1.adapt.L1 <- getspanel::distorttest(am2.isat_L1,  coef=rel.coef.adapt)

  temp_seq <- seq(-5, 30, 1)


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

  coef.plot$est_version <- est_version

  assign(x = paste0("coef.plot_",est_version),coef.plot)

}


pdf(here(paste0("data-raw/projections/out/allestversions.coef.adapt.pdf")), height=5, width=10)

par(mfrow=c(1,4))
par(mar = c(3,0.5,3,0.5), oma = c(5,0.5,2,0.5), las =1)
for (i in 1:4){
  #i <- 1
  par(mar = c(0.5,3,3,0.5))

  yint <- max(max(abs(coef.plot$iis.ci.p975.adapt[i]),  abs(coef.plot$iis.ci.p025.adapt[i]) ), max(abs(coef.plot$ols.ci.p975.adapt[i]),  abs(coef.plot$ols.ci.p025.adapt[i]) ))
  yscale <- 1.15
  barwidth <- 0.003

  ylim <- c(-yint*yscale, yint*yscale)

  ylab <- ""
  if (i==1){
    ylab="Estimated Coefficient"
  }

  # est_version = 0.05
  coef.plot <- coef.plot_0.05
  j = 0.05

  plot(coef.plot$plot.index[i],
       coef.plot$ols.coef.adapt[i],
       ylim=ylim,  xlim=c(0.45,0.75),  main=coef.plot$coef[i], pch=19, cex=3,   ylab=ylab,
       xlab="", xaxt="n", col=ols.col)

  points(coef.plot$plot.index[i]+coef.offset+ ifelse(j == 0.05,0.05,ifelse(j == 0.001,-0.05,0)),
         coef.plot$iis.coef.adapt[i], pch=19, cex=3,  col=iis.col)
  rect(ybottom=coef.plot$ols.ci.p025.adapt[i],
       ytop=coef.plot$ols.ci.p975.adapt[i],

       xleft=coef.plot$plot.index[i]- barwidth,
       xright=coef.plot$plot.index[i] + barwidth,
       density = NULL, border = ols.col, col=ols.col)
  rect(ybottom=coef.plot$iis.ci.p025.adapt[i],
       ytop=coef.plot$iis.ci.p975.adapt[i],

       xleft=coef.plot$plot.index[i]-barwidth+ ifelse(j == 0.05,0.05,ifelse(j == 0.001,-0.05,0))+coef.offset,
       xright=coef.plot$plot.index[i]+barwidth+coef.offset+ ifelse(j == 0.05,0.05,ifelse(j == 0.001,-0.05,0)),
       density = NULL, border = iis.col, col=iis.col)



  # est_version = 0.01
  coef.plot <- coef.plot_0.01
  j = 0.01

  # points(coef.plot$plot.index[i],
  #        coef.plot$ols.coef.adapt[i],
  #        ylim=ylim,  xlim=c(0.45,0.7),  main=coef.plot$coef[i], pch=19, cex=3,   ylab=ylab,
  #        xlab="", xaxt="n", col=ols.col)

  points(coef.plot$plot.index[i]+coef.offset+ ifelse(j == 0.05,0.05,ifelse(j == 0.001,-0.05,0)),
         coef.plot$iis.coef.adapt[i], pch=19, cex=3,  col=iis.col)
  # rect(ybottom=coef.plot$ols.ci.p025.adapt[i],
  #      ytop=coef.plot$ols.ci.p975.adapt[i],
  #
  #      xleft=coef.plot$plot.index[i]- barwidth,
  #      xright=coef.plot$plot.index[i] + barwidth,
  #      density = NULL, border = ols.col, col=ols.col)
  rect(ybottom=coef.plot$iis.ci.p025.adapt[i],
       ytop=coef.plot$iis.ci.p975.adapt[i],

       xleft=coef.plot$plot.index[i]-barwidth+ ifelse(j == 0.05,0.05,ifelse(j == 0.001,-0.05,0))+coef.offset,
       xright=coef.plot$plot.index[i]+barwidth+coef.offset+ ifelse(j == 0.05,0.05,ifelse(j == 0.001,-0.05,0)),
       density = NULL, border = iis.col, col=iis.col)



  # est_version = 0.001
  coef.plot <- coef.plot_0.001
  j = 0.001

  # points(coef.plot$plot.index[i],
  #        coef.plot$ols.coef.adapt[i],
  #        ylim=ylim,  xlim=c(0.45,0.7),  main=coef.plot$coef[i], pch=19, cex=3,   ylab=ylab,
  #        xlab="", xaxt="n", col=ols.col)
  #
  points(coef.plot$plot.index[i]+coef.offset+ ifelse(j == 0.05,0.05,ifelse(j == 0.001,-0.05,0)),
         coef.plot$iis.coef.adapt[i], pch=19, cex=3,  col=iis.col)
  # rect(ybottom=coef.plot$ols.ci.p025.adapt[i],
  #      ytop=coef.plot$ols.ci.p975.adapt[i],
  #
  #      xleft=coef.plot$plot.index[i]- barwidth,
  #      xright=coef.plot$plot.index[i] + barwidth,
  #      density = NULL, border = ols.col, col=ols.col)
  rect(ybottom=coef.plot$iis.ci.p025.adapt[i],
       ytop=coef.plot$iis.ci.p975.adapt[i],

       xleft=coef.plot$plot.index[i]-barwidth+ ifelse(j == 0.05,0.05,ifelse(j == 0.001,-0.05,0))+coef.offset,
       xright=coef.plot$plot.index[i]+barwidth+coef.offset+ ifelse(j == 0.05,0.05,ifelse(j == 0.001,-0.05,0)),
       density = NULL, border = iis.col, col=iis.col)
  axis(1, at=c(coef.plot$plot.index[i], coef.plot$plot.index[i]+coef.offset), labels=c("OLS", "Robust IIS"), cex.axis=1.3)
  axis(1, at=c(c(0.45, 0.5, 0.55),c(0.45, 0.5, 0.55)+coef.offset),  line = 2,
       labels=c("","","","0.1%","1%", "5%"), cex.axis=1, tick = FALSE)


  if (i==1){
    legend(x=0.45, y=-0.065, legend=c("OLS", "Robust IIS"),
           col=c(ols.col, iis.col), lty=c(1, 1), lwd=c(4,4), cex=1.1, bg="transparent", bty = "n", y.intersp=0.7)

  }
  abline(h=0, lty=2, col="gray25", lwd=1.5)

}

dev.off()

