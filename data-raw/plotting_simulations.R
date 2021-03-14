library(here)

########### Analyse Simulation: compare p-values to 0.01 and 0.05 cut-offs

sum_list <- list()
sum_rej_05_list <- list()
sum_rej_01_list <- list()

load(here("data-raw","simulations","spec_list.RData"))
spec_n <- NROW(specs)

for (l in 1:spec_n){

  # l <- 1
  # res <- list.res[[l]]
  if(file.exists(here("data-raw","simulations",paste0(paste0(specs[l,],collapse = "_"),".RData")))){
    load(here("data-raw","simulations",paste0(paste0(specs[l,],collapse = "_"),".RData")))
  } else{
    print(paste0("File Specification ",l," skipped - not found."))
    next
  }


  res.rej05 <- res[,2:NCOL(res)]
  res.rej05$rej <- NA
  res.rej05$rej.L2.boot <- NA
  res.rej05$rej.L1.boot <- NA
  res.rej05$rej.dist.boot <- NA

  res.rej05$rej.prop.test <- NA
  res.rej05$rej.prop.test.boot <- NA
  res.rej05$rej.prop.boot <- NA



  res.rej05$rej[res.rej05$is.dist1.p > 0.05] <- 999
  res.rej05$rej[res.rej05$is.dist1.p < 0.05] <- 1
  res.rej05$rej[res.rej05$rej > 1] <- 0

  res.rej05$rej.L2.boot[res.rej05$is.dist1.boot.L2.p > 0.05] <- 999
  res.rej05$rej.L2.boot[res.rej05$is.dist1.boot.L2.p < 0.05] <- 1
  res.rej05$rej.L2.boot[res.rej05$rej.L2.boot > 1] <- 0

  res.rej05$rej.L1.boot[res.rej05$is.dist1.boot.L1.p > 0.05] <- 999
  res.rej05$rej.L1.boot[res.rej05$is.dist1.boot.L1.p < 0.05] <- 1
  res.rej05$rej.L1.boot[res.rej05$rej.L1.boot > 1] <- 0

  res.rej05$rej.dist.boot[res.rej05$is.dist1.boot.dist.p > 0.05] <- 999
  res.rej05$rej.dist.boot[res.rej05$is.dist1.boot.dist.p < 0.05] <- 1
  res.rej05$rej.dist.boot[res.rej05$rej.dist.boot > 1] <- 0


  ####proportion test
  res.rej05$rej.prop.test[res.rej05$is.prop.test.p > 0.05] <- 999
  res.rej05$rej.prop.test[res.rej05$is.prop.test.p < 0.05] <- 1
  res.rej05$rej.prop.test[res.rej05$rej.prop.test > 1] <- 0

  res.rej05$rej.prop.test.boot[res.rej05$is.prop.boot.test.p > 0.05] <- 999
  res.rej05$rej.prop.test.boot[res.rej05$is.prop.boot.test.p < 0.05] <- 1
  res.rej05$rej.prop.test.boot[res.rej05$rej.prop.test.boot > 1] <- 0

  res.rej05$rej.prop.boot[res.rej05$is.prop.boot.p > 0.05] <- 999
  res.rej05$rej.prop.boot[res.rej05$is.prop.boot.p < 0.05] <- 1
  res.rej05$rej.prop.boot[res.rej05$rej.prop.boot > 1] <- 0





  res.rej05$id <- res$id
  res.rej05$id.seq <- res$id.seq


  sum_rej_05_list[[l]] <- colMeans(res.rej05, na.rm = TRUE)


  res.rej01 <- res[,2:NCOL(res)]
  res.rej01$rej <- NA
  res.rej01$rej.L2.boot <- NA
  res.rej01$rej.L1.boot <- NA
  res.rej01$rej.dist.boot <- NA

  res.rej01$rej.prop.test <- NA
  res.rej01$rej.prop.test.boot <- NA
  res.rej01$rej.prop.boot <- NA



  res.rej01$rej[res.rej01$is.dist1.p > 0.01] <- 999
  res.rej01$rej[res.rej01$is.dist1.p < 0.01] <- 1
  res.rej01$rej[res.rej01$rej > 1] <- 0

  res.rej01$rej.L2.boot[res.rej01$is.dist1.boot.L2.p > 0.01] <- 999
  res.rej01$rej.L2.boot[res.rej01$is.dist1.boot.L2.p < 0.01] <- 1
  res.rej01$rej.L2.boot[res.rej01$rej.L2.boot > 1] <- 0

  res.rej01$rej.L1.boot[res.rej01$is.dist1.boot.L1.p > 0.01] <- 999
  res.rej01$rej.L1.boot[res.rej01$is.dist1.boot.L1.p < 0.01] <- 1
  res.rej01$rej.L1.boot[res.rej01$rej.L1.boot > 1] <- 0

  res.rej01$rej.dist.boot[res.rej01$is.dist1.boot.dist.p > 0.01] <- 999
  res.rej01$rej.dist.boot[res.rej01$is.dist1.boot.dist.p < 0.01] <- 1
  res.rej01$rej.dist.boot[res.rej01$rej.dist.boot > 1] <- 0

  ###proportion test
  res.rej01$rej.prop.test[res.rej01$is.prop.test.p > 0.01] <- 999
  res.rej01$rej.prop.test[res.rej01$is.prop.test.p < 0.01] <- 1
  res.rej01$rej.prop.test[res.rej01$rej.prop.test > 1] <- 0

  res.rej01$rej.prop.test.boot[res.rej01$is.prop.boot.test.p > 0.01] <- 999
  res.rej01$rej.prop.test.boot[res.rej01$is.prop.boot.test.p < 0.01] <- 1
  res.rej01$rej.prop.test.boot[res.rej01$rej.prop.test.boot > 1] <- 0

  res.rej01$rej.prop.boot[res.rej01$is.prop.boot.p > 0.01] <- 999
  res.rej01$rej.prop.boot[res.rej01$is.prop.boot.p < 0.01] <- 1
  res.rej01$rej.prop.boot[res.rej01$rej.prop.boot > 1] <- 0





  res.rej01$id <- res$id
  res.rej01$id.seq <- res$id.seq

  sum_rej_01_list[[l]] <- colMeans(res.rej01, na.rm = TRUE)


}


sum.05 <- do.call(rbind.data.frame, sum_rej_05_list)
names(sum.05) <- names(sum_rej_05_list[[1]])
sum.05$test.lev <- 0.05

sum.01 <- do.call(rbind.data.frame, sum_rej_01_list)
names(sum.01) <- names(sum_rej_01_list[[1]])
sum.01$test.lev <- 0.01

###combine
sum.01.m <- merge(sum.01, specs, by="id")
sum.05.m <- merge(sum.05, specs, by="id")

sum_tab <- rbind(sum.01.m, sum.05.m)

sum_tab

#
#
# if (alternative){
#   write.csv(sum_tab, here("data-raw/simulations/alt_v1_boot_wprop.csv"), row.names = FALSE)
#   #write.csv(sum_tab, "./simulations/alt_v1_boot_wprop.csv", row.names = FALSE)
#   #  write.csv(uncond, "./simulations/uncond_alt_v2.csv", row.names = FALSE)
#
# } else {
#   write.csv(sum_tab, here("data-raw/simulations/null_v1_boot_wprop.csv"), row.names = FALSE)
#   #write.csv(sum_tab, "./simulations/null_v1_boot_wprop.csv", row.names = FALSE)
#   #  write.csv(uncond, "./simulations/uncond_null_v2.csv", row.names = FALSE)
# }


# Plotting ----------------------------------------------------------------


# Notes:
# AR plots currently only for alternative

# Bootstrap test.lev currently only 0.05
# Same for p_alpha
# Currently only lambda 4

plot_specs <- dplyr::distinct(specs,hypothesis,out_prop, bootstrap, dist, ar, p_alpha, nreg)

plot_specs <- plot_specs[plot_specs$bootstrap==FALSE&plot_specs$hypothesis=="alternative",]

for(i in 1:nrow(plot_specs)){

  print(i)

  if(plot_specs[i,"bootstrap"]==FALSE){ # Null --------------------------------------------------------------------


    if(plot_specs[i,"hypothesis"]=="null"){
      datnull <- sum_tab[sum_tab$hypothesis=="null"&sum_tab$dist=="t3",]

      pdf.width <- 11
      pdf.height <- 5.5

      pdf(here("data-raw/figures/t3_null_1.pdf"), width=pdf.width, height=pdf.height)
      par(mfrow=c(1,3))

      #### Plot 1: Size against number of observations for different levels
      col.01 <- "red"
      col.05 <- "blue"

      datnull$specfam <- 0
      datnull$specfam[datnull$test.lev==0.01 & datnull$p_alpha==0.01 & datnull$nreg==5 & datnull$ar==0] <- 1
      datnull$specfam[datnull$test.lev==0.01 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0] <- 2
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.01 & datnull$nreg==5 & datnull$ar==0] <- 3
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0] <- 4


      plot(datnull$sample[datnull$specfam==1], datnull$rej[datnull$specfam==1 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 520), col=col.01, ylab="Null Rejection Frequency", xlab="Sample Size n", main="Size (Varying Level, P-Alpha, & Sample, Nreg=5)")
      lines(datnull$sample[datnull$specfam==2], datnull$rej[datnull$specfam==2 ], lty=2, type="b",   col=col.01)
      abline(h=0.01, col=col.01)
      text(x=50, y=0.017, label="0.01", col=col.01)

      lines(datnull$sample[datnull$specfam==3], datnull$rej[datnull$specfam==3], lty=1, type="b",  col=col.05)
      lines(datnull$sample[datnull$specfam==4], datnull$rej[datnull$specfam==4 ], lty=2, type="b",  col=col.05)
      abline(h=0.05, col=col.05)
      text(x=50, y=0.057, label="0.05", col=col.05)

      legend(75, 0.4, c("Level: 0.01 (p-alpha=0.01)", "Level: 0.01 (p-alpha=0.05)", "Level: 0.05 (p-alpha=0.01)", "Level: 0.05 (p-alpha=0.05)"),  bg=NA, bty = "n", title.adj=-0.03,
             lty=c(1, 2, 1, 2), col=c(col.01, col.01, col.05, col.05), lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)


      #### Plot 2: Size against number of observations for different number of coefficients

      #datnull$specfam[datnull$test.lev==0.01 & datnull$p_alpha==0.05 & datnull$nreg==1 & datnull$ar==0] <- 5
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==1 & datnull$ar==0] <- 6

      #datnull$specfam[datnull$test.lev==0.01 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0] <- 7
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0] <- 8

      #datnull$specfam[datnull$test.lev==0.01 & datnull$p_alpha==0.05 & datnull$nreg==10 & datnull$ar==0] <- 9
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==10 & datnull$ar==0] <- 10


      col.p1 <- "darkgreen"
      col.p5 <- "purple"
      col.p10 <- "orange"

      plot(datnull$sample[datnull$specfam==6], datnull$rej[datnull$specfam==6], lty=1, type="b", ylim=c(0, 1),  xlim=c(50, 520),  col=col.p1, ylab="Null Rejection Frequency", xlab="Sample Size n", main="Size (Varying Nreg, Level, & Sample, P-alpha=0.05)")
      #lines(datnull$sample[datnull$specfam==6], datnull$rej[datnull$specfam==6], lty=3, type="b",   col=col.p1)

      #lines(datnull$sample[datnull$specfam==7], datnull$rej[datnull$specfam==7], lty=1, type="b",   col=col.p5)
      lines(datnull$sample[datnull$specfam==8], datnull$rej[datnull$specfam==8], lty=1, type="b",   col=col.p5)

      #lines(datnull$sample[datnull$specfam==9], datnull$rej[datnull$specfam==9], lty=1, type="b",   col=col.p10)
      lines(datnull$sample[datnull$specfam==10], datnull$rej[datnull$specfam==10], lty=1, type="b",   col=col.p10)

      legend(75, 0.4, c("NReg=1, (Lev = 0.05)", "NReg=5, (Lev = 0.05)", "NReg=10, (Lev = 0.05)"),  bg=NA, bty = "n", title.adj=-0.03,
             lty=c(1, 1, 1), col=c(col.p1, col.p5, col.p10), lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)

      # abline(h=0.01, col="gray55")
      # text(x=50, y=0.017, label="0.01", col="gray55")

      abline(h=0.05, col="gray55")
      text(x=50, y=0.057, label="0.05", col="gray55")

      #### plot 3: ar effect

      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0] <- 11
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0.5] <- 12
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==1] <- 13

      col.ar0 <- "#fcbba1"
      col.ar05 <- "#fb6a4a"
      col.ar1 <- "#99000d"


      plot(datnull$sample[datnull$specfam==11], datnull$rej[datnull$specfam==11], lty=1, type="b", ylim=c(0, 1),  xlim=c(50, 520), col=col.ar0, ylab="Null Rejection Frequency", xlab="Sample Size n", main="Size (Varying AR, & Sample, P-alpha=0.05, NReg=5, Level=0.05)")
      lines(datnull$sample[datnull$specfam==12], datnull$rej[datnull$specfam==12], lty=1, type="b",   col=col.ar05)
      lines(datnull$sample[datnull$specfam==13], datnull$rej[datnull$specfam==13], lty=1, type="b",   col=col.ar1)

      legend(75, 0.4, c("AR=0, (Lev = 0.05)", "AR=0.5 (Lev = 0.05)", "AR=1, (Lev = 0.05)"),  bg=NA, bty = "n", title.adj=-0.03,
             lty=c(1, 1, 1), col=c(col.ar0, col.ar05, col.ar1), lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)

      abline(h=0.05, col="gray55")
      text(x=50, y=0.057, label="0.05", col="gray55")

      dev.off()

    }



    # Alternative -------------------------------------------------------------
    # varying component: AR, dist, p_alpha, out_prop

    if(plot_specs[i,"hypothesis"]=="alternative"){

      #### against sample size (for different lambda)

      #&sum_tab$bootstrap==FALSE
      datalt <- sum_tab[sum_tab$hypothesis=="alternative"&sum_tab$dist==plot_specs[i,"dist"]&
                          sum_tab$out_prop==plot_specs[i,"out_prop"] & sum_tab$bootstrap==FALSE,]

      datalt$specfam <- 0
      datalt$specfam[datalt$test.lev==0.01 & datalt$p_alpha==plot_specs[i,"p_alpha"] & datalt$nreg==plot_specs[i,"nreg"] & datalt$ar==plot_specs[i,"ar"] & datalt$lambda==2] <- 1
      datalt$specfam[datalt$test.lev==0.01 & datalt$p_alpha==plot_specs[i,"p_alpha"] & datalt$nreg==plot_specs[i,"nreg"] & datalt$ar==plot_specs[i,"ar"] & datalt$lambda==3] <- 2
      datalt$specfam[datalt$test.lev==0.01 & datalt$p_alpha==plot_specs[i,"p_alpha"] & datalt$nreg==plot_specs[i,"nreg"] & datalt$ar==plot_specs[i,"ar"] & datalt$lambda==4] <- 3
      datalt$specfam[datalt$test.lev==0.01 & datalt$p_alpha==plot_specs[i,"p_alpha"] & datalt$nreg==plot_specs[i,"nreg"] & datalt$ar==plot_specs[i,"ar"] & datalt$lambda==6] <- 4


      col.lam1 <- "#fcbba1"
      col.lam2 <- "#fb6a4a"
      col.lam4 <- "#99000d"
      col.lam6 <- "#67000d"
      col.lam8 <- "gray25"

      #"./simulations/stored/alt_ar0.pdf"
      pdf(here(paste0("data-raw/figures/alt_ar",plot_specs[i,"ar"],"_nreg",plot_specs[i,"nreg"],"_palpha",
                      plot_specs[i,"p_alpha"],"_dist",plot_specs[i,"dist"],"_outprop",plot_specs[i,"out_prop"],".pdf")), width=pdf.width, height=pdf.height)

      par(mfrow=c(1,3))

      #datalt[datalt$specfam == 1,]

      main_title = paste0("Power (Varying Lambda, p-alpha=",plot_specs[i,"p_alpha"],", level=0.05, ar=",plot_specs[i,"ar"],
                          "\nDist=",plot_specs[i,"dist"],", Out Prop=",plot_specs[i,"out_prop"],")")

      plot(datalt$sample[datalt$specfam==1], datalt$rej[datalt$specfam==1] , lty=1, type="b", ylim=c(0, 1),
           xlim=c(50, 520), col=col.lam1, ylab="Null Rejection Frequency", xlab="Sample Size n",
           main=main_title)

      lines(datalt$sample[datalt$specfam==2], datalt$rej[datalt$specfam==2], lty=1, type="b",   col=col.lam2)
      lines(datalt$sample[datalt$specfam==3], datalt$rej[datalt$specfam==3], lty=1, type="b",   col=col.lam4)
      lines(datalt$sample[datalt$specfam==4], datalt$rej[datalt$specfam==4], lty=1, type="b",   col=col.lam6)

      abline(h=0.05, col="gray55")
      text(x=50, y=0.017, label="0.05", col="gray55")

      legend(50, 0.9, c("lambda=2", "lambda=3", "lambda=4", "lambda=6"),  bg=NA, bty = "n", title.adj=-0.03,
             lty=c(1, 1, 1), col=c(col.lam1, col.lam2, col.lam4, col.lam6), lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)


      ####fixed sample against lambda

      #datalt$specfam <- 0
      datalt$specfam[datalt$test.lev==0.01 & datalt$p_alpha==plot_specs[i,"p_alpha"] & datalt$nreg==plot_specs[i,"nreg"] & datalt$ar==plot_specs[i,"ar"] & datalt$sample==100] <- 4
      datalt$specfam[datalt$test.lev==0.01 & datalt$p_alpha==plot_specs[i,"p_alpha"] & datalt$nreg==plot_specs[i,"nreg"] & datalt$ar==plot_specs[i,"ar"] & datalt$sample==200] <- 5
      datalt$specfam[datalt$test.lev==0.01 & datalt$p_alpha==plot_specs[i,"p_alpha"] & datalt$nreg==plot_specs[i,"nreg"] & datalt$ar==plot_specs[i,"ar"] & datalt$sample==300] <- 6
      datalt$specfam[datalt$test.lev==0.01 & datalt$p_alpha==plot_specs[i,"p_alpha"] & datalt$nreg==plot_specs[i,"nreg"] & datalt$ar==plot_specs[i,"ar"] & datalt$sample==400] <- 7
      datalt$specfam[datalt$test.lev==0.01 & datalt$p_alpha==plot_specs[i,"p_alpha"] & datalt$nreg==plot_specs[i,"nreg"] & datalt$ar==plot_specs[i,"ar"] & datalt$sample==500] <- 8


      #datalt[datalt$specfam==4,]
      plot(datalt$lambda[datalt$specfam==4], datalt$rej[datalt$specfam==4] , lty=1, type="b", ylim=c(0, 1),
           xlim=c(2, 6), col=col.lam1,
           ylab="Null Rejection Frequency", xlab="Outlier Magnitude, Lambda",
           main=main_title)
      lines(datalt$lambda[datalt$specfam==5], datalt$rej[datalt$specfam==5], lty=1, type="b",   col=col.lam2)
      lines(datalt$lambda[datalt$specfam==6], datalt$rej[datalt$specfam==6], lty=1, type="b",   col=col.lam4)
      lines(datalt$lambda[datalt$specfam==7], datalt$rej[datalt$specfam==7], lty=1, type="b",   col=col.lam6)
      lines(datalt$lambda[datalt$specfam==8], datalt$rej[datalt$specfam==8], lty=1, type="b",   col=col.lam8)


      legend(2, 0.9, c("n=100", "n=200", "n=300", "n=400", "n=500"),  bg=NA, bty = "n", title.adj=-0.03,
             lty=c(1, 1, 1, 1, 1), col=c(col.lam1, col.lam2, col.lam4, col.lam6, col.lam8),
             lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)

      abline(h=0.05, col="gray55")
      text(x=50, y=0.017, label="0.05", col="gray55")
      #### against euclidian distance of coefficients
      datalt$is.euclid.sc <- datalt$is.euclid/max(datalt$is.euclid)

      plot(datalt$is.euclid.sc[datalt$specfam==4], datalt$rej[datalt$specfam==4] , lty=1, type="b", ylim=c(0, 1),
           xlim=c(0,1), col=col.lam1, ylab="Null Rejection Frequency",
           xlab="Scaled Euclidian Distance of Coefficients: d/max(d)",
           main=main_title)

      lines(datalt$is.euclid.sc[datalt$specfam==5], datalt$rej[datalt$specfam==5], lty=1, type="b",   col=col.lam2)
      lines(datalt$is.euclid.sc[datalt$specfam==6], datalt$rej[datalt$specfam==6], lty=1, type="b",   col=col.lam4)
      lines(datalt$is.euclid.sc[datalt$specfam==7], datalt$rej[datalt$specfam==7], lty=1, type="b",   col=col.lam6)
      lines(datalt$is.euclid.sc[datalt$specfam==8], datalt$rej[datalt$specfam==8], lty=1, type="b",   col=col.lam8)


      legend(0.0, 0.9, c("n=100", "n=200", "n=300", "n=400", "n=500"),  bg=NA, bty = "n", title.adj=-0.03,
             lty=c(1, 1, 1, 1, 1), col=c(col.lam1, col.lam2, col.lam4, col.lam6, col.lam8), lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)

      abline(h=0.05, col="gray55")
      text(x=50, y=0.017, label="0.05", col="gray55")
      #datalt[datalt$specfam==4,]

      dev.off()

    }


  }



  if(plot_specs[i,"bootstrap"]==TRUE){


    # Bootstrap Null -----------------------------------------------------------

    if(plot_specs[i,"hypothesis"]=="null"){

      datnull <- sum_tab[sum_tab$bootstrap==TRUE&sum_tab$hypothesis=="null",]

      datnull <- datnull[datnull$test.lev==0.05,]

      pdf.width <- 13
      pdf.height <- 5.5


      pdf(here("data-raw/figures/null_1_bootstr_1000reps_p05.pdf"), width=pdf.width, height=pdf.height)
      par(mfrow=c(1,4))

      #### Plot 1: Size against number of observations for different levels
      col.asym <- "gray55"
      col.bootfull <- "#ff7f00"
      col.bootclean <- "#1f78b4"
      col.bootclean.scale <- "#33a02c"


      datnull$specfam <- 0
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0 & datnull$dist == "norm" & datnull$clean.sample == FALSE] <- 1
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0 & datnull$dist == "norm" & datnull$clean.sample == TRUE & datnull$boot.pval.scale == 1] <- 2
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0 & datnull$dist == "norm" & datnull$clean.sample == TRUE & datnull$boot.pval.scale == 5] <- 3

      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0 & datnull$dist == "t3" & datnull$clean.sample == FALSE] <- 4
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0 & datnull$dist == "t3" & datnull$clean.sample == TRUE & datnull$boot.pval.scale == 1] <- 5
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0 & datnull$dist == "t3" & datnull$clean.sample == TRUE & datnull$boot.pval.scale == 5] <- 6


      dat.sub <- datnull[datnull$specfam %in% c(1, 2,3, 4, 5, 6),]

      #
      # datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.01 & datnull$nreg==5 & datnull$ar==0] <- 3
      # datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0] <- 4


      plot(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej[dat.sub$specfam==2 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 220), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L2: Correct Reference Distribution (Normal)")
      lines(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej.L2.boot[dat.sub$specfam==1 ], lty=1, type="b",   col=col.bootfull, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej.L2.boot[dat.sub$specfam==2 ], lty=2, type="b",   col=col.bootclean, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==3], dat.sub$rej.L2.boot[dat.sub$specfam==3 ], lty=2, type="b",   col=col.bootclean.scale, lwd=2)

      legend(50, 0.8, c("Asym", "Full Data", "Clean Data", "Clean Data Scaled"),  bg=NA, bty = "n", title.adj=-0.03,
             lty=c(1, 1, 1, 1), col=c(col.asym, col.bootfull, col.bootclean, col.bootclean.scale), lwd=2,  cex=0.9, seg.len=0.5, pt.cex=0.1,  x.intersp=0.2,  y.intersp=1)

      abline(h=0.05, col="gray12")
      text(x=55, y=0.07, label="0.05", col="gray12")

      plot(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej[dat.sub$specfam==5 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 220), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L2: Incorrect Reference Distribution (t3)")
      lines(dat.sub$sample[dat.sub$specfam==4], dat.sub$rej.L2.boot[dat.sub$specfam==4 ], lty=1, type="b",   col=col.bootfull, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej.L2.boot[dat.sub$specfam==5 ], lty=2, type="b",   col=col.bootclean, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==6], dat.sub$rej.L2.boot[dat.sub$specfam==6 ], lty=2, type="b",   col=col.bootclean.scale, lwd=2)

      abline(h=0.05, col="gray12")
      text(x=55, y=0.07, label="0.05", col="gray12")

      plot(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej[dat.sub$specfam==2 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 220), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L1: Correct Reference Distribution (Normal)")
      lines(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej.L1.boot[dat.sub$specfam==1 ], lty=1, type="b",   col=col.bootfull, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej.L1.boot[dat.sub$specfam==2 ], lty=2, type="b",   col=col.bootclean, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==3], dat.sub$rej.L1.boot[dat.sub$specfam==3 ], lty=2, type="b",   col=col.bootclean.scale, lwd=2)

      abline(h=0.05, col="gray12")
      text(x=55, y=0.07, label="0.05", col="gray12")

      plot(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej[dat.sub$specfam==5 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 220), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L1: Incorrect Reference Distribution (t3)")
      lines(dat.sub$sample[dat.sub$specfam==4], dat.sub$rej.L1.boot[dat.sub$specfam==4 ], lty=1, type="b",   col=col.bootfull, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej.L1.boot[dat.sub$specfam==5 ], lty=2, type="b",   col=col.bootclean, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==6], dat.sub$rej.L1.boot[dat.sub$specfam==6 ], lty=2, type="b",   col=col.bootclean.scale, lwd=2)

      abline(h=0.05, col="gray12")
      text(x=55, y=0.07, label="0.05", col="gray12")

      dev.off()




    }



    # Bootstrap Alt -----------------------------------------------------------



    if(plot_specs[i,"hypothesis"]=="alternative"){

      #### against sample size (for different lambda)

      datalt.boot <- sum_tab[sum_tab$bootstrap==TRUE&sum_tab$hypothesis=="alternative",]

      #datalt.boot <- read.csv("./simulations/stored/500 reps 500 boots/alt_v1_boot.csv")

      datalt.boot$specfam <- 0
      datalt.boot$specfam[datalt.boot$test.lev==0.05 &
                            datalt.boot$p_alpha==0.05 &
                            datalt.boot$nreg==5 &
                            datalt.boot$ar==0 &
                            datalt.boot$lambda==plot_specs[i,"lambda"] &
                            datalt.boot$dist=="norm" &
                            datalt.boot$boot.pval.scale == 1 &
                            datalt.boot$clean.sample == FALSE] <- 1

      datalt.boot$specfam[datalt.boot$test.lev==0.05 &
                            datalt.boot$p_alpha==0.05 &
                            datalt.boot$nreg==5 &
                            datalt.boot$ar==0 &
                            datalt.boot$lambda==plot_specs[i,"lambda"] &
                            datalt.boot$dist=="norm" &
                            datalt.boot$boot.pval.scale == 1 &
                            datalt.boot$clean.sample == TRUE] <- 2

      datalt.boot$specfam[datalt.boot$test.lev==0.05 &
                            datalt.boot$p_alpha==0.05 &
                            datalt.boot$nreg==5 &
                            datalt.boot$ar==0 &
                            datalt.boot$lambda==plot_specs[i,"lambda"] &
                            datalt.boot$dist=="norm" &
                            datalt.boot$boot.pval.scale == 5 &
                            datalt.boot$clean.sample == TRUE] <- 3


      # datalt.boot$specfam[datalt.boot$test.lev==0.05 & datalt.boot$p_alpha==0.05 & datalt.boot$nreg==5 & datalt.boot$ar==0 & datalt.boot$lambda==3] <- 2
      # datalt.boot$specfam[datalt.boot$test.lev==0.05 & datalt.boot$p_alpha==0.05 & datalt.boot$nreg==5 & datalt.boot$ar==0 & datalt.boot$lambda==4] <- 3

      col.lam1 <- "#fcbba1"
      col.lam2 <- "#fb6a4a"
      col.lam4 <- "#99000d"
      col.lam6 <- "#67000d"
      col.lam8 <- "gray25"


      pdf(here(paste0("data-raw/figures/alt_ar0_boot_lambda",plot_specs[i,"lambda"],".pdf")), width=12, height=7)

      par(mfrow=c(1,3))

      #datalt.boot[datalt.boot$specfam == 1,]

      plot(datalt.boot$sample[datalt.boot$specfam==1], datalt.boot$rej.L2.boot[datalt.boot$specfam==1] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 220), col=col.lam1, ylab="Null Rejection Frequency", xlab="Sample Size n", main="Power (Norm, Lambda=4, L2)")
      lines(datalt.boot$sample[datalt.boot$specfam==2], datalt.boot$rej.L2.boot[datalt.boot$specfam==2], lty=1, type="b",   col=col.lam2)
      lines(datalt.boot$sample[datalt.boot$specfam==3], datalt.boot$rej.L2.boot[datalt.boot$specfam==3], lty=1, type="b",   col=col.lam4)
      lines(datalt.boot$sample[datalt.boot$specfam==1], datalt.boot$rej[datalt.boot$specfam==1], lty=1, type="b",   col="gray55")



      abline(h=0.05, col="gray55")
      text(x=50, y=0.017, label="0.05", col="gray55")

      legend(50, 0.7, c("Asympt", "Raw Data", "Clean Data", "Clean Data Scale"),  bg=NA, bty = "n", title.adj=-0.03,
             lty=c(1, 1, 1), col=c("gray55", col.lam1, col.lam2, col.lam4), lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)


      ##### t3 distribution

      #datalt.boot$specfam <- 0
      datalt.boot$specfam[datalt.boot$test.lev==0.05 &
                            datalt.boot$p_alpha==0.05 &
                            datalt.boot$nreg==5 &
                            datalt.boot$ar==0 &
                            datalt.boot$lambda==plot_specs[i,"lambda"] &
                            datalt.boot$dist=="t3" &
                            datalt.boot$boot.pval.scale == 1 &
                            datalt.boot$clean.sample == FALSE] <- 4

      datalt.boot$specfam[datalt.boot$test.lev==0.05 &
                            datalt.boot$p_alpha==0.05 &
                            datalt.boot$nreg==5 &
                            datalt.boot$ar==0 &
                            datalt.boot$lambda==plot_specs[i,"lambda"] &
                            datalt.boot$dist=="t3" &
                            datalt.boot$boot.pval.scale == 1 &
                            datalt.boot$clean.sample == TRUE] <- 5

      datalt.boot$specfam[datalt.boot$test.lev==0.05 &
                            datalt.boot$p_alpha==0.05 &
                            datalt.boot$nreg==5 &
                            datalt.boot$ar==0 &
                            datalt.boot$lambda==plot_specs[i,"lambda"] &
                            datalt.boot$dist=="t3" &
                            datalt.boot$boot.pval.scale == 5 &
                            datalt.boot$clean.sample == TRUE] <- 6

      #datalt.boot$is.euclid.sc <- datalt.boot$is.euclid/max(datalt.boot$is.euclid)


      plot(datalt.boot$sample[datalt.boot$specfam==4], datalt.boot$rej.L2.boot[datalt.boot$specfam==4] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 220), col=col.lam1, ylab="Null Rejection Frequency", xlab="Sample Size n", main="Power (t3, Lambda=4, L2)")
      lines(datalt.boot$sample[datalt.boot$specfam==5], datalt.boot$rej.L2.boot[datalt.boot$specfam==5], lty=1, type="b",   col=col.lam2)
      lines(datalt.boot$sample[datalt.boot$specfam==6], datalt.boot$rej.L2.boot[datalt.boot$specfam==6], lty=1, type="b",   col=col.lam4)
      lines(datalt.boot$sample[datalt.boot$specfam==4], datalt.boot$rej[datalt.boot$specfam==4], lty=1, type="b",   col="gray55")



      abline(h=0.05, col="gray55")
      text(x=50, y=0.017, label="0.05", col="gray55")

      legend(50, 0.7, c("Asympt", "Raw Data", "Clean Data", "Clean Data Scale"),  bg=NA, bty = "n", title.adj=-0.03,
             lty=c(1, 1, 1), col=c("gray55", col.lam1, col.lam2, col.lam4), lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)


      # L1 vs L2 vs Stat ###


      plot(datalt.boot$sample[datalt.boot$specfam==3], datalt.boot$rej.L2.boot[datalt.boot$specfam==3] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 220), col=col.lam4, ylab="Null Rejection Frequency", xlab="Sample Size n", main="Power (Norm, Lambda=4, Clean Data Scale)")
      lines(datalt.boot$sample[datalt.boot$specfam==3], datalt.boot$rej.L1.boot[datalt.boot$specfam==3], lty=2, type="b",   col=col.lam4)
      lines(datalt.boot$sample[datalt.boot$specfam==3], datalt.boot$rej.dist.boot[datalt.boot$specfam==3], lty=3, type="b",   col=col.lam4)
      lines(datalt.boot$sample[datalt.boot$specfam==1], datalt.boot$rej[datalt.boot$specfam==1], lty=1, type="b",   col="gray55")



      abline(h=0.05, col="gray55")
      text(x=50, y=0.017, label="0.05", col="gray55")

      legend(50, 0.7, c("Asympt", "L2", "L1", "Test Stat."),  bg=NA, bty = "n", title.adj=-0.03,
             lty=c(1, 1, 2, 3), col=c("gray55", col.lam4, col.lam4, col.lam4), lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)

      dev.off()


    }



  }





}



