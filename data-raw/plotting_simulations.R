library(here)


# Start -------------------------------------------------------------------



########### Analyse Simulation: compare p-values to 0.01 and 0.05 cut-offs

sum_list <- list()
sum_rej_05_list <- list()
sum_rej_01_list <- list()

load(here("data-raw","simulations/rr2203","spec_list.RData"))
spec_n <- NROW(specs)

missing <- 0
failed <- vector()

files <- list.files(here("data-raw/simulations/rr2203"), pattern = "[0-9]+.RData", full.names = TRUE, recursive = FALSE)

for (l in files){

  # res <- list.res[[l]]
  #if(file.exists(here("data-raw","simulations/rr2203",paste0(paste0(specs[l,],collapse = "_"),".RData")))){
  load(l)
  #} else{
  # print(paste0("File Specification ",l," skipped - not found."))
  #  missing <- missing + 1
  # next
  #}
  # <- 1
  #load(list.files(here("data-raw/simulations"), pattern = "602.RData", full.names = TRUE))
  #load(here("data-raw/simulations/","100_null_TRUE_100_TRUE_99_TRUE_TRUE_norm_NA_NA_5_0.05_1_0.5_601_601.RData"))
  #load(here("data-raw/simulations/rr2203/1_t3_30_0.1_50_5_0.05_TRUE_3_20_1_TRUE_TRUE_alternative_4_1_0.5_TRUE_TRUE_1772_1.RData"))
  if(class(res) == "list"){
    failed <- c(failed,res$id)
    next
  }



  #load(list.files(here("data-raw/simulations"), pattern = "6[0-9][0-9].RData", full.names = TRUE))
  res.rej05 <- res[,2:NCOL(res)]
  res.rej05$rej <- NA
  res.rej05$rej.L2.boot <- NA
  res.rej05$rej.L1.boot <- NA
  res.rej05$rej.dist.boot <- NA

  res.rej05$rej.prop.test <- NA
  res.rej05$rej.prop.test.boot <- NA
  res.rej05$rej.prop.boot <- NA

  res.rej05$rej.var.boot <- NA


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

  res.rej05$rej.var.boot[res.rej05$is.dist1.boot.var.p > 0.05] <- 999
  res.rej05$rej.var.boot[res.rej05$is.dist1.boot.var.p < 0.05] <- 1
  res.rej05$rej.var.boot[res.rej05$rej.var.boot > 1] <- 0


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

  res.rej01$rej.var.boot <- NA

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

  res.rej01$rej.var.boot[res.rej01$is.dist1.boot.var.p > 0.01] <- 999
  res.rej01$rej.var.boot[res.rej01$is.dist1.boot.var.p < 0.01] <- 1
  res.rej01$rej.var.boot[res.rej01$rej.var.boot > 1] <- 0

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

print(paste0("Number of files still missing: ",missing))


sum.05 <- dplyr::bind_rows(sum_rej_05_list)
#names(sum.05) <- names(sum_rej_05_list[[1]])
sum.05$test.lev <- 0.05

sum.01 <- dplyr::bind_rows(sum_rej_01_list)
#names(sum.01) <- names(sum_rej_01_list[[1]])
sum.01$test.lev <- 0.01

###combine
sum.01.m <- merge(sum.01, specs, by="id")
sum.05.m <- merge(sum.05, specs, by="id")

sum_tab <- rbind(sum.01.m, sum.05.m)


# sum_tab -----------------------------------------------------------------

sum_tab

failed_df <- specs[specs$id %in% failed,]
failed_df$id

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

# Non-BS Alt: test.lev currently only 0.05

# Bootstrap test.lev currently only 0.05
# Same for p_alpha
# Currently only lambda 4

plot_specs_overall <- dplyr::distinct(specs,
                                      hypothesis,out_prop, bootstrap, dist, ar,
                                      p_alpha, nreg, lambda, parametric, bad_leverage, timeseries, clean.sample)


#for(boot in c(FALSE,TRUE)){
for(boot in c(TRUE)){
  for(hypo in c("null","alternative")){

    print(paste0("Boot: ",boot," Hypothesis: ",hypo))


    # Asympototic Plotting ----------------------------------------------------

    ## Asym - Null ----
    if(!boot & hypo == "null"){

      plot_specs <- plot_specs_overall[plot_specs_overall$bootstrap==FALSE&plot_specs_overall$hypothesis=="null",]
      plot_specs <- dplyr::distinct(plot_specs,dist)

      for(i in 1:nrow(plot_specs)){

        datnull <- sum_tab[sum_tab$hypothesis=="null"&sum_tab$dist==plot_specs[i,"dist"] & sum_tab$bootstrap==FALSE,]

        pdf.width <- 11
        pdf.height <- 5.5


        pdf(here(paste0("data-raw/figures/rr2203/null_dist",plot_specs[i,"dist"],".pdf")), width=pdf.width, height=pdf.height)
        par(mfrow=c(1,3))

        #### Plot 1: Size against number of observations for different levels
        col.01 <- "red"
        col.05 <- "blue"

        datnull$specfam <- 0
        datnull$specfam[datnull$test.lev==0.01 & datnull$p_alpha==0.01 & datnull$nreg==5 & datnull$ar==0] <- 1
        datnull$specfam[datnull$test.lev==0.01 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0] <- 2
        datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.01 & datnull$nreg==5 & datnull$ar==0] <- 3
        datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0] <- 4


        plot(datnull$sample[datnull$specfam==1], datnull$rej[datnull$specfam==1 ] , lty=1, type="b", ylim=c(0, 1),
             xlim=c(50, 520), col=col.01, ylab="Null Rejection Frequency", xlab="Sample Size n",
             main=paste0("Size (Varying Level, P-Alpha, & Sample, Nreg=5)\nDist = ",plot_specs[i,"dist"]))
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

        plot(datnull$sample[datnull$specfam==6], datnull$rej[datnull$specfam==6], lty=1, type="b", ylim=c(0, 1),
             xlim=c(50, 520),  col=col.p1, ylab="Null Rejection Frequency", xlab="Sample Size n",
             main=paste0("Size (Varying Nreg, Level, & Sample, P-alpha=0.05)\nDist = ",plot_specs[i,"dist"]))
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


        plot(datnull$sample[datnull$specfam==11], datnull$rej[datnull$specfam==11], lty=1, type="b", ylim=c(0, 1),
             xlim=c(50, 520), col=col.ar0, ylab="Null Rejection Frequency", xlab="Sample Size n",
             main=paste0("Size (Varying AR, & Sample, P-alpha=0.05, NReg=5, Level=0.05)\nDist = ",plot_specs[i,"dist"]))
        lines(datnull$sample[datnull$specfam==12], datnull$rej[datnull$specfam==12], lty=1, type="b",   col=col.ar05)
        lines(datnull$sample[datnull$specfam==13], datnull$rej[datnull$specfam==13], lty=1, type="b",   col=col.ar1)

        legend(75, 0.4, c("AR=0, (Lev = 0.05)", "AR=0.5 (Lev = 0.05)", "AR=1, (Lev = 0.05)"),  bg=NA, bty = "n", title.adj=-0.03,
               lty=c(1, 1, 1), col=c(col.ar0, col.ar05, col.ar1), lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)

        abline(h=0.05, col="gray55")
        text(x=50, y=0.057, label="0.05", col="gray55")

        dev.off()

      }

    }


    ## Asym - Alternative  --------------------------------------------------------------


    if(!boot & hypo == "alternative"){
      plot_specs <- plot_specs_overall[plot_specs_overall$bootstrap==FALSE&plot_specs_overall$hypothesis=="alternative",]
      plot_specs <- dplyr::distinct(plot_specs,dist,out_prop, p_alpha, ar,nreg)
      for(i in 1:nrow(plot_specs)){

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
        pdf(here(paste0("data-raw/figures/rr2203/alt_ar",plot_specs[i,"ar"],"_nreg",plot_specs[i,"nreg"],"_palpha",
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


    # Bootstraps --------------------------------------------------------------



    ## Boot - Null ---------------------------------------------------------------
    if(boot & hypo == "null"){

      datnull.boot <- sum_tab[sum_tab$bootstrap==TRUE&sum_tab$hypothesis=="null",]

      # exclude lognorm
      datnull.boot <- datnull.boot[datnull.boot$dist != "lognorm",]

      ### Non Parametric, non TS -----------------------------------------------------------------
      for(param in c(TRUE, FALSE)){
        for(TS in c(0,0.5)){
          datnull <- datnull.boot

          datnull <- datnull[datnull$test.lev==0.05,] # choose test level
          datnull <- datnull[datnull$parametric==param,] # choose non-parametric

          pdf.width <- 13
          pdf.height <- 5.5

          pdf(here(paste0("data-raw/figures/rr2203/boot_null_",ifelse(param,"non",""),"param_",ifelse(TS == 0,"non",""),"TS",".pdf")), width=pdf.width, height=pdf.height)
          par(mfrow=c(1,4))

          #### Plot 1: Size against number of observations for different levels
          col.asym <- "gray55"
          col.bootfull <- "#ff7f00"
          col.bootclean <- "#1f78b4"

          datnull$specfam <- 0
          datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==TS & datnull$dist == "norm" & datnull$clean.sample == FALSE] <- 1
          datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==TS & datnull$dist == "norm" & datnull$clean.sample == TRUE & datnull$boot.pval.scale == 1] <- 2
          #datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0 & datnull$dist == "norm" & datnull$clean.sample == TRUE & datnull$boot.pval.scale == 5] <- 3

          datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==TS & datnull$dist == "t3" & datnull$clean.sample == FALSE] <- 4
          datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==TS & datnull$dist == "t3" & datnull$clean.sample == TRUE & datnull$boot.pval.scale == 1] <- 5
          #datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0 & datnull$dist == "t3" & datnull$clean.sample == TRUE & datnull$boot.pval.scale == 5] <- 6


          dat.sub <- datnull[datnull$specfam %in% c(1, 2,3, 4, 5, 6),]

          # datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.01 & datnull$nreg==5 & datnull$ar==0] <- 3
          # datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0] <- 4


          plot(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej[dat.sub$specfam==2 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L2: Correct Reference Distribution (Normal)")
          lines(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej.L2.boot[dat.sub$specfam==1 ], lty=1, type="b",   col=col.bootfull, lwd=2)
          lines(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej.L2.boot[dat.sub$specfam==2 ], lty=1, type="b",   col=col.bootclean, lwd=2)
          # variance
          lines(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej.var.boot[dat.sub$specfam==1 ], lty=3, type="b",   col=col.bootfull, lwd=2)
          lines(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej.var.boot[dat.sub$specfam==2 ], lty=3, type="b",   col=col.bootclean, lwd=2)
          # now defunct: clean scaled
          # lines(dat.sub$sample[dat.sub$specfam==3], dat.sub$rej.L2.boot[dat.sub$specfam==3 ], lty=2, type="b",   col=col.bootvar, lwd=2)


          legend(50, 0.8, c("Asym", "Full Data", "Clean Data"),  bg=NA, bty = "n", title.adj=-0.03,
                 lty=c(1, 1, 1, 1), col=c(col.asym, col.bootfull, col.bootclean), lwd=2,  cex=0.9, seg.len=0.5, pt.cex=0.1,  x.intersp=0.2,  y.intersp=1)

          abline(h=0.05, col="gray12")
          text(x=55, y=0.07, label="0.05", col="gray12")

          plot(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej[dat.sub$specfam==5 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L2: Incorrect Reference Distribution (t3)")
          lines(dat.sub$sample[dat.sub$specfam==4], dat.sub$rej.L2.boot[dat.sub$specfam==4 ], lty=1, type="b",   col=col.bootfull, lwd=2)
          lines(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej.L2.boot[dat.sub$specfam==5 ], lty=1, type="b",   col=col.bootclean, lwd=2)
          # variance
          lines(dat.sub$sample[dat.sub$specfam==4], dat.sub$rej.var.boot[dat.sub$specfam==4 ], lty=3, type="b",   col=col.bootfull, lwd=2)
          lines(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej.var.boot[dat.sub$specfam==5 ], lty=3, type="b",   col=col.bootclean, lwd=2)
          # now defunct: clean scaled
          # lines(dat.sub$sample[dat.sub$specfam==6], dat.sub$rej.L2.boot[dat.sub$specfam==6 ], lty=2, type="b",   col=col.bootvar, lwd=2)

          abline(h=0.05, col="gray12")
          text(x=55, y=0.07, label="0.05", col="gray12")

          plot(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej[dat.sub$specfam==2 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L1: Correct Reference Distribution (Normal)")
          lines(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej.L1.boot[dat.sub$specfam==1 ], lty=1, type="b",   col=col.bootfull, lwd=2)
          lines(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej.L1.boot[dat.sub$specfam==2 ], lty=1, type="b",   col=col.bootclean, lwd=2)
          # variance
          lines(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej.var.boot[dat.sub$specfam==1 ], lty=2, type="b",   col=col.bootfull, lwd=2)
          lines(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej.var.boot[dat.sub$specfam==2 ], lty=2, type="b",   col=col.bootclean, lwd=2)
          # now defunct: clean scaled
          # lines(dat.sub$sample[dat.sub$specfam==3], dat.sub$rej.L1.boot[dat.sub$specfam==3 ], lty=2, type="b",   col=col.bootvar, lwd=2)

          abline(h=0.05, col="gray12")
          text(x=55, y=0.07, label="0.05", col="gray12")

          plot(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej[dat.sub$specfam==5 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L1: Incorrect Reference Distribution (t3)")
          lines(dat.sub$sample[dat.sub$specfam==4], dat.sub$rej.L1.boot[dat.sub$specfam==4 ], lty=1, type="b",   col=col.bootfull, lwd=2)
          lines(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej.L1.boot[dat.sub$specfam==5 ], lty=1, type="b",   col=col.bootclean, lwd=2)
          # variance
          lines(dat.sub$sample[dat.sub$specfam==4], dat.sub$rej.var.boot[dat.sub$specfam==4 ], lty=2, type="b",   col=col.bootfull, lwd=2)
          lines(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej.var.boot[dat.sub$specfam==5 ], lty=2, type="b",   col=col.bootclean, lwd=2)
          # now defunct: clean scaled
          lines(dat.sub$sample[dat.sub$specfam==6], dat.sub$rej.L1.boot[dat.sub$specfam==6 ], lty=2, type="b",   col=col.bootvar, lwd=2)

          abline(h=0.05, col="gray12")
          text(x=55, y=0.07, label="0.05", col="gray12")

          dev.off()
        }
      }


      ## Non Parametric, TS -----------------------------------------------------------------
      datnull <- datnull.boot

      datnull <- datnull[datnull$test.lev==0.05,] # choose test level
      datnull <- datnull[datnull$parametric==FALSE,] # choose non-parametric

      pdf.width <- 13
      pdf.height <- 5.5

      pdf(here(paste0("data-raw/figures/rr2203/boot_null_nonparam_TS",".pdf")), width=pdf.width, height=pdf.height)
      par(mfrow=c(1,4))

      #### Plot 1: Size against number of observations for different levels
      col.asym <- "gray55"
      col.bootfull <- "#ff7f00"
      col.bootclean <- "#1f78b4"

      datnull$specfam <- 0
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0.5 & datnull$dist == "norm" & datnull$clean.sample == FALSE] <- 1
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0.5 & datnull$dist == "norm" & datnull$clean.sample == TRUE & datnull$boot.pval.scale == 1] <- 2
      #datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0 & datnull$dist == "norm" & datnull$clean.sample == TRUE & datnull$boot.pval.scale == 5] <- 3

      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0.5 & datnull$dist == "t3" & datnull$clean.sample == FALSE] <- 4
      datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0.5 & datnull$dist == "t3" & datnull$clean.sample == TRUE & datnull$boot.pval.scale == 1] <- 5
      #datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0 & datnull$dist == "t3" & datnull$clean.sample == TRUE & datnull$boot.pval.scale == 5] <- 6


      dat.sub <- datnull[datnull$specfam %in% c(1, 2,3, 4, 5, 6),]

      # datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.01 & datnull$nreg==5 & datnull$ar==0] <- 3
      # datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0] <- 4


      plot(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej[dat.sub$specfam==2 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L2: Correct Reference Distribution (Normal)")
      lines(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej.L2.boot[dat.sub$specfam==1 ], lty=1, type="b",   col=col.bootfull, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej.L2.boot[dat.sub$specfam==2 ], lty=1, type="b",   col=col.bootclean, lwd=2)
      # variance
      lines(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej.var.boot[dat.sub$specfam==1 ], lty=3, type="b",   col=col.bootfull, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej.var.boot[dat.sub$specfam==2 ], lty=3, type="b",   col=col.bootclean, lwd=2)
      # now defunct: clean scaled
      # lines(dat.sub$sample[dat.sub$specfam==3], dat.sub$rej.L2.boot[dat.sub$specfam==3 ], lty=2, type="b",   col=col.bootvar, lwd=2)


      legend(50, 0.8, c("Asym", "Full Data", "Clean Data"),  bg=NA, bty = "n", title.adj=-0.03,
             lty=c(1, 1, 1, 1), col=c(col.asym, col.bootfull, col.bootclean), lwd=2,  cex=0.9, seg.len=0.5, pt.cex=0.1,  x.intersp=0.2,  y.intersp=1)

      abline(h=0.05, col="gray12")
      text(x=55, y=0.07, label="0.05", col="gray12")

      plot(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej[dat.sub$specfam==5 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L2: Incorrect Reference Distribution (t3)")
      lines(dat.sub$sample[dat.sub$specfam==4], dat.sub$rej.L2.boot[dat.sub$specfam==4 ], lty=1, type="b",   col=col.bootfull, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej.L2.boot[dat.sub$specfam==5 ], lty=1, type="b",   col=col.bootclean, lwd=2)
      # variance
      lines(dat.sub$sample[dat.sub$specfam==4], dat.sub$rej.var.boot[dat.sub$specfam==4 ], lty=3, type="b",   col=col.bootfull, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej.var.boot[dat.sub$specfam==5 ], lty=3, type="b",   col=col.bootclean, lwd=2)
      # now defunct: clean scaled
      # lines(dat.sub$sample[dat.sub$specfam==6], dat.sub$rej.L2.boot[dat.sub$specfam==6 ], lty=2, type="b",   col=col.bootvar, lwd=2)

      abline(h=0.05, col="gray12")
      text(x=55, y=0.07, label="0.05", col="gray12")

      plot(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej[dat.sub$specfam==2 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L1: Correct Reference Distribution (Normal)")
      lines(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej.L1.boot[dat.sub$specfam==1 ], lty=1, type="b",   col=col.bootfull, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej.L1.boot[dat.sub$specfam==2 ], lty=1, type="b",   col=col.bootclean, lwd=2)
      # variance
      lines(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej.var.boot[dat.sub$specfam==1 ], lty=2, type="b",   col=col.bootfull, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej.var.boot[dat.sub$specfam==2 ], lty=2, type="b",   col=col.bootclean, lwd=2)
      # now defunct: clean scaled
      # lines(dat.sub$sample[dat.sub$specfam==3], dat.sub$rej.L1.boot[dat.sub$specfam==3 ], lty=2, type="b",   col=col.bootvar, lwd=2)

      abline(h=0.05, col="gray12")
      text(x=55, y=0.07, label="0.05", col="gray12")

      plot(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej[dat.sub$specfam==5 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L1: Incorrect Reference Distribution (t3)")
      lines(dat.sub$sample[dat.sub$specfam==4], dat.sub$rej.L1.boot[dat.sub$specfam==4 ], lty=1, type="b",   col=col.bootfull, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej.L1.boot[dat.sub$specfam==5 ], lty=1, type="b",   col=col.bootclean, lwd=2)
      # variance
      lines(dat.sub$sample[dat.sub$specfam==4], dat.sub$rej.var.boot[dat.sub$specfam==4 ], lty=2, type="b",   col=col.bootfull, lwd=2)
      lines(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej.var.boot[dat.sub$specfam==5 ], lty=2, type="b",   col=col.bootclean, lwd=2)
      # now defunct: clean scaled
      lines(dat.sub$sample[dat.sub$specfam==6], dat.sub$rej.L1.boot[dat.sub$specfam==6 ], lty=2, type="b",   col=col.bootvar, lwd=2)

      abline(h=0.05, col="gray12")
      text(x=55, y=0.07, label="0.05", col="gray12")

      dev.off()


      for(param in c(TRUE,FALSE)){

        # plot_specs <- plot_specs_overall[plot_specs_overall$bootstrap==TRUE&plot_specs_overall$hypothesis=="null",]
        # plot_specs <- dplyr::distinct(plot_specs,dist)
        #
        # for(i in 1:nrow(plot_specs)){

        datnull <- sum_tab[sum_tab$bootstrap==TRUE&sum_tab$hypothesis=="null",]

        datnull <- datnull[datnull$test.lev==0.05,]
        datnull <- datnull[datnull$parametric==param,]

        pdf.width <- 13
        pdf.height <- 5.5

        pdf(here(paste0("data-raw/figures/rr2203/boot_null_",ifelse(param,"param","nonparam"),".pdf")), width=pdf.width, height=pdf.height)
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


        plot(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej[dat.sub$specfam==2 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L2: Correct Reference Distribution (Normal)")
        lines(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej.L2.boot[dat.sub$specfam==1 ], lty=1, type="b",   col=col.bootfull, lwd=2)
        lines(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej.L2.boot[dat.sub$specfam==2 ], lty=2, type="b",   col=col.bootclean, lwd=2)
        lines(dat.sub$sample[dat.sub$specfam==3], dat.sub$rej.L2.boot[dat.sub$specfam==3 ], lty=2, type="b",   col=col.bootclean.scale, lwd=2)

        legend(50, 0.8, c("Asym", "Full Data", "Clean Data", "Variance"),  bg=NA, bty = "n", title.adj=-0.03,
               lty=c(1, 1, 1, 1), col=c(col.asym, col.bootfull, col.bootclean, col.bootclean.scale), lwd=2,  cex=0.9, seg.len=0.5, pt.cex=0.1,  x.intersp=0.2,  y.intersp=1)

        abline(h=0.05, col="gray12")
        text(x=55, y=0.07, label="0.05", col="gray12")

        plot(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej[dat.sub$specfam==5 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L2: Incorrect Reference Distribution (t3)")
        lines(dat.sub$sample[dat.sub$specfam==4], dat.sub$rej.L2.boot[dat.sub$specfam==4 ], lty=1, type="b",   col=col.bootfull, lwd=2)
        lines(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej.L2.boot[dat.sub$specfam==5 ], lty=2, type="b",   col=col.bootclean, lwd=2)
        lines(dat.sub$sample[dat.sub$specfam==6], dat.sub$rej.L2.boot[dat.sub$specfam==6 ], lty=2, type="b",   col=col.bootclean.scale, lwd=2)

        abline(h=0.05, col="gray12")
        text(x=55, y=0.07, label="0.05", col="gray12")

        plot(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej[dat.sub$specfam==2 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L1: Correct Reference Distribution (Normal)")
        lines(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej.L1.boot[dat.sub$specfam==1 ], lty=1, type="b",   col=col.bootfull, lwd=2)
        lines(dat.sub$sample[dat.sub$specfam==2], dat.sub$rej.L1.boot[dat.sub$specfam==2 ], lty=2, type="b",   col=col.bootclean, lwd=2)
        lines(dat.sub$sample[dat.sub$specfam==3], dat.sub$rej.L1.boot[dat.sub$specfam==3 ], lty=2, type="b",   col=col.bootclean.scale, lwd=2)

        abline(h=0.05, col="gray12")
        text(x=55, y=0.07, label="0.05", col="gray12")

        plot(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej[dat.sub$specfam==5 ] , lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L1: Incorrect Reference Distribution (t3)")
        lines(dat.sub$sample[dat.sub$specfam==4], dat.sub$rej.L1.boot[dat.sub$specfam==4 ], lty=1, type="b",   col=col.bootfull, lwd=2)
        lines(dat.sub$sample[dat.sub$specfam==5], dat.sub$rej.L1.boot[dat.sub$specfam==5 ], lty=2, type="b",   col=col.bootclean, lwd=2)
        lines(dat.sub$sample[dat.sub$specfam==6], dat.sub$rej.L1.boot[dat.sub$specfam==6 ], lty=2, type="b",   col=col.bootclean.scale, lwd=2)

        abline(h=0.05, col="gray12")
        text(x=55, y=0.07, label="0.05", col="gray12")

        dev.off()

      }

      # library(tidyverse)
      # library(MetBrewer)
      # library(extrafont)
      # for(param in c(TRUE, FALSE)){
      #   for(TS in c(TRUE, FALSE)){
      #
      #     datnull.boot %>%
      #       filter(timeseries == TS, parametric == param) -> intermed
      #
      #     if(nrow(intermed)==0){next}
      #
      #     intermed %>%
      #       filter(test.lev == 0.05, sample > 50) %>%
      #
      #       as_tibble() %>% select(-hypothesis) %>%
      #       pivot_longer(-c(id, dist, sample, parametric, clean.sample, timeseries, bad_leverage)) %>%
      #       # pivot_longer(-c(id, where(is.character)))
      #
      #       #remove clean == TRUE for rej (they should be the same)
      #       filter(!(name == "rej" & clean.sample)) %>%
      #
      #
      #       filter(name %in% c("rej","rej.L2.boot","rej.var.boot","rej.L1.boot" ,
      #                          NULL
      #       )) %>%
      #
      #
      #       ggplot(aes(x = sample, y = value, linetype = clean.sample, color = name)) +
      #       geom_hline(aes(yintercept = 0.05), size = 0.5, colour = "grey")+
      #       geom_line(size = .75) +
      #       geom_point(size = 1) +
      #
      #       facet_grid(~dist) +
      #       scale_colour_met_d("Java")+
      #       labs(y = "Null Rejection Frequency", x = "Sample", subtitle = paste0("Null\nTime Series = ", TS, "   ",
      #                                                                            "Parametric = ", param))+
      #       theme(panel.background = element_blank(),#,
      #             panel.border = element_rect(colour = "black", fill = NA),
      #             #panel.grid.major.y = element_line(color = "grey"),
      #             text = element_text(family = "Myriad Pro", size = 14),
      #             strip.background = element_blank()
      #       ) -> a
      #     ggsave(a, filename = here("data-raw/figures/rr2203",
      #                               paste0("Boot_Null_",
      #                                      "TS", TS, "_",
      #                                      "Param",param, ".pdf")), width = 8, height = 6)
      #   }
      # }
      #
      #
      #
      #



      ### Log Normal --------------------------------------------------------------

      datnull.boot <- sum_tab[sum_tab$dist == "lognorm" & sum_tab$test.lev == 0.05,]

      datnull <- datnull.boot

      datnull <- datnull[datnull$test.lev==0.05,] # choose test level

      pdf.width <- 5.5
      pdf.height <- 5.5

      pdf(here(paste0("data-raw/figures/rr2203/boot_null_lognorm.pdf")), width=pdf.width, height=pdf.height)
      par(mfrow=c(1,1))

      #### Plot 1: Size against number of observations for different levels
      col.asym <- "gray55"
      datnull$specfam <- 1

      dat.sub <- datnull[datnull$specfam %in% c(1),]

      # datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.01 & datnull$nreg==5 & datnull$ar==0] <- 3
      # datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0] <- 4


      plot(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej[dat.sub$specfam==1 ], lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.asym, ylab="Null Rejection Frequency", xlab="Sample Size n", main="L2: Incorrect Reference Distribution (Lognormal)")
      lines(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej.L2.boot[dat.sub$specfam==1 ], lty=1, type="b",   col=col.bootfull, lwd=2)
      # variance
      #lines(dat.sub$sample[dat.sub$specfam==1], dat.sub$rej.var.boot[dat.sub$specfam==1 ], lty=3, type="b",   col=col.bootfull, lwd=2)
      # now defunct: clean scaled
      # lines(dat.sub$sample[dat.sub$specfam==3], dat.sub$rej.L2.boot[dat.sub$specfam==3 ], lty=2, type="b",   col=col.bootvar, lwd=2)


      legend(50, 0.8, c("Asym", "Full Data"),  bg=NA, bty = "n", title.adj=-0.03,
             lty=c(1, 1, 1, 1), col=c(col.asym, col.bootfull, col.bootclean), lwd=2,  cex=0.9, seg.len=0.5, pt.cex=0.1,  x.intersp=0.2,  y.intersp=1)

      abline(h=0.05, col="gray12")
      text(x=55, y=0.07, label="0.05", col="gray12")


      dev.off()

  }


    # Boot - Alt --------------------------------------------------------------



    if(boot & hypo == "alternative"){


      plot_specs <- plot_specs_overall[plot_specs_overall$bootstrap==TRUE&plot_specs_overall$hypothesis=="alternative",]
      plot_specs <- dplyr::distinct(plot_specs,lambda, ar, parametric, bad_leverage, timeseries, clean.sample)

      datalt.boot <- sum_tab[sum_tab$bootstrap==TRUE&sum_tab$hypothesis=="alternative",]


      for(param in c(TRUE, FALSE)){
        for(BL in c(TRUE, FALSE)){
          for(TS in c(TRUE, FALSE)){

            datalt.boot %>%
              filter(bad_leverage == BL, timeseries == TS, parametric == param) -> intermed

            if(nrow(intermed)==0){next}

            intermed %>%
              filter(test.lev == 0.05, bootstrap, sample > 50) %>%
              as_tibble() %>% select(-hypothesis) %>%
              pivot_longer(-c(id, dist, sample, parametric, clean.sample, timeseries, lambda, bad_leverage)) %>%
              # pivot_longer(-c(id, where(is.character)))
              filter(name %in% c("rej","rej.L2.boot","rej.var.boot",#"rej.L1.boot" ,
                                 NULL
              )) %>%


              ggplot(aes(x = sample, y = value, linetype = clean.sample, color = name)) +
              geom_hline(aes(yintercept = 0.05), size = 0.5, colour = "grey")+
              geom_line(size = .75) +
              geom_point(size = 1) +

              facet_grid(dist~lambda) +
              scale_colour_met_d("Java")+
              labs(y = "Null Rejection Frequency", x = "Sample",
                   subtitle = paste0("Alternative\nBad Leverage = ",BL,"   ",
                                     "Time Series = ", TS, "   ",
                                     "Parametric = ", param))+
              theme(panel.background = element_blank(),#,
                    panel.border = element_rect(colour = "black", fill = NA),
                    #panel.grid.major.y = element_line(color = "grey"),
                    text = element_text(family = "Myriad Pro", size = 14),
                    strip.background = element_blank()
              ) -> a
            ggsave(a, filename = here("data-raw/figures/rr2203",
                                      paste0("Boot_Alt_BL",BL,"_",
                                             "TS", TS, "_",
                                             "Param",param, ".pdf")), width = 8, height = 6)
          }
        }
      }


      # for(i in 1:nrow(plot_specs)){

      #### against sample size (for different lambda)
      # TO DO MS:
      # Used to be: cleansample T/F and boot.pval scale
      # now not needed/appropriate anymore

      #datalt.boot <- read.csv("./simulations/stored/500 reps 500 boots/alt_v1_boot.csv")
      #
      # datalt.boot$specfam <- 0
      # datalt.boot$specfam[datalt.boot$test.lev==0.05 &
      #                       datalt.boot$p_alpha==0.05 &
      #                       datalt.boot$nreg==5 &
      #                       datalt.boot$timeseries==plot_specs[i,"timeseries"] &
      #                       datalt.boot$lambda==plot_specs[i,"lambda"] &
      #                       datalt.boot$dist=="norm" &
      #                       datalt.boot$boot.pval.scale == 1 &
      #                       datalt.boot$bad_leverage == plot_specs[i,"bad_leverage"] &
      #                       datalt.boot$parametric == plot_specs[i,"parametric"] &
      #                       datalt.boot$clean.sample == plot_specs[i,"clean.sample"]] <- 1
      #
      # datalt.boot$specfam[datalt.boot$test.lev==0.05 &
      #                       datalt.boot$p_alpha==0.05 &
      #                       datalt.boot$nreg==5 &
      #                       datalt.boot$timeseries==plot_specs[i,"timeseries"] &
      #                       datalt.boot$lambda==plot_specs[i,"lambda"] &
      #                       datalt.boot$dist=="norm" &
      #                       datalt.boot$boot.pval.scale == 1 &
      #                       datalt.boot$bad_leverage == plot_specs[i,"bad_leverage"] &
      #                       datalt.boot$parametric == plot_specs[i,"parametric"] &
      #                       datalt.boot$clean.sample == plot_specs[i,"clean.sample"]] <- 2
      #
      # datalt.boot$specfam[datalt.boot$test.lev==0.05 &
      #                       datalt.boot$p_alpha==0.05 &
      #                       datalt.boot$nreg==5 &
      #                       datalt.boot$timeseries==plot_specs[i,"timeseries"] &
      #                       datalt.boot$lambda==plot_specs[i,"lambda"] &
      #                       datalt.boot$dist=="norm" &
      #                       datalt.boot$boot.pval.scale == 5 &
      #                       datalt.boot$bad_leverage == plot_specs[i,"bad_leverage"] &
      #                       datalt.boot$parametric == plot_specs[i,"parametric"] &
      #                       datalt.boot$clean.sample == plot_specs[i,"clean.sample"]] <- 3
      #
      #
      # # datalt.boot$specfam[datalt.boot$test.lev==0.05 & datalt.boot$p_alpha==0.05 & datalt.boot$nreg==5 & datalt.boot$ar==0 & datalt.boot$lambda==3] <- 2
      # # datalt.boot$specfam[datalt.boot$test.lev==0.05 & datalt.boot$p_alpha==0.05 & datalt.boot$nreg==5 & datalt.boot$ar==0 & datalt.boot$lambda==4] <- 3
      #
      # col.lam1 <- "#fcbba1"
      # col.lam2 <- "#fb6a4a"
      # col.lam4 <- "#99000d"
      # col.lam6 <- "#67000d"
      # col.lam8 <- "gray25"
      #
      #
      # pdf(here(paste0("data-raw/figures/rr2203/boot_alt_lambda",plot_specs[i,"lambda"],"_",
      #                 ifelse(plot_specs[i,"parametric"],"param","nonparam"),
      #                 ifelse(plot_specs[i,"bad_leverage"],"_badleverage",""),
      #                 ifelse(plot_specs[i,"timeseries"],"_TS","_stationary"),".pdf")), width=12, height=7)
      #
      # par(mfrow=c(1,3))
      #
      # #datalt.boot[datalt.boot$specfam == 1,]
      #
      # plot(datalt.boot$sample[datalt.boot$specfam==1], datalt.boot$rej.L2.boot[datalt.boot$specfam==1] , lty=1, type="b", ylim=c(0, 1),
      #      xlim=c(50, 420), col=col.lam1, ylab="Null Rejection Frequency", xlab="Sample Size n",
      #      main=paste0("Power (Norm, Lambda=",plot_specs[i,"lambda"],", L2)"))
      # lines(datalt.boot$sample[datalt.boot$specfam==2], datalt.boot$rej.L2.boot[datalt.boot$specfam==2], lty=1, type="b",   col=col.lam2)
      # lines(datalt.boot$sample[datalt.boot$specfam==3], datalt.boot$rej.L2.boot[datalt.boot$specfam==3], lty=1, type="b",   col=col.lam4)
      # lines(datalt.boot$sample[datalt.boot$specfam==1], datalt.boot$rej[datalt.boot$specfam==1], lty=1, type="b",   col="gray55")
      #
      #
      #
      # abline(h=0.05, col="gray55")
      # text(x=50, y=0.017, label="0.05", col="gray55")
      #
      # legend(50, 0.7, c("Asympt", "Raw Data", "Clean Data", "Clean Data Scale"),  bg=NA, bty = "n", title.adj=-0.03,
      #        lty=c(1, 1, 1), col=c("gray55", col.lam1, col.lam2, col.lam4), lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)
      #
      #
      # ##### t3 distribution
      #
      # #datalt.boot$specfam <- 0
      # datalt.boot$specfam[datalt.boot$test.lev==0.05 &
      #                       datalt.boot$p_alpha==0.05 &
      #                       datalt.boot$nreg==5 &
      #                       datalt.boot$ar==0 &
      #                       datalt.boot$lambda==plot_specs[i,"lambda"] &
      #                       datalt.boot$dist=="t3" &
      #                       datalt.boot$boot.pval.scale == 1 &
      #                       datalt.boot$clean.sample == FALSE] <- 4
      #
      # datalt.boot$specfam[datalt.boot$test.lev==0.05 &
      #                       datalt.boot$p_alpha==0.05 &
      #                       datalt.boot$nreg==5 &
      #                       datalt.boot$ar==0 &
      #                       datalt.boot$lambda==plot_specs[i,"lambda"] &
      #                       datalt.boot$dist=="t3" &
      #                       datalt.boot$boot.pval.scale == 1 &
      #                       datalt.boot$clean.sample == TRUE] <- 5
      #
      # datalt.boot$specfam[datalt.boot$test.lev==0.05 &
      #                       datalt.boot$p_alpha==0.05 &
      #                       datalt.boot$nreg==5 &
      #                       datalt.boot$ar==0 &
      #                       datalt.boot$lambda==plot_specs[i,"lambda"] &
      #                       datalt.boot$dist=="t3" &
      #                       datalt.boot$boot.pval.scale == 5 &
      #                       datalt.boot$clean.sample == TRUE] <- 6
      #
      # #datalt.boot$is.euclid.sc <- datalt.boot$is.euclid/max(datalt.boot$is.euclid)
      #
      #
      # plot(datalt.boot$sample[datalt.boot$specfam==4], datalt.boot$rej.L2.boot[datalt.boot$specfam==4] ,
      #      lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.lam1, ylab="Null Rejection Frequency",
      #      xlab="Sample Size n", main=paste0("Power (t3, Lambda=",plot_specs[i,"lambda"],", L2)"))
      # lines(datalt.boot$sample[datalt.boot$specfam==5], datalt.boot$rej.L2.boot[datalt.boot$specfam==5], lty=1, type="b",   col=col.lam2)
      # lines(datalt.boot$sample[datalt.boot$specfam==6], datalt.boot$rej.L2.boot[datalt.boot$specfam==6], lty=1, type="b",   col=col.lam4)
      # lines(datalt.boot$sample[datalt.boot$specfam==4], datalt.boot$rej[datalt.boot$specfam==4], lty=1, type="b",   col="gray55")
      #
      #
      #
      # abline(h=0.05, col="gray55")
      # text(x=50, y=0.017, label="0.05", col="gray55")
      #
      # legend(50, 0.7, c("Asympt", "Raw Data", "Clean Data", "Clean Data Scale"),  bg=NA, bty = "n", title.adj=-0.03,
      #        lty=c(1, 1, 1), col=c("gray55", col.lam1, col.lam2, col.lam4), lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)
      #
      #
      # # L1 vs L2 vs Stat ###
      #
      #
      # plot(datalt.boot$sample[datalt.boot$specfam==3], datalt.boot$rej.L2.boot[datalt.boot$specfam==3] ,
      #      lty=1, type="b", ylim=c(0, 1), xlim=c(50, 420), col=col.lam4, ylab="Null Rejection Frequency",
      #      xlab="Sample Size n", main=paste0("Power (Norm, Lambda=",plot_specs[i,"lambda"],", Clean Data Scale)"))
      # lines(datalt.boot$sample[datalt.boot$specfam==3], datalt.boot$rej.L1.boot[datalt.boot$specfam==3], lty=2, type="b",   col=col.lam4)
      # #lines(datalt.boot$sample[datalt.boot$specfam==3], datalt.boot$rej.dist.boot[datalt.boot$specfam==3], lty=3, type="b",   col=col.lam4)
      # lines(datalt.boot$sample[datalt.boot$specfam==1], datalt.boot$rej[datalt.boot$specfam==1], lty=1, type="b",   col="gray55")
      #
      #
      #
      # abline(h=0.05, col="gray55")
      # text(x=50, y=0.017, label="0.05", col="gray55")
      #
      # legend(50, 0.7, c("Asympt", "L2", "L1"),  bg=NA, bty = "n", title.adj=-0.03,
      #        lty=c(1, 1, 2), col=c("gray55", col.lam4, col.lam4), lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)
      #
      # dev.off()
      # }
    }
}
  }

