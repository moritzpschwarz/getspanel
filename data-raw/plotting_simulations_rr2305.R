library(here)


# Start -------------------------------------------------------------------


########### Analyse Simulation: compare p-values to 0.01 and 0.05 cut-offs

sum_list <- list()
sum_rej_05_list <- list()
sum_rej_01_list <- list()

load(here("data-raw","simulations/rr2305","spec_list.RData"))
spec_n <- NROW(specs)

missing <- 0
failed <- vector()

# NULL files for RR2305
files_null <- list.files(here("data-raw/simulations/rr2305"), pattern = "[0-9]+.RData", full.names = TRUE, recursive = FALSE)
# Only subsetting the files I actually need
files_null <- files_null[grepl("NA_null_norm_", files_null)]
files_null <- files_null[grepl("10000_0.9|10000_0.75", files_null)]

# Alt files for RR2305
files_alt <- list.files(here("data-raw/simulations/rr2305"), pattern = "[0-9]+.RData", full.names = TRUE, recursive = FALSE)
# Only subsetting the files I actually need
files_alt <- files_alt[!grepl("NA_null_norm_", files_alt)]
files_alt <- files_alt[grepl("10000_0.9|10000_0.75", files_alt)]


# ADDING FILES not run in RR2305 but in the initial submission
# this should only be the Null, non-BS, p_alpha = 0.05 and 0.01, Nreg = 10
files_initsubm <- list.files(here("data-raw/simulations/"), pattern = "[0-9]+.RData", full.names = TRUE, recursive = FALSE)
files_initsubm <- files_initsubm[grepl("FALSE_null_10000_", files_initsubm)]
#files_initsubm <- files_initsubm[grepl("0.05_10_|0.01_10_", files_initsubm)]
files_initsubm1 <- files_initsubm[grepl("_norm_", files_initsubm)]

files_initsubm <- list.files(here("data-raw/simulations/"), pattern = "[0-9]+.RData", full.names = TRUE, recursive = FALSE)
files_initsubm <- files_initsubm[grepl("FALSE_alternative_10000_", files_initsubm)]
files_initsubm <- files_initsubm[grepl("_norm_", files_initsubm)]
files_initsubm2 <- files_initsubm[grepl("_0.1_NA", files_initsubm)]


specs_backup <- specs
load(here("data-raw","simulations","spec_list.RData"))
specs_initsubm <- specs[!specs$bootstrap &  specs$dist=="norm",]

# combine them again
files <- c(files_null, files_alt, files_initsubm1, files_initsubm2)
specs <- dplyr::bind_rows(specs_backup, specs_initsubm)
spec_n <- NROW(specs)

for (l in files){
  load(l)

  if(any(class(res) == "list")){
    failed <- c(failed,res$id)
    next
  }

  if("dist" %in% names(res)){
    res$dist <- NULL
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



# Plotting ----------------------------------------------------------------


# Notes:

plot_specs_overall <- dplyr::distinct(specs,
                                      hypothesis,out_prop, bootstrap, dist, ar,
                                      p_alpha, nreg, lambda, parametric, bad_leverage, timeseries, clean.sample)


#for(boot in c(FALSE,TRUE)){
for(boot in c(FALSE)){
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


        pdf(here(paste0("data-raw/figures/rr2305/null_dist",plot_specs[i,"dist"],".pdf")), width=pdf.width, height=pdf.height)
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
             main="Size: Varying Cut-off c and Level of Test")
        lines(datnull$sample[datnull$specfam==2], datnull$rej[datnull$specfam==2 ], lty=2, type="b",   col=col.01)
        abline(h=0.01, col=col.01)
        text(x=50, y=0.017, label="0.01", col=col.01)

        lines(datnull$sample[datnull$specfam==3], datnull$rej[datnull$specfam==3], lty=1, type="b",  col=col.05)
        lines(datnull$sample[datnull$specfam==4], datnull$rej[datnull$specfam==4 ], lty=2, type="b",  col=col.05)
        abline(h=0.05, col=col.05)
        text(x=50, y=0.057, label="0.05", col=col.05)

        legend(75, 0.4, c("Level: 0.01 (c=2.57)", "Level: 0.01 (c=1.96)", "Level: 0.05 (c=2.57)", "Level: 0.05 (c=1.96)"),  bg=NA, bty = "n", title.adj=-0.03,
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
             main = "Size: Varying Number of Regressors Tested")
        #lines(datnull$sample[datnull$specfam==6], datnull$rej[datnull$specfam==6], lty=3, type="b",   col=col.p1)

        #lines(datnull$sample[datnull$specfam==7], datnull$rej[datnull$specfam==7], lty=1, type="b",   col=col.p5)
        lines(datnull$sample[datnull$specfam==8], datnull$rej[datnull$specfam==8], lty=1, type="b",   col=col.p5)

        #lines(datnull$sample[datnull$specfam==9], datnull$rej[datnull$specfam==9], lty=1, type="b",   col=col.p10)
        lines(datnull$sample[datnull$specfam==10], datnull$rej[datnull$specfam==10], lty=1, type="b",   col=col.p10)

        legend(75, 0.4, c("Number of Regr. = 1", "Number of Regr. = 5", "Number of Regr. = 10"),  bg=NA, bty = "n", title.adj=-0.03,
               lty=c(1, 1, 1), col=c(col.p1, col.p5, col.p10), lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)

        # abline(h=0.01, col="gray55")
        # text(x=50, y=0.017, label="0.01", col="gray55")

        abline(h=0.05, col="gray55")
        text(x=150, y=0.057, label="Nominal Level (0.05)", col="gray55")

        #### plot 3: ar effect

        datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0] <- 11
        datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0.5] <- 12
        datnull$specfam[datnull$test.lev==0.05 & datnull$p_alpha==0.05 & datnull$nreg==5 & datnull$ar==0.9] <- 13

        col.ar0 <- "#fcbba1"
        col.ar05 <- "#fb6a4a"
        col.ar1 <- "#99000d"


        plot(datnull$sample[datnull$specfam==11], datnull$rej[datnull$specfam==11], lty=1, type="b", ylim=c(0, 1),
             xlim=c(50, 520), col=col.ar0, ylab="Null Rejection Frequency", xlab="Sample Size n",
             main="Size: Varying Degree of Persistence")
        points(datnull$sample[datnull$specfam==12], datnull$rej[datnull$specfam==12], lty=1, type="b",   col=col.ar05)
        lines(datnull$sample[datnull$specfam==12], datnull$rej[datnull$specfam==12], lty=1, type="b",   col=col.ar05)
        lines(datnull$sample[datnull$specfam==13], datnull$rej[datnull$specfam==13], lty=1, type="b",   col=col.ar1)

        legend(75, 0.4, c("Stationary (iid)", expression('Autoregressive, '*rho * {phantom() == phantom()} * 0.5), expression('Autoregressive, '*rho * {phantom() == phantom()} * 0.9)),  bg=NA, bty = "n", title.adj=-0.03,
               lty=c(1, 1, 1), col=c(col.ar0, col.ar05, col.ar1), lwd=2,  cex=1.1, pt.cex=1.1,  x.intersp=0.5,  y.intersp=1)

        abline(h=0.05, col="gray55")
        text(x=150, y=0.057, label="Nominal Level (0.05)", col="gray55")

        dev.off()

      }

    }


    ## Asym - Alternative  --------------------------------------------------------------
    if(!boot & hypo == "alternative"){
      plot_specs <- plot_specs_overall[plot_specs_overall$bootstrap==FALSE&plot_specs_overall$hypothesis=="alternative",]
      plot_specs <- dplyr::distinct(plot_specs,dist,out_prop, p_alpha, ar,nreg)
      plot_specs <- plot_specs[(plot_specs$ar == 0.9|plot_specs$ar == 0.75)&plot_specs$out_prop == 0.1,]
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

        pdf.width <- 11
        pdf.height <- 5.5

        #"./simulations/stored/alt_ar0.pdf"
        pdf(here(paste0("data-raw/figures/rr2305/alt_ar",plot_specs[i,"ar"],"_nreg",plot_specs[i,"nreg"],"_palpha",
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

    }
  }
}


