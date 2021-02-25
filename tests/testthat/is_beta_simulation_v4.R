##############################################
#### Simulation for the IS distortion tests: runs either under the alternative or under the null
################################################

###############################

#library(getspanel)
devtools::load_all()
library(gets)
library(here)

#install.packages("metap")
#install.packages("goftest")

#install.packages("multtest")
#install.packages("survcomp")

# library(metap)
# library(goftest)
# library(gets)
# library(car)
# library(purrr)
# library(KSgeneral)

#install.packages("KSgeneral")

##### now includes bootstrap option
#reps <- data.frame(bootstrap = 1000, no_bootstrap = 10000) #number of simulation replications
#alternative <- FALSE #whether we are under the null or no

#bootstrap <- TRUE #does not work yet with AR terms

#### bootstrap options
#nboot <- 1000 #number of bootstrap replications
clean.sample_c <- c(FALSE, TRUE) #bootstrap on clean sample, TRUE or FALSE
boot.pval.scale_c <- c(1, 5) #scale the p-value
boot.parallel <- TRUE #parallelize bootstrap (significant speed gains!)
cores <- parallel::detectCores()-1 #number of cores for bootstrap
#cores <- 7 #number of cores for bootstrap
parametric <- FALSE #nonparametric or parametric bootstrap
#############

max.block.size <- 2 #block size for isat, =2 is fastest.

set.seed(123)
### number of regressors
px <- c(5) #regressors
ar_c <- c(0)#ar coefficient in DGP#### ar non-zero does not yet work for bootstrap, as it's not clear what the null model should be on 'clean' observations, and bootstrap also unclear
sample_c <- c(50, 100) #sample size

p_alpha_c <- c(0.05) #significance level for isat testing

############### Specifications for alternative
lambda_c <- c(6) ### outlier magnitude:
out_prop_c <- c(0.1) ### outlier proportion:

### distribution in sample
loc_limits_c <- c(1) #outliers distributed from 0 to this value times the sample T

####distribution of error terms
dist_c <- c("norm", "t3") #can choose from: ("norm", "t3", "cauchy", "uniform", "lognorm")








####### Combining all the above options into a list of specifications to be looped over:
###############
###############
### NO BOOT ###
###############
###############
# Null
spec_list <- list(
  bootstrap = FALSE,
  hypothesis = "null",
  reps = 10000,
  p_alpha = c(0.01,0.05),
  nreg = c(1,5,10),
  sample = seq(100,500,100),
  ar = c(0,0.5,1),
  dist = dist_c
)
specs_intermed <- expand.grid(spec_list)
specs <- specs_intermed

# Alternative
spec_list <- list(
  bootstrap = FALSE,
  hypothesis = "alternative",
  reps = 10000,
  p_alpha = c(0.05),
  lambda = c(2,3,4,6),
  out_prop = c(0.1,0.15),
  nreg = c(1,5),
  sample = seq(100,500,100),
  ar = c(0,0.5,1),
  dist = dist_c
)

specs_intermed <- expand.grid(spec_list)
specs <- dplyr::bind_rows(specs, specs_intermed)

###############
###############
#### BOOT #####
###############
###############
# NULL
spec_list <- list(
  bootstrap = TRUE,
  hypothesis = "null",
  reps = 1000,
  nboot = 1000,
  p_alpha = 0.05,
  nreg = 5,
  sample = seq(50,200,50),
  ar = 0,
  dist = dist_c,
  clean.sample = clean.sample_c,
  boot.pval.scale = boot.pval.scale_c
)
specs_intermed <- expand.grid(spec_list)
specs <- dplyr::bind_rows(specs, specs_intermed)


# ALTERNATIVE
spec_list <- list(
  bootstrap = TRUE,
  hypothesis = "alternative",
  reps = 1000,
  nboot = 1000,
  p_alpha = 0.05,
  nreg = 5,
  sample = seq(50,200,50),
  ar = 0,
  lambda = c(2,3,4,6),
  out_prop = 0.1,
  dist = dist_c,
  clean.sample = clean.sample_c,
  boot.pval.scale = boot.pval.scale_c
)
specs_intermed <- expand.grid(spec_list)
specs <- dplyr::bind_rows(specs, specs_intermed)

specs$hypothesis <- as.character(specs$hypothesis)
specs$dist <- as.character(specs$dist)
specs$id <- 1:nrow(specs)





###drop a few specs that are redundant
spec.drop <- specs$id[specs$ar > 0 & specs$nreg != 5]
specs <- specs[!(specs$id %in% spec.drop),]

spec.drop <- specs$id[specs$ar > 0 & specs$p_alpha != 0.05]
specs <- specs[!(specs$id %in% spec.drop),]

spec.drop <- specs$id[specs$clean.sample == FALSE & specs$boot.pval.scale > 1]
specs <- specs[!(specs$id %in% spec.drop),]

#### FOR NOW ###



#################### All the different specifications to be looped over, each with "reps" number of replications.
spec_n <- NROW(specs)
spec_n

specs$id.seq <- seq(1:NROW(specs))
# ###### Total runs:
# spec_n*reps

######################################

list.res <- list()
list.reg <- list()

start.time <- Sys.time()

save(specs, file = here("data-raw", "simulations", "spec_list.RData"))

library(doMC)
registerDoMC(detectCores()-1)  # coefsamples if enough cores available - otherwise total-1
foreach(j = 1:spec_n, .packages = loadedNamespaces()) %dopar% {
  #for (j in 1:spec_n){
  #j <- 1
  p_alpha <-   specs$p_alpha[j]
  ar <- specs$ar[j]

  if (specs$bootstrap[j]){

    clean.sample <- specs$clean.sample[j]
    boot.pval.scale <- specs$boot.pval.scale[j]
  }

  p <- specs$nreg[j]
  dist <- specs$dist[j]



  if (specs$hypothesis[j] == "alternative"){
    out_lam <- specs$lambda[j]
    out_prop <- specs$out_prop[j]

  }


  id <- specs$id[j]
  id.seq <- specs$id.seq[j]
  Tlength <- specs$sample[j]
  loc_limit <-   floor(Tlength*1)

  if (p > 0){
    pcoef <- c(rep(0.5, p))
  } else {
    pcoef <- NULL
  }


  is.dates.list <- list()
  isalt.dates.list <- list()

  sis.dates.list <- list()

  res <- data.frame(matrix(NA, specs$reps[j], 2))
  names(res) <- c("rep", "is.dist1.p")

  res$is.dist1.boot.L2.p <- NA
  res$is.dist1.boot.L1.p <- NA
  res$is.dist1.boot.dist.p <- NA

  res$is.prop.test.p <- NA
  res$is.prop.boot.p <- NA
  res$is.prop.boot.test.p <- NA

  res$is.avg.dist.pct <- NA
  res$is.euclid <- NA



  if (specs$hypothesis[j]=="alternative"){
    print(paste0("Alt | ", "Spec: ", j,  " of ", spec_n, " |", paste(colnames(specs),specs[j,],  collapse=", ")))

  } else {
    print(paste0("Null | ", "Spec: ", j,  " of ", spec_n, " |", paste(colnames(specs),specs[j,],  collapse=", ")))

  }


  for (i in 1:specs$reps[j]){
    #i <- 1
    # if (specs$hypothesis[j]=="alternative"){
    #   print(paste("Alt | ", "Spec: ", j,  " of ", spec_n, " |", paste(colnames(specs),specs[j,],  collapse=", "), "| rep:", i, " of ", specs$reps[j], sep="") )
    #
    # } else {
    #   print(paste("Null | ", "Spec: ", j,  " of ", spec_n, " |", paste(colnames(specs),specs[j,],  collapse=", "), "| rep:", i, " of ", specs$reps[j], sep="") )
    #
    # }



    if (specs$hypothesis[j]=="alternative"){
      noutl <- floor(out_prop*Tlength)
      out_loc <- rdunif(noutl, 0, loc_limit)
    } else {
      noutl <- 0
      out_loc <- NULL
      out_lam <- 0
    }
    ########## regression tests

    out_loc_s_T1 <- as.vector(rep(0,Tlength+1))
    out_loc_s_T1[out_loc] <- 1

    out_loc_s <- out_loc_s_T1[2:(Tlength+1)]

    if (dist=="norm"){
      eps_T1 <-  rnorm((Tlength+1), 0, 1)
    } else if (dist=="t3"){
      eps_T1 <-  rt((Tlength+1), df=3)
    } else if (dist=="cauchy"){
      eps_T1 <-  rcauchy((Tlength+1))
    } else if (dist=="uniform"){
      eps_T1 <-  runif((Tlength+1))
    }else if (dist=="lognorm"){
      eps_T1 <-  rlnorm((Tlength+1), 0, 1)
    }


    eps <-  eps_T1[2:(Tlength+1)]

    y_x_T1 <- as.vector(rep(NA, Tlength+1))

    if ( p > 0){

      if (p > 1){
        mx_T1 <- matrix(rnorm((Tlength+1)*p, 0, 1), ncol=p)
        mx <- mx_T1[2:(Tlength+1),]
        colnames(mx) <- paste("x", seq(1:p), sep="")
      } else {
        mx_T1 <- matrix(rnorm((Tlength+1)*p, 0, 1), ncol=p)
        mx <- mx_T1[2:(Tlength+1),]
        #names(mx) <- paste("x", seq(1:p), sep="")
      }




      if (ar !=0){

        if (p > 1){
          y_x_T1[1] <- mx_T1[1,]%*%pcoef+ out_loc_s[1]*out_lam + eps_T1[1]
        } else {
          y_x_T1[1] <- mx_T1[1,]*pcoef+ out_loc_s[1]*out_lam + eps_T1[1]
        }


        for (t in 2:((Tlength+1))){
          if (p > 1){
            y_x_T1[t] <- mx_T1[t,]%*%pcoef + out_loc_s_T1[t]*out_lam + eps_T1[t] +  ar* y_x_T1[t-1]
          } else {
            y_x_T1[t] <- mx_T1[t,]*pcoef + out_loc_s_T1[t]*out_lam + eps_T1[t] +  ar* y_x_T1[t-1]
          }

        }

        # yx <- y_x_T1[2:Tlength]
      } else {
        if (p > 1){
          y_x <- mx%*%pcoef + out_loc_s*out_lam + eps #+  ar* y_x[t-1]
        } else {
          y_x <- mx*pcoef + out_loc_s*out_lam + eps #+  ar* y_x[t-1]
        }



      }



    } else {
      mx <- NULL

      if (ar !=0){

        if (p > 1){
          y_x_T1[1] <- mx_T1[1,]%*%pcoef+ out_loc_s[1]*out_lam + eps_T1[1]
        } else {
          y_x_T1[1] <- mx_T1[1,]*pcoef+ out_loc_s[1]*out_lam + eps_T1[1]
        }

        for (t in 2:(Tlength)){
          if (p > 1){
            y_x_T1[t] <- mx_T1[t,]%*%pcoef + out_loc_s_T1[t]*out_lam + eps_T1[t] +  ar* y_x_T1[t-1]
          } else {
            y_x_T1[t] <- mx_T1[t,]*pcoef + out_loc_s_T1[t]*out_lam + eps_T1[t] +  ar* y_x_T1[t-1]
          }

        }


      } else {
        if (alternative){
          y_x <- out_loc_s*out_lam +  eps
        } else {
          y_x <-  eps
        }


      }


    }




    if (ar != 0){
      is1 <- isat(y_x_T1, mxreg=mx_T1, ar=1, iis=TRUE, sis=FALSE, tis=FALSE, t.pval=p_alpha, plot=FALSE, print.searchinfo = FALSE, max.block.size=max.block.size)

    } else {
      is1 <- isat(y_x, mxreg=mx, ar=NULL, iis=TRUE, sis=FALSE, tis=FALSE, t.pval=p_alpha, plot=FALSE, print.searchinfo = FALSE, max.block.size=max.block.size)

    }



    dist1 <- distorttest(is1, coef="all")
    outl1 <- outliertest(is1)



    if (specs$bootstrap[j]){

      dist1.boot <-  distorttest.boot(
        x = is1,
        nboot = nboot,
        clean.sample = clean.sample,
        parametric = parametric,
        scale.t.pval = boot.pval.scale,
        parallel = FALSE,
        #parallel = boot.parallel,
        #ncore = cores,
        max.block.size = max.block.size
      )

    }


    #####record distortion

    rel.coef.name <- names(dist1$coef.diff)
    sum.ols <- summary(dist1$ols)

    coefdif.prop <- dist1$coef.diff/dist1$ols$mean.results[rel.coef.name,"coef"]
    coefdif.prop.abs.m <- mean(abs(coefdif.prop))
    coefdif.euclid <- sqrt(sum(dist1$coef.diff^2)) #euclidian distance of coefficients

    #hist(dist1.boot$coefdist.res$prop)

    #dist1.boot$coefdist.res$prop

    #(1-ecdf(dist1.boot$coefdist.res$prop)(prop.full))*2

    # xx <- rnorm(100000)
    # (1-ecdf(xx)(1.96))*2
    # #0.99
    #
    #
    # aycdf(c(0, .5, .7))
    #
    # prop.full <- outl1$proportion$estimate
    #
    # boot.p.prop <- 2*min(sum(dist1.boot$coefdist.res$prop > prop.full)/nboot,  sum(dist1.boot$coefdist.res$prop <= prop.full)/nboot) #check the p-values here.

    #######################


    res$rep[i] <- i
    res$is.dist1.p[i] <- dist1$p.value


    res$is.prop.test.p[i] <- outl1$proportion$p.value


    if (specs$bootstrap[j]){
      res$is.dist1.boot.L2.p[i] <- dist1.boot$boot.p.L2
      res$is.dist1.boot.L1.p[i] <- dist1.boot$boot.p.L1
      res$is.dist1.boot.dist.p[i] <- dist1.boot$boot.p.dist

      res$is.prop.boot.p[i] <- dist1.boot$boot.p.prop
      res$is.prop.boot.test.p[i] <- dist1.boot$boot.p.prop.stat

    }

    res$is.avg.dist.pct[i] <- coefdif.prop.abs.m
    res$is.euclid[i] <- coefdif.euclid

  } #i loop closed

  res$id <- id
  res$id.seq <- id.seq

  list.res[[j]] <- res

  save(res, file = here("data-raw", "simulations",paste0(paste(specs[j,],collapse = "_"),".RData")))

} #j loop  closed



save(list.res, file = here("data-raw",paste0("outlier_sim_loopresult_full_",Sys.Date(),".RData")))




########### Analyse Simulation: compare p-values to 0.01 and 0.05 cut-offs

sum_list <- list()
sum_rej_05_list <- list()
sum_rej_01_list <- list()

for (l in 1:spec_n){

  # l <- 1
  res <- list.res[[l]]

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

if (alternative){
  write.csv(sum_tab, here("data-raw/simulations/alt_v1_boot_wprop.csv"), row.names = FALSE)
  #write.csv(sum_tab, "./simulations/alt_v1_boot_wprop.csv", row.names = FALSE)
  #  write.csv(uncond, "./simulations/uncond_alt_v2.csv", row.names = FALSE)

} else {
  write.csv(sum_tab, here("data-raw/simulations/null_v1_boot_wprop.csv"), row.names = FALSE)
  #write.csv(sum_tab, "./simulations/null_v1_boot_wprop.csv", row.names = FALSE)
  #  write.csv(uncond, "./simulations/uncond_null_v2.csv", row.names = FALSE)
}


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

print(paste("Simulation Complete in:"))
print(time.taken)

