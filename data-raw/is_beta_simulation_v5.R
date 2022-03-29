####
#### Simulation for the IS distortion tests: runs either under the alternative or under the null
####

####

#library(getspanel)
devtools::load_all()
library(gets)
library(here)
library(boot)
library(foreach)

#install.packages("metap")
#install.packages("goftest")

#install.packages("multtest")
#install.packages("survcomp")

# library(metap)
# library(goftest)
# library(gets)
# library(car)
library(purrr)
# library(KSgeneral)

#install.packages("KSgeneral")

##### now includes bootstrap option
#reps <- data.frame(bootstrap = 1000, no_bootstrap = 10000) #number of simulation replications
#alternative <- FALSE #whether we are under the null or no

#bootstrap <- TRUE #does not work yet with AR terms

# General Options ---------------------------------------------------------
#
# ## bootstrap options
# #nboot <- 1000 #number of bootstrap replications
# clean.sample_c <- c(FALSE, TRUE) #bootstrap on clean sample, TRUE or FALSE
# boot.pval.scale_c <- c(1, 5) #scale the p-value
# # boot.parallel <- TRUE #parallelize bootstrap (significant speed gains!)
# # cores <- parallel::detectCores()-1 #number of cores for bootstrap
# # cores <- 7 #number of cores for bootstrap
# parametric <- FALSE #nonparametric or parametric bootstrap
#
#
# max.block.size <- 30 #block size for isat, =2 is fastest.
#
# set.seed(123)
# ### number of regressors
# #px <- c(5) #regressors
# #ar_c <- c(0)#ar coefficient in DGP#### ar non-zero does not yet work for bootstrap, as it's not clear what the null model should be on 'clean' observations, and bootstrap also unclear
# #sample_c <- c(50, 100) #sample size
#
# #p_alpha_c <- c(0.05) #significance level for isat testing
#
# ############### Specifications for alternative
# #lambda_c <- c(6) ### outlier magnitude:
# out_prop_c <- c(0.1) ### outlier proportion:
#
# ### distribution in sample
# loc_limits_c <- c(1) #outliers distributed from 0 to this value times the sample T
#
# ####distribution of error terms
# dist_c <- c("norm", "t3") #can choose from: ("norm", "t3", "cauchy", "uniform", "lognorm")
#

# turn off arx warning
options(mc.warning = FALSE)


####### Combining all the above options into a list of specifications to be looped over:
general_options <- list(
  beta_coef = c(0.5,1), # Coefficients of the covariates
  dist = c("norm","t3"),  # reference distributions
  #max.block.size = 30, # block size for isat
  out_prop = 0.1, # proportion of sample to be outliers
  sample = c(50,100,200,400),
  nreg = 5,
  p_alpha = 0.05
)


general_asymptotic_options <- list(
  bootstrap = FALSE,
  nreg = 1,
  beta_coef = 1,
  lambda = c(2,3,4,6),
  sample = c(100,200,300,400,500),
  reps = 10000,
  ar = c(0,0.5),
  out_prop = 0.05,
  hypothesis = "alternative",
  dist = "norm",
  p_alpha = 0.05,
  loc_limits = 1,
  parametric = NA,
  timeseries = NA,
  nboot = NA,
  boot.pval.scale = 1,
  clean.sample = NA,
  bad_leverage = TRUE
)


general_bootstrap_options <- list(
  bootstrap = TRUE,
  boot.pval.scale = 1, # value to scale the p-value by
  nboot = 499,
  clean.sample = c(TRUE,FALSE),
  bad_leverage = c(TRUE,FALSE)

)

general_null_options <- list(
  hypothesis = "null",
  lambda = NA,
  loc_limits = NA
)


general_alternative_options <- list(
  hypothesis = "alternative",
  lambda = c(2,4,6),
  loc_limits = 1 #outliers distributed from 0 to this value times the sample T
)

general_ar_options <- list(
  ar = c(0,0.5),
  parametric = c(TRUE,FALSE),
  timeseries = c(TRUE,FALSE)
)


# Add the specs together --------------------------------------------------

specs_asym <- expand.grid(general_asymptotic_options)

specs_alt_BS <- expand.grid(append(general_options,
                                   append(general_bootstrap_options,
                                          append(general_alternative_options,
                                                 append(general_ar_options, list(reps = 500))))))

specs_null_BS <- expand.grid(append(general_options,
                                    append(general_bootstrap_options,
                                           append(general_null_options,
                                                  append(general_ar_options, list(reps = 1000))))))



specs_BS <- rbind(specs_alt_BS, specs_null_BS)

specs <- bind_rows(specs_asym,specs_BS)


# add lognormal
specs <- rbind(specs, data.frame(
  dist = "lognorm",

  beta_coef = 0.5,
  out_prop = NA, sample = 400, nreg = 5, p_alpha = 0.05, bootstrap = TRUE,
  reps = 500, nboot = 499, boot.pval.scale = 1, clean.sample = FALSE,
  bad_leverage = FALSE, hypothesis = "null", lambda = NA,
  loc_limits = NA, ar = 0, parametric = FALSE, timeseries = FALSE))




specs$hypothesis <- as.character(specs$hypothesis)
specs$dist <- as.character(specs$dist)
specs$id <- 1001:(nrow(specs)+1000)


# modify bad leverage
specs$nreg[specs$bad_leverage] <- 1 # inspired by Dehon et al
specs$out_prop[specs$bad_leverage] <- 0.05 # inspired by Dehon et al

###drop a few specs that are redundant
spec.drop <- vector()
spec.drop <- c(spec.drop,specs$id[(specs$ar == 0 & specs$timeseries == TRUE)]) # if AR = 0 then we don't need TS treatment
spec.drop <- c(spec.drop,specs$id[(specs$beta_coef == 1 & specs$bad_leverage == FALSE)]) # we don't want other coefficients than 0.5 unless for bad leverage points
spec.drop <- c(spec.drop,specs$id[(specs$beta_coef == 0.5 & specs$bad_leverage == TRUE)])# we don't want other coefficients than 0.5 unless for bad leverage points
spec.drop <- c(spec.drop,specs$id[(specs$ar > 0 & specs$timeseries == FALSE)]) # if AR > 0 then we don't need non-TS treatment

spec.drop <- c(spec.drop,specs$id[(specs$hypothesis == "null" & specs$bad_leverage == TRUE)])
spec.drop <- c(spec.drop,specs$id[(specs$parametric & !specs$clean.sample)]) # when we have parametric we always want to have a clean sample

spec.drop <- c(spec.drop,specs$id[(specs$bootstrap & specs$parametric & !specs$clean.sample & specs$hypothesis=="alternative")]) # should not return any because clean.sample must be true when parametric
spec.drop <- c(spec.drop,specs$id[(specs$bootstrap & !specs$parametric & specs$hypothesis=="alternative" & !specs$timeseries & specs$clean.sample & specs$dist=="t3")]) # when we have non-parametric non-TS alt we don't want the combination of T3 and clean.sample
spec.drop <- c(spec.drop,specs$id[(specs$bootstrap & specs$parametric & specs$hypothesis == "alternative" & !specs$timeseries & specs$clean.sample & specs$dist=="t3")])
spec.drop <- c(spec.drop,specs$id[(specs$bootstrap & !specs$parametric & specs$hypothesis == "alternative" & specs$timeseries & specs$clean.sample & specs$dist=="t3")])
spec.drop <- c(spec.drop,specs$id[(specs$bootstrap & specs$parametric & specs$hypothesis == "alternative" & specs$timeseries & specs$clean.sample & specs$dist=="t3")]) # Drop t3 for alt param TS because clean overrejects

# Bad Leverage
spec.drop <- c(spec.drop,specs$id[specs$bootstrap & specs$bad_leverage & specs$ar > 0])
spec.drop <- c(spec.drop,specs$id[specs$bootstrap & specs$bad_leverage & specs$parametric])
spec.drop <- c(spec.drop,specs$id[specs$bootstrap & specs$bad_leverage & specs$timeseries])
spec.drop <- c(spec.drop,specs$id[specs$bootstrap & specs$bad_leverage & specs$dist == "t3"])

# Asymptotic
spec.drop <- c(spec.drop,specs$id[!specs$bootstrap & !specs$bad_leverage])

# Deprecated
spec.drop <- c(spec.drop,specs$id[specs$ar > 0 & specs$p_alpha != 0.05]) # deprecated will not return any
spec.drop <- c(spec.drop,specs$id[specs$clean.sample == FALSE & specs$boot.pval.scale > 1]) # deprecated will not return any

# Actually Drop
specs <- specs[!(specs$id %in% spec.drop),]


# Create sequential id
specs$id.seq <- seq(1:NROW(specs))

specs


spec_n <- nrow(specs)


# Finalise the specs section (order) --------------------------------------------------

spec_order <- specs$id.seq
spec_order <- specs$id.seq[specs$bootstrap] # only BS
spec_order <- spec_order[order(specs[spec_order,"sample"])]


### Changes to the current run - not changing the fundamental structure
#
#


#
# specs <- failed_df
# specs <- specs[specs$id == 1836,]
# specs$nboot <- 20

save(specs, file = here("data-raw", "simulations/rr2203", "spec_list.RData"))

use_parallel <- TRUE


# Start of spec loop ------------------------------------------------------
#for (j in specs$id.seq){
#for (j in spec_order){
for (j in spec_order){

  #for (j in new_order_final$id.seq){
  #j <- 1
  print(j)
  # if(file.exists(here("data-raw", "simulations/rr2203",paste0(paste(specs[j,],collapse = "_"),".RData")))){next}
  # if(file.exists(here("data-raw", "simulations/rr2203",paste0("Running_",j,".txt")))){
  #   next
  # } else {
  #   file.create(here("data-raw", "simulations/rr2203",paste0("Running_",j,".txt")))
  # }

  p_alpha <-   specs$p_alpha[j]
  ar <- specs$ar[j]

  if (specs$bootstrap[j]){

    clean.sample <- specs$clean.sample[j]
    boot.pval.scale <- specs$boot.pval.scale[j]
  }

  p <- specs$nreg[j]
  dist <- specs$dist[j]

  bad_leverage <- specs$bad_leverage[j]

  if (specs$hypothesis[j] == "alternative"){
    out_lam <- specs$lambda[j]
    out_prop <- specs$out_prop[j]
  }


  id <- specs$id[j]
  id.seq <- specs$id.seq[j]
  Tlength <- specs$sample[j]
  loc_limit <-   floor(Tlength*1)

  if (p > 0){
    pcoef <- c(rep(specs$beta_coef[j], p))
  } else {
    pcoef <- NULL
  }


  is.dates.list <- list()
  isalt.dates.list <- list()

  sis.dates.list <- list()

  # res <- data.frame(matrix(NA, specs$reps[j], 2))
  # names(res) <- c("rep", "is.dist1.p")
  #
  # res$is.dist1.boot.L2.p <- NA
  # res$is.dist1.boot.L1.p <- NA
  # res$is.dist1.boot.dist.p <- NA
  #
  # res$is.dist1.boot.var.p <- NA
  #
  # res$is.prop.test.p <- NA
  # res$is.prop.boot.p <- NA
  # res$is.prop.boot.test.p <- NA
  #
  # res$is.avg.dist.pct <- NA
  # res$is.euclid <- NA
  #

  # if (specs$hypothesis[j]=="alternative"){
  #   print(paste0("Alt | ", "Spec: ", j,  " of ", spec_n, " |", paste(colnames(specs),specs[j,],  collapse=", ")))
  #
  # } else {
  #   print(paste0("Null | ", "Spec: ", j,  " of ", spec_n, " |", paste(colnames(specs),specs[j,],  collapse=", ")))
  #
  # }

  # Start of rep loop in parallel ------------------------------------------------------

  # Setting up the parallel system on Linux and Windows
  if(exists("res")){rm(res)}
  if(!Sys.info()["sysname"]=="Windows" & use_parallel){
    library(doMC)
    registerDoMC(detectCores()-2)}  # coefsamples if enough cores available - otherwise total-1
  # } else {
  #   parallel::makeCluster((parallel::detectCores()-1))
  # }

  #Starting the loop
  res <- foreach(i = 1:specs$reps[j],
                 .packages = loadedNamespaces(),
                 .errorhandling = "pass",
                 .combine = "rbind"
  ) %dopar% {

    #res <- data.frame()

    #for (i in 1:specs$reps[j]){
    #i <- 1
    print(i)

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

      if (bad_leverage){
        out_lam_x <- specs$lambda[j]
        out_lam <- 0
      }

    } else {
      noutl <- 0
      out_loc <- NULL
      out_lam <- 0
      out_lam_x <- 0
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
    if (p > 0){
      if (bad_leverage){
        if (p==1){
          mx_T1_lev <- mx_T1
          mx_T1_lev[which(out_loc_s==1)] <- mx_T1_lev[which(out_loc_s==1)]  + out_lam_x
          mx_T1 <- mx_T1_lev

          mx_lev <- mx
          mx_lev[which(out_loc_s==1)] <- mx_lev[which(out_loc_s==1)]  + out_lam_x
          mx <- mx_lev
        } else {
          mx_T1_lev <- mx_T1
          mx_T1_lev[which(out_loc_s==1),] <- mx_T1_lev[which(out_loc_s==1),]  + out_lam_x
          mx_T1 <- mx_T1_lev

          mx_lev <- mx
          mx_lev[which(out_loc_s==1),] <- mx_lev[which(out_loc_s==1),]  + out_lam_x
          mx <- mx_lev
        }
      }
    }


    ## ISAT --------------------------------------------------------------------

    if (ar != 0){
      is1 <- isat(y_x_T1, mxreg=mx_T1, ar=1, iis=TRUE, sis=FALSE, tis=FALSE,
                  t.pval=p_alpha, plot=FALSE, print.searchinfo = FALSE,
                  max.block.size=30)

    } else {
      is1 <- isat(y_x, mxreg=mx, ar=NULL, iis=TRUE, sis=FALSE, tis=FALSE,
                  t.pval=p_alpha, plot=FALSE, print.searchinfo = FALSE,
                  max.block.size=30)

    }



    dist1 <- distorttest(is1, coef="all")
    outl1 <- outliertest(is1)


    # Bootstrap ---------------------------------------------------------------
    if (specs$bootstrap[j]){
      #if (FALSE){

      # j
      # i
      # boot.pval.scale <- 1
      #
      # nboot <- 199
      #
      #  set.seed(123)

      # N <- is1$aux$y.n
      # if (!is.null(m_out_n)){
      #   m <- round((2/3)*N)
      # } else {
      #   m <- NULL
      # }

      dist1.boot <-  distorttest.boot(
        x = is1,
        nboot = specs$nboot[j],
        ols.only = specs$only.ols[j],
        m = NULL,
        clean.sample = specs$clean.sample[j],
        parametric = specs$parametric[j],
        scale.t.pval = specs$boot.pval.scale[j],
        parallel = FALSE,
        #parallel = TRUE,
        #ncore = 1,
        #ncore = parallel::detectCores()-1,
        max.block.size = 30,
        timeseries = specs$timeseries[j],
        raw_data = cbind(y_x_T1,mx_T1)
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


    # Collect results into res ---------------------------------------------------------


    res_local <- data.frame(rep = i,
                            is.dist1.p = dist1$p.value,
                            is.prop.test.p = outl1$proportion$p.value,
                            is.avg.dist.pct = coefdif.prop.abs.m,
                            is.euclid = coefdif.euclid)

    if (specs$bootstrap[j]){
      res_local <- data.frame(
        res_local,
        data.frame(
          is.dist1.boot.L2.p = dist1.boot$boot.p.L2,
          is.dist1.boot.L1.p = dist1.boot$boot.p.L1,
          is.dist1.boot.dist.p = dist1.boot$boot.p.dist,

          is.dist1.boot.var.p = as.numeric(dist1.boot$boot.var.p),
          is.prop.boot.p = dist1.boot$boot.p.prop,
          is.prop.boot.test.p = dist1.boot$boot.p.prop.stat))

    }

    as.vector(res_local)
    #res <- rbind(res,res_local)


    # this is the original res below
    # res$rep[i] <- i
    # res$is.dist1.p[i] <- dist1$p.value
    #
    #
    # res$is.prop.test.p[i] <- outl1$proportion$p.value
    #
    #
    # if (specs$bootstrap[j]){
    #   res$is.dist1.boot.L2.p[i] <- dist1.boot$boot.p.L2
    #   res$is.dist1.boot.L1.p[i] <- dist1.boot$boot.p.L1
    #   res$is.dist1.boot.dist.p[i] <- dist1.boot$boot.p.dist
    #
    #   res$is.dist1.boot.var.p[i] <- as.numeric(dist1.boot$boot.var.p)
    #   res$is.prop.boot.p[i] <- dist1.boot$boot.p.prop
    #   res$is.prop.boot.test.p[i] <- dist1.boot$boot.p.prop.stat
    #
    # }
    #
    # res$is.avg.dist.pct[i] <- coefdif.prop.abs.m
    # res$is.euclid[i] <- coefdif.euclid

    #print(res_local)

  } #i loop closed

  res$id <- id
  res$id.seq <- id.seq

  #res

  #list.res[[j]] <- res

  save(res, file = here("data-raw", "simulations/rr2203",paste0(paste(specs[j,],collapse = "_"),".RData")))
  unlink(here("data-raw", "simulations/rr2203",paste0("Running_",j,".txt")))
} #j loop  closed
