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

#### bootstrap options
#nboot <- 1000 #number of bootstrap replications
clean.sample_c <- c(FALSE, TRUE) #bootstrap on clean sample, TRUE or FALSE
boot.pval.scale_c <- c(1, 5) #scale the p-value
# boot.parallel <- TRUE #parallelize bootstrap (significant speed gains!)
# cores <- parallel::detectCores()-1 #number of cores for bootstrap
# cores <- 7 #number of cores for bootstrap
parametric <- FALSE #nonparametric or parametric bootstrap
####

max.block.size <- 30 #block size for isat, =2 is fastest.

set.seed(123)
### number of regressors
#px <- c(5) #regressors
#ar_c <- c(0)#ar coefficient in DGP#### ar non-zero does not yet work for bootstrap, as it's not clear what the null model should be on 'clean' observations, and bootstrap also unclear
#sample_c <- c(50, 100) #sample size

#p_alpha_c <- c(0.05) #significance level for isat testing

############### Specifications for alternative
#lambda_c <- c(6) ### outlier magnitude:
out_prop_c <- c(0.1) ### outlier proportion:

### distribution in sample
loc_limits_c <- c(1) #outliers distributed from 0 to this value times the sample T

####distribution of error terms
dist_c <- c("norm", "t3") #can choose from: ("norm", "t3", "cauchy", "uniform", "lognorm")








####### Combining all the above options into a list of specifications to be looped over:


general_options <- list(
  beta_coef = c(0.5,1), # Coefficients of the covariates
  dist = c("norm","t3"),  # reference distributions
  max.block.size = 30, # block size for isat
  out_prop = 0.1, # proportion of sample to be outliers
  sample = c(50,100,200,400),
  nreg = 5,
  p_alpha = 0.05
)


general_asymptotic_options <- list(
  bootstrap = FALSE,
  reps = 10000,
  nboot = NA,
  boot.pval.scale = 1,
  clean.sample = NA,
  bad_leverage = FALSE
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

specs_alt_nonBS <- expand.grid(append(general_options,
                                      append(general_asymptotic_options,
                                             append(general_alternative_options, general_ar_options))))

specs_null_nonBS <- expand.grid(append(general_options,
                                       append(general_asymptotic_options,
                                              append(general_null_options, general_ar_options))))



specs_alt_BS <- expand.grid(append(general_options,
                                   append(general_bootstrap_options,
                                          append(general_alternative_options,
                                                 append(general_ar_options, list(reps = 500))))))

specs_null_BS <- expand.grid(append(general_options,
                                    append(general_bootstrap_options,
                                           append(general_null_options,
                                                  append(general_ar_options, list(reps = 1000))))))



specs_BS <- rbind(specs_alt_BS, specs_null_BS)
specs_asym <- rbind(specs_alt_nonBS,specs_null_nonBS)

setdiff(names(specs_asym), names(specs_BS))


specs <- rbind(specs_asym,specs_BS)


# add lognormal
specs <- rbind(specs, data.frame(
  dist = "lognorm",

  beta_coef = 0.5,  max.block.size = 30,
  out_prop = NA, sample = 400, nreg = 5, p_alpha = 0.05, bootstrap = TRUE,
  reps = 500, nboot = 499, boot.pval.scale = NA, clean.sample = FALSE,
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
spec.drop <- c(spec.drop,specs$id[(specs$ar == 0 & specs$timeseries == TRUE)]) # if AR = 0 then we don't need TS treamtment
spec.drop <- c(spec.drop,specs$id[(specs$beta_coef == 1 & specs$bad_leverage == FALSE)]) # we don't want other coefficients than 0.5 unless for bad leverage points
spec.drop <- c(spec.drop,specs$id[(specs$beta_coef == 0.5 & specs$bad_leverage == TRUE)])# we don't want other coefficients than 0.5 unless for bad leverage points
spec.drop <- c(spec.drop,specs$id[(specs$ar > 0 & specs$timeseries == FALSE)]) # if AR > 0 then we don't need non-TS treatment
spec.drop <- c(spec.drop,specs$id[(specs$ar > 0 & specs$nreg != 5)])
spec.drop <- c(spec.drop,specs$id[(specs$hypothesis == "null" & specs$bad_leverage == TRUE)])
spec.drop <- c(spec.drop,specs$id[(specs$parametric & !specs$clean.sample)]) # when we have parametric we always want to have a clean sample
spec.drop <- c(spec.drop,specs$id[(specs$bootstrap & specs$parametric & !specs$clean.sample & specs$hypothesis=="alternative")]) # should not return any because clean.sample must be true when parametric
spec.drop <- c(spec.drop,specs$id[(!specs$parametric & specs$hypothesis=="alternative" & !specs$timeseries & specs$bootstrap & specs$clean.sample & specs$dist=="t3")]) # when we have non-parametric non-TS alt we don't want the combination of T3 and clean.sample
spec.drop <- c(spec.drop,specs$id[(specs$parametric & specs$hypothesis == "alternative" & !specs$timeseries & specs$bootstrap & specs$clean.sample & specs$dist=="t3")])
spec.drop <- c(spec.drop,specs$id[(!specs$parametric & specs$hypothesis == "alternative" & specs$timeseries & specs$bootstrap & specs$clean.sample & specs$dist=="t3")])
spec.drop <- c(spec.drop,specs$id[(specs$parametric & specs$hypothesis == "alternative" & specs$timeseries & specs$bootstrap & specs$clean.sample & specs$dist=="t3")]) # Drop t3 for alt param TS because clean overrejects

# Bad Leverage
spec.drop <- c(spec.drop,specs$id[specs$bad_leverage & specs$ar > 0])
spec.drop <- c(spec.drop,specs$id[specs$bad_leverage & specs$parametric])
spec.drop <- c(spec.drop,specs$id[specs$bad_leverage & specs$timeseries])
spec.drop <- c(spec.drop,specs$id[specs$bad_leverage & specs$dist == "t3"])

# Deprecated
spec.drop <- c(spec.drop,specs$id[specs$ar > 0 & specs$p_alpha != 0.05]) # deprecated will not return any
spec.drop <- c(spec.drop,specs$id[specs$clean.sample == FALSE & specs$boot.pval.scale > 1]) # deprecated will not return any


specs <- specs[!(specs$id %in% spec.drop),]
specs$id.seq <- seq(1:NROW(specs))


specs







#################### All the different specifications to be looped over, each with "reps" number of replications.



# ###### Total runs:
# spec_n*reps

## DELETE THIS!!!!
# specs <- specs[specs$id==740,]
# specs$reps <- 1
# specs$nboot <- 10
###

spec_n <- NROW(specs)
spec_n


# Finalise the specs section (order) --------------------------------------------------

list.res <- list()
list.reg <- list()

start.time <- Sys.time()

#save(specs, file = here("data-raw", "simulations", "spec_list.RData"))

# library(doMC)
# registerDoMC(detectCores()-1)  # coefsamples if enough cores available - otherwise total-1
# foreach(j = 1:spec_n, .packages = loadedNamespaces()) %dopar% {

#for (j in 1:spec_n){

spec_order <- 1:spec_n
spec_order <- spec_order[!spec_order < 401]
spec_order <- spec_order[order(specs[spec_order,"sample"])]

new_order <- specs[spec_order,c("id.seq","sample","lambda")]
new_order1 <- new_order[new_order$sample != 150,]
new_order2 <- new_order1[new_order1$lambda %in% c(2,4,6,NA),]

new_order3 <- new_order[new_order$sample == 150,]
new_order4 <- new_order[new_order$lambda %in% 3 & new_order$sample!=150,]

new_order_final <- rbind(rbind(new_order2, new_order3),new_order4)

# specs <- rbind(specs, data.frame(bootstrap = TRUE, hypothesis = "null",
#                                  reps = 100,
#                                  p_alpha = 0.05, nreg = 5, sample = 50, ar = 0.5,
#                                  dist = "norm",
#                                  lambda = NA, out_prop = NA, nboot = 99,
#                                  clean.sample = TRUE,
#                                  boot.pval.scale = 1, id = 1, id.seq = 521))
#
# specs <- rbind(specs, data.frame(bootstrap = TRUE, hypothesis = "alternative",
#                                  reps = 20,
#                                  p_alpha = 0.05, nreg = 5, sample = 50, ar = 0.5,
#                                  dist = "norm",
#                                  lambda = 6, out_prop = 0.1, nboot = 99,
#                                  clean.sample = TRUE,
#                                  boot.pval.scale = 1, id = 2, id.seq = 522))
#
# specs <- rbind(specs, data.frame(bootstrap = TRUE, hypothesis = "alternative",
#                                  reps = 20,
#                                  p_alpha = 0.05, nreg = 5, sample = 100, ar = 0.000001,
#                                  dist = "norm",
#                                  lambda = 6, out_prop = 0.1, nboot = 99,
#                                  clean.sample = TRUE,
#                                  boot.pval.scale = 1, id = 523, id.seq = 523))
#
# specs <- rbind(specs, data.frame(bootstrap = TRUE, hypothesis = "null",
#                                  reps = 20,
#                                  p_alpha = 0.05, nreg = 5, sample = 50, ar = 0.5,
#                                  dist = "norm",
#                                  lambda = NA, out_prop = NA, nboot = 99,
#                                  clean.sample = FALSE,
#                                  boot.pval.scale = 1, id = 524, id.seq = 524))
#
# specs <- rbind(specs, data.frame(bootstrap = TRUE, hypothesis = "null",
#                                  reps = 20,
#                                  p_alpha = 0.05, nreg = 5, sample = 50, ar = 0.5,
#                                  dist = "norm",
#                                  lambda = NA, out_prop = NA, nboot = 99,
#                                  clean.sample = TRUE,
#                                  boot.pval.scale = 1, id = 525, id.seq = 525))
#
# specs <- rbind(specs, data.frame(bootstrap = TRUE, hypothesis = "alternative",
#                                  reps = 10,
#                                  p_alpha = 0.05, nreg = 5, sample = 100, ar = 0.5,
#                                  dist = "norm",
#                                  lambda = 6, out_prop = 0.1, nboot = 99,
#                                  clean.sample = TRUE,
#                                  boot.pval.scale = 1, id = 526, id.seq = 526))
#
#
#
# spec_list_null <- list(
#   sample = c(100,200),
#   hypothesis = "null",
#   bootstrap = TRUE,
#   reps = 100,
#   clean.sample = c(TRUE,FALSE),
#   nboot = 99,
#   timeseries = TRUE,
#   parametric = c(TRUE, FALSE),
#   dist = c("norm","t3"),
#   lambda = NA,
#   out_prop = NA,
#   nreg = 5,
#   p_alpha = 0.05,
#   boot.pval.scale = 1,
#   ar = 0.5
# )
#
# spec_list_alt <- list(
#   sample = c(100,200),
#   hypothesis = "alternative",
#   bootstrap = TRUE,
#   reps = 50,
#   clean.sample = c(TRUE,FALSE),
#   nboot = 99,
#   timeseries = TRUE,
#   parametric = c(TRUE, FALSE),
#   dist = c("norm","t3"),
#   lambda = 6,
#   out_prop = 0.1,
#   nreg = 5,
#   p_alpha = 0.05,
#   boot.pval.scale = 1,
#   ar = 0.5
#
# )
#
#
# specs <- rbind(expand.grid(spec_list_null),expand.grid(spec_list_alt))
# specs$id.seq <- 601:632
# specs$id <- 601:632
#
# a <- matrix(NA, nrow = 600, ncol = ncol(specs))
# colnames(a) <- names(specs)
# specs <- rbind(a, specs)
#
# specs <- as.data.frame(specs, row.names = NULL)
#
# #specs[602,c("reps","nboot")] <- c(3,99)
#

### Changes to the current run - not changing the fundamental structure
#
# specs <- specs[specs$bootstrap == TRUE,]
# specs <- specs[specs$lambda > 3 | is.na(specs$lambda),]
# specs <- specs[!(specs$parametric & !specs$clean.sample),]
#
# specs <- specs[order(specs$sample),]
#
# specs$id.seq <- 1:nrow(specs)
#
#
#
# specs$nboot <- 199
# specs$reps <- 200
# #save(specs, file = here("data-raw", "simulations/rr2203", "spec_list_trial.RData"))
#
#
#
# # #1836      specs <- failed_df
# # specs <- specs[specs$id == 1836,]
# # specs$id.seq <- 1:nrow(specs)
# # specs$nboot <- 20
# # specs$reps <- 3
#
#
#
#
# specs1 <- data.frame(nboot = 99,
#                      reps = 50,
#                      hypothesis = "null",
#                      parametric = TRUE,
#                      timeseries = FALSE,
#                      beta_coef = 0.5,
#                      sample = 50,
#                      dist = "norm",
#                      max.block.size = 30,
#                      nreg = 5,
#                      ar = 0,
#                      p_alpha = 0.05,
#                      bootstrap = TRUE,
#                      clean.sample = TRUE,
#                      bad_leverage = FALSE,
#                      loc_limits = 1,
#                      id = 1,
#                      id.seq = 1,
#                      out_prop = NA,
#                      boot.pval.scale = 1,
#                      lambda = NA)
#
# specs <- structure(list(beta_coef = c(1, 1, 1, 1, 0.5, 0.5, 1, 1, 0.5,
#                                       0.5, 1, 1, 0.5, 0.5, 1),
#                         dist = c("t3", "t3", "norm", "t3", "norm",
#                                  "t3", "norm", "t3", "norm", "t3", "norm", "t3", "norm", "t3",
#                                  "norm"),
#                         max.block.size = c(30, 30, 30, 30, 30, 30, 30, 30, 30,
#                                            30, 30, 30, 30, 30, 30),
#                         out_prop = c(0.1, 0.1, 0.1, 0.1, 0.1,
#                                      0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
#                         sample = c(50,
#                                    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 100),
#                         nreg = c(5,
#                                  5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5),
#                         p_alpha = c(0.05,
#                                     0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
#                                     0.05, 0.05, 0.05),
#                         bootstrap = c(TRUE, TRUE, TRUE, TRUE, TRUE,
#                                       TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
#                         reps = c(200, 200, 200, 200, 200, 200, 200, 200, 200, 200,
#                                  200, 200, 200, 200, 200),
#                         nboot = c(199, 199, 199, 199, 199,
#                                   199, 199, 199, 199, 199, 199, 199, 199, 199, 199),
#                         boot.pval.scale = c(1,
#                                             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#                         clean.sample = c(TRUE,
#                                          TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
#                                          FALSE, FALSE, FALSE, FALSE, FALSE),
#                         bad_leverage = c(TRUE,
#                                          TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE,
#                                          TRUE, TRUE, FALSE, FALSE, TRUE),
#                         hypothesis = c("alternative",
#                                        "alternative", "alternative", "alternative", "alternative",
#                                        "alternative", "alternative", "alternative", "alternative",
#                                        "alternative", "null", "null", "null", "null", "alternative"
#                         ),
#                         lambda = c(4, 6, 4, 4, 4, 4, 6, 6, 6, 6, NA, NA, NA, NA,
#                                    4),
#                         loc_limits = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, NA,
#                                        NA, NA, 1),
#                         ar = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
#                                0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
#                         parametric = c(TRUE,
#                                        TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
#                                        FALSE, FALSE, FALSE, FALSE, FALSE),
#                         timeseries = c(TRUE,
#                                        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
#                                        TRUE, TRUE, TRUE, TRUE), id = c(1772L, 1836L, 2170L, 2172L,
#                                                                        2201L, 2203L, 2234L, 2236L, 2265L, 2267L, 3258L, 3260L, 3289L,
#                                                                        3291L, 2174L),
#                         id.seq = c(2L, 6L, 11L, 12L, 15L, 16L, 19L,
#                                    20L, 23L, 24L, 55L, 56L, 59L, 60L, 83L)),
#                    out.attrs = list(
#                      dim = c(beta_coef = 2L, dist = 2L, max.block.size = 1L, out_prop = 1L,
#                              sample = 4L, nreg = 1L, p_alpha = 1L, bootstrap = 1L, reps = 1L,
#                              nboot = 1L,
#                              boot.pval.scale = 1L, clean.sample = 1L,
#                              bad_leverage = 1L,
#                              hypothesis = 1L,
#                              lambda = 3L, loc_limits = 1L, ar = 2L, parametric = 2L,
#                              timeseries = 2L),
#                      dimnames = list(beta_coef = c("beta_coef=0.5",
#                                                    "beta_coef=1.0"), dist = c("dist=norm", "dist=t3"), max.block.size = "max.block.size=30",
#                                      out_prop = "out_prop=0.1", sample = c("sample= 50", "sample=100",
#                                                                            "sample=200", "sample=400"), nreg = "nreg=5", p_alpha = "p_alpha=0.05",
#                                      bootstrap = "bootstrap=FALSE", reps = "reps=10000", nboot = "nboot=NA",
#                                      boot.pval.scale = "boot.pval.scale=1", clean.sample = "clean.sample=NA",
#                                      bad_leverage = "bad_leverage=FALSE", hypothesis = "hypothesis=alternative",
#                                      lambda = c("lambda=2", "lambda=4", "lambda=6"), loc_limits = "loc_limits=1",
#                                      ar = c("ar=0.0", "ar=0.5"), parametric = c("parametric=TRUE",
#                                                                                 "parametric=FALSE"),
#                                      timeseries = c("timeseries=TRUE",
#                                                     "timeseries=FALSE"))),
#                    row.names = c(772L, 836L, 1170L,
#                                  1172L, 1201L, 1203L, 1234L, 1236L, 1265L, 1267L, 2258L, 2260L,
#                                  2289L, 2291L, 1174L), class = "data.frame")
#
# specs$id.seq <- 1:nrow(specs)
use_parallel <- TRUE
# Start of spec loop ------------------------------------------------------
for (j in specs$id.seq){
  #for (j in new_order_final$id.seq){
  # for (j in specs$id.seq[specs$id %in% c(1772L, 1836L, 2170L, 2172L, 2201L, 2203L, 2234L, 2236L, 2265L,
  #                                        2267L, 3260L)]){

  #j = 131
  #for (j in 602:602){
  #for (j in 601:632){
  #j = 129
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
    #bad_leverage <- specs$bad_leverage[j]

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

  if (specs$hypothesis[j]=="alternative"){
    print(paste0("Alt | ", "Spec: ", j,  " of ", spec_n, " |", paste(colnames(specs),specs[j,],  collapse=", ")))

  } else {
    print(paste0("Null | ", "Spec: ", j,  " of ", spec_n, " |", paste(colnames(specs),specs[j,],  collapse=", ")))

  }

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
                 #) %dopar% {
  ) %dopar% {

    #res <- data.frame()

    #for (i in 1:10){ #specs$reps[j]){
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
      is1 <- isat(y_x_T1, mxreg=mx_T1, ar=1, iis=TRUE, sis=FALSE, tis=FALSE, t.pval=p_alpha, plot=FALSE, print.searchinfo = FALSE, max.block.size=max.block.size)

    } else {
      is1 <- isat(y_x, mxreg=mx, ar=NULL, iis=TRUE, sis=FALSE, tis=FALSE, t.pval=p_alpha, plot=FALSE, print.searchinfo = FALSE, max.block.size=max.block.size)

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
        max.block.size = specs$max.block.size[j],
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

  res

  #list.res[[j]] <- res

  save(res, file = here("data-raw", "simulations/rr2203",paste0(paste(specs[j,],collapse = "_"),".RData")))
  #unlink(here("data-raw", "simulations/rr2203",paste0("Running_",j,".txt")))
} #j loop  closed
