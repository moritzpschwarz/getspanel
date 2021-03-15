library(here)

##### now includes bootstrap option
reps <- 3 #number of simulation replications
alternative <- FALSE #whether we are under the null or no

bootstrap <- TRUE #does not work yet with AR terms

#### bootstrap options
nboot <- 100 #number of bootstrap replications
clean.sample_c <- c(FALSE, TRUE) #bootstrap on clean sample, TRUE or FALSE
boot.pval.scale_c <- c(1, 5) #scale the p-value
boot.parallel <- TRUE #parallelize bootstrap (significant speed gains!)
cores <- 7 #number of cores for bootstrap
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
if (bootstrap==FALSE){
  
  if (alternative == TRUE){
    spec_list <- list(p_alpha = p_alpha_c, lambda = lambda_c, out_prop = out_prop_c, nreg = px, sample=sample_c, ar=ar_c, dist=dist_c)
  } else { #null simulation
    spec_list <- list(p_alpha = p_alpha_c, nreg = px, sample=sample_c, ar=ar_c, dist=dist_c)
    
  }
  
} else { #bootstrap is true
  
  if (alternative == TRUE){
    spec_list <- list(p_alpha = p_alpha_c, lambda = lambda_c, out_prop = out_prop_c, nreg = px, sample=sample_c, ar=ar_c, dist=dist_c, clean.sample = clean.sample_c, boot.pval.scale=boot.pval.scale_c)
  } else { #null simulation
    spec_list <- list(p_alpha = p_alpha_c, nreg = px, sample=sample_c, ar=ar_c,  dist=dist_c, clean.sample = clean.sample_c, boot.pval.scale=boot.pval.scale_c)
    
  }
  
}

specs <- expand.grid(spec_list)
specs$id <- seq(1:NROW(specs))

###drop a few specs that are redundant
spec.drop <- specs$id[specs$ar > 0 & specs$nreg != 5]
specs <- specs[!(specs$id %in% spec.drop),]

spec.drop <- specs$id[specs$ar > 0 & specs$p_alpha != 0.05]
specs <- specs[!(specs$id %in% spec.drop),]

spec.drop <- specs$id[specs$clean.sample == FALSE & specs$boot.pval.scale > 1]
specs <- specs[!(specs$id %in% spec.drop),]


save(specs, file = here("data-raw","specs_outlier_simulations.RData"))
