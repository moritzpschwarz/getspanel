#### Sample Script for Variance Correction

set.seed(123)

########## Setup

y <- rnorm(100, 0, 1)
x <- rnorm(100, 0, 1)
pval = 0.01

library(gets)
is1 <- isat(y, mxreg=x, iis=TRUE, sis=FALSE, t.pval=pval, plot=TRUE)

#################################################################################
########## 1. Consistency correction of sigma estimate (affects all regressors)
vcov.mean <- is1$vcov.mean
vcov.mean.cor.1 <- is1$vcov.mean * as.numeric(isvarcor(is1$aux$t.pval, 1)[2]^2)
###################################################################################

###############################################################################################################
######### 2. Correction for the variance of retained regressors (affects only fixed regressors, not impulses)
vcov.mean.cor.2 <- vcov.mean.cor.1
rel_names <- is1$aux$mXnames[!(is1$aux$mXnames %in% is1$ISnames)]
mcor <- 1
vcov.mean.cor.2[rel_names, rel_names] <- vcov.mean.cor.2[rel_names, rel_names] * as.numeric(isvareffcor(is1$aux$t.pval, 1, mcor)[2]^2)
###############################################################################################################

############################################
#### Compare the three:
vcov.mean
vcov.mean.cor.1
vcov.mean.cor.2 #this is the final corrected VCOV matrix

#percentage change in Variance relative to original:
(vcov.mean.cor.1/vcov.mean)*100-100
(vcov.mean.cor.2/vcov.mean)*100-100
###############################################

###############################
###############################

#### Replace matrix in isat object if that's what's being read for the projections:
is1$vcov.mean <- vcov.mean.cor.2
