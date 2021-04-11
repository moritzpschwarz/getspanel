#' IIS Consistency Correction for isatobjects (S3 Method)
#' Consistency correction for estimate of residual variance when using impulse indicator saturation.
#' @param isatobject An isatobject as returned by gets::isat()
#' The Johansen and Nielsen (2016) impulse-indicator consistency correction for the estimated residual standard deviation.
#' @return An `isatobject`  containing the corrected $vcov.mean as well as the original vcov.mean under $vcov.mean.original and additionally the intermediate steps $vcov.mean.cor.1, which is the consistency correction of sigma estimate (affects all regressors) and $vcov.mean.cor.2, which is equal to the new $vcov.mean and represents the additional correction on top of $vcov.mean.cor.1 for the variance of retained regressors (affects only fixed regressors, not impulses).
#' @export
#'
#' @examples
#' set.seed(123)
#'
#' ########## Setup
#'
#' y <- rnorm(100, 0, 1)
#' x <- rnorm(100, 0, 1)
#' pval = 0.01
#'
#' is1 <- isat(y, mxreg=x, iis=TRUE, sis=FALSE, t.pval=pval, plot=TRUE)
#' is1.cor <- isvarcor(is1)
#'
#' # Modified vcov.mean
#' is1.cor$vcov.mean
#' # Equal to is1.cor$vcov.mean.cor.2
#'
#' # Intermediate Step
#' is1.cor$vcov.mean.cor.1
#'
#' # Original vcov.mean
#' is1.cor$vcov.mean.original

isvarcor.isat <- function(isatobject){


  #################################################################################
  ########## 1. Consistency correction of sigma estimate (affects all regressors)
  vcov.mean.original <- isatobject$vcov.mean

  vcov.mean.cor.1 <- isatobject$vcov.mean * as.numeric(isvarcor(isatobject$aux$t.pval, 1)[2]^2)
  ###################################################################################

  ###############################################################################################################
  ######### 2. Correction for the variance of retained regressors (affects only fixed regressors, not impulses)
  vcov.mean.cor.2 <- vcov.mean.cor.1
  rel_names <- isatobject$aux$mXnames[!(isatobject$aux$mXnames %in% isatobject$ISnames)]
  mcor <- 1
  vcov.mean.cor.2[rel_names, rel_names] <- vcov.mean.cor.2[rel_names, rel_names] * as.numeric(isvareffcor(isatobject$aux$t.pval, 1, mcor)[2]^2)
  ###############################################################################################################

  isatobject <- isatobject
  isatobject$vcov.mean <- vcov.mean.cor.2
  isatobject$vcov.mean.cor.1 <- vcov.mean.cor.1
  isatobject$vcov.mean.original <- vcov.mean.original

  # correcting the S.E. in isatobject
  isatobject$mean.results["std.error"] <- sqrt(diag(vcov.mean.cor.2))

  return(isatobject)
}
