#' Log-Likelihood Function for a plm object
#'
#' @param object A plm object
#'
#' @return The Log-Likelihood
#'
#'
logLik.plm <- function(object){
  -plm:::nobs.panelmodel(summary(object)) * log(2 * var(object$residuals) * pi)/2 - plm:::deviance.panelmodel(summary(object))/(2 * var(object$residuals))
}
