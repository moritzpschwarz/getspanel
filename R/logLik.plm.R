#' Log-Likelihood Function for a plm object
#'
#' @param object A plm object
#' @param ... Further Arguments
#'
#' @return The Log-Likelihood
#' @export
#'
logLik.plm <- function(object, ...){
  -plm::nobs(object) * log(2 * var(object$residuals) * pi)/2 - deviance(object)/(2 * var(object$residuals))
}
