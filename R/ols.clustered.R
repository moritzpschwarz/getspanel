#' Clustered OLS Function
#'
#' @param y
#' @param x
#' @param untransformed.residuals
#' @param tol
#' @param LAPACK
#' @param method
#' @param variance.spec
#' @param cluster
#' @param time
#' @param id
#' @param ...
#'
#' @return
#'
#'
#'
ols.clustered <- function (y, x, untransformed.residuals = NULL, tol = 1e-07,LAPACK = FALSE, method = 3, variance.spec = NULL, cluster = "individual", time, id,...){
  browser()
  if(method!=3){stop("Ols Method (vcov argument) not yet implemented. Set vcov.type to 'ordinary'. Contact the package author for more information.")}
  out <- list()
  out$n <- length(y)
  if (is.null(x)) {
    out$k <- 0
  }
  else {
    out$k <- NCOL(x)
  }
  out$df <- out$n - out$k
  if (out$k > 0) {
    qx <- qr(x, tol, LAPACK = LAPACK)
    out <- c(out, qx)
    out$coefficients <- solve.qr(qx, y, tol = tol)
    out$xtxinv <- chol2inv(qx$qr)
    out$fit <- as.vector(x %*% out$coefficients)
  }
  else {
    out$fit <- rep(0, out$n)
  }
  out$residuals <- y - out$fit
  out$residuals2 <- out$residuals^2
  out$rss <- sum(out$residuals2)
  out$sigma2 <- out$rss/out$df
  if (out$k > 0) {
    out$vcov <- out$sigma2 * out$xtxinv
  }

  out$logl <- -out$n * log(2 * out$sigma2 * pi)/2 - out$rss/(2 * out$sigma2)
return(out)
}
