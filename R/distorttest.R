#' Distortion Test Function
#'
#' @param x An isat object
#' @param coef character string. Default is "all"
#'
#' @return
#' @export
#'
distorttest <- function(x, coef="all"){

  if (!is.null(x)){

    if (class(x)=="isat"){

      if (any(x$call$sis, x$call$tis, x$call$uis)==TRUE){
        stop("Test only valid for iis")
      } else {
        if (  x$call$iis == TRUE){
          ISnames <- c(x$ISnames[grep("iis", x$ISnames)])
          noutl = length(ISnames)
          t.pval <- x$aux$t.pval
          T <- x$n # MORITZ: Want to change this. Using T is not good practice - update: this is not used anywhere else (I think)

          alpha <-   t.pval
          c <- abs(qnorm(alpha/2))
          fc <- dnorm(c)
          psi <- pnorm(c) - pnorm(-c)
          tau <- psi - 2 * c * fc

          varsigmac = tau / psi
          rhoc1 = psi^2 / (4*(c^2)*(fc^2) + 4*tau*c*fc + tau)
          rhocinfinity = (psi - 2*c*fc)^2 / tau
          avar1 = ((2*c*fc - psi)^2 + 2*tau*(2*c*fc - psi) + tau) /psi^2
          avarinfinity = ((2*c*fc - psi)^2 + 2*tau*(2*c*fc - psi) + tau) / (2*c*fc - psi)^2


        } else {
          stop("iis must be TRUE")
        }
      }


    } else {
      stop("x must be an isat object")
    }
  } #x null closed



  x.date <- isatdates(x)$iis$date

  n.null <-  x$aux$y.index[!(x$aux$y.index %in% x.date)]

  keep <- which(!(names(coefficients(x)) %in%  x$ISnames))

  y.null <-  x$aux$y[n.null]


  if (!is.null(x$call$ar)){
    ar.call <- x$call$ar
  }

  if (!is.null(x$call$ar)){
    mx.null <- x$aux$mX[(n.null-max(x$call$ar)),keep]
  } else{
    mx.null <- x$aux$mX[n.null,keep]
  }


  y <-  x$aux$y
  mx <- x$aux$mX[,keep]

  nOLS <- length(x$aux$y)
  nOLS_rob <- length(x$aux$y) - length(x$ISnames)

  # This is the GUM
  ols.y <- arx(y, mxreg=mx, mc=FALSE, plot=FALSE, ar=FALSE)


  if (NROW(coef)==1){
    if (coef=="all"){ #if testing on all coefficients

        betaOLS1 <- coefficients(x)[keep]
        betaOLS <- coefficients(ols.y)

        V <- (nOLS_rob/nOLS) * (rhoc1)^(-1)*(varsigmac)^(-1) * x$vcov.mean[keep, keep]

    } else { #testing on subset of coefficients

        betaOLS1 <- coefficients(x)[coef]
        betaOLS <- coefficients(ols.y)[coef]


        V <- (nOLS_rob/nOLS) * (rhoc1)^(-1)*(varsigmac)^(-1) * x$vcov.mean[coef, coef]

    }

  } else {

      betaOLS1 <- coefficients(x)[coef]
      betaOLS <- coefficients(ols.y)[coef]

      V <- (nOLS_rob/nOLS) * (rhoc1)^(-1)*(varsigmac)^(-1) * x$vcov.mean[coef, coef]

  }

    if (NROW(coef)==1){
      if ( coef=="all"){
        rel.df <- length(coef(x)[keep])  #degrees of freedom
      } else {
        rel.df <- length(coef)
      }

    } else {
      rel.df <- length(coef)
    }


    cf_diff <- (betaOLS1 - betaOLS) #difference between coefficients

    esigma2MinverseOLS1 = nOLS * rhoc1 * V
    eavarOLS10 = avar1 * esigma2MinverseOLS1


  if (any(!cf_diff==0)){ #if the difference between OLS and IIS coefficients is not zero

    HtestOLS10 = as.numeric(nOLS * t(cf_diff) %*% solve(eavarOLS10) %*% as.vector(cf_diff))

     } else {
       HtestOLS10 <- 0
  }

  p.test <- pchisq(HtestOLS10, df = rel.df, lower.tail = FALSE)
  rval_chi <- list(statistic = HtestOLS10, p.value = p.test, estimate=NULL, null.value = NULL, alternative = NULL, method="Jiao-Pretis Outlier Distortion Test", data.name="Difference between IIS and OLS Estimates", coef.diff = cf_diff, var.diff = eavarOLS10, iis=x, ols=ols.y)
  attr(rval_chi, "class") <- "htest"

  out <- return(rval_chi)


}
