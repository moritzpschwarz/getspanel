library(fixest)
library(gets)

load("y.RData")
load("mxreg.RData")
load("user.estimator.RData") # ignore warning

# define user estimator function
fixestFun <- function (y, x, effect, time, id, cluster = "individual", ...) {
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
    # in arx, all column names of a matrix are removed - here we use a generic x_ because we only use the coefficient positions anyway
    if(is.null(colnames(x)) && dim(x)[2]>1){
      x <- as.matrix(x)
      colnames(x) <- paste0("x_",1:dim(x)[2])}
    # in case there is only one regressor left, R converts it to a nameless matrix - so colnames would be NULL
    if(is.null(colnames(x)) && dim(x)[2]==1){
      x <- as.matrix(x)
      colnames(x) <- "x"}

    est_df <- data.frame(y,individual = id,time)
    est_df <- cbind(est_df,x)

    # fixest always uses the first FE to cluster the standard errors.
    # When twoways, we need to arrange them in the right order to match
    # the cluster argument
    parse_FE <- if(effect == "twoways") {
      if (cluster == "individual") {
        "| individual + time"
      } else {
        "| time + individual"
      }
    } else if (effect == "time") {
      "| time"
    } else if (effect == "individual") {
      "| individual"
    } else if (effect == "none") {
      ""
    } else {
      stop("No Fixed Effect Specification was selected. Choose from 'twoways', 'time', 'individual', or 'none'")
    }

    if(!cluster %in% c("individual","time", "none")){
      stop("Please only use 'none', 'individual' or 'time' for the cluster variable. Other specifications have not yet been implmented.")
    }

    parsed_formula <- as.formula(paste0("y ~ ",paste0(colnames(x),collapse = " + "),parse_FE))

    tmp <- fixest::feols(fml = parsed_formula,data = est_df, notes=FALSE)

    out$coefficients <- coef(tmp)
    out$vcov <- if (cluster == "none" || is.null(cluster) || cluster == "0") {
      vcov(tmp, se = "standard")
    } else if (cluster == "twoway" || cluster == "twoways") {
      vcov(tmp,
           se = "twoway",
           cluster = list(est_df$time, est_df$individual))
    } else {
      vcov(tmp, se = "cluster", cluster = est_df[, cluster])
    }
    out$fit <- tmp$fitted.values
    out$logl <- logLik(tmp)

  }
  else {
    out$fit <- rep(0, out$n)
  }
  out$residuals <- y - out$fit
  out$residuals2 <- out$residuals^2
  out$rss <- sum(out$residuals2)
  out$sigma2 <- out$rss/out$df

  # # Adapt mXnames in arx when fixest removes collinearity
  # This is what I had tried using the <<- operator, but this didn't work either
  browser()
  if(ncol(x) > length(out$coefficients) & exists("aux", envir = -3)){
    browser()
    aux$mXnames <<- names(out$coefficients)
  }

  return(out)
}

arx(y = y, mc = FALSE, ar = NULL, mxreg = mxreg, user.estimator = user.estimator)
