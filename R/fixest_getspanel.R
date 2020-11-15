#' fixest Estimation Method
#'
#' @param y dependent variable
#' @param x matrix of regressors
#' @param effect Fixed Effect specification
#' @param time Character vector of name of the time variable
#' @param id Character vector of the name of the group variable
#' @param cluster Character vector of the variable(s) to cluster Standard Errors at
#' @param ... Further arguments to pass to gets::isat
#'
#' @return
#'




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

    est_df <- data.frame(y,x,individual = id,time)

    # fixest always uses the first FE to cluster the standard errors. When twoways, we need to arrange them in the right order to match
    # the cluster argument
    parse_FE <- if(effect == "twoways") {
      if (cluster == "individual") {
        "individual + time"
      } else {
        "time + individual"
      }
    } else if (effect == "time") {
      "time"
    } else if (effect == "individual") {
      "id"
    } else if (effect == "none") {
      "0"
    } else {
      stop("No Fixed Effect Specification was selected. Choose from 'twoways', 'time', 'individual', or 'none'")
    }


    # Check if the cluster variable is a fixed effect
    #if((!cluster=="twoway" ||!cluster=="twoways") && !grepl(cluster,parse_FE)){
    #  stop("The cluster variable is not selected as a Fixed Effect. This is currently not recommended.")
    #}
    if(!cluster %in% c("individual","time", "none")){
      stop("Please only use 'none', 'individual' or 'time' for the cluster variable. Other specifications have not yet been implmented.")
    }

    parsed_formula <- as.formula(paste0("y ~ ",paste0(colnames(x),collapse = " + "),"|",parse_FE))

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

  return(out)
}
















#
#
#
#
#
# fixestFun <- function(y, mxreg, time, id, cluster, effect, ...){
#   require(fixest)
#
#   if(effect == "nested"){stop("Effect = nested is currently not possible with engine fixest. Use plm as engine for nested effect.")}
#   #if(!is.null(model)){stop("Model specification (e.g. pooling, within, between, random, fd, or ht) is currently not possible with engine fixest. Use plm as engine to use a model argument.")}
#
#   ##create list:
#   result <- list()
#   ##n, k and df:
#   result$n <- length(y)
#
#   if( is.null(mxreg) || NCOL(mxreg) == 0 ){
#     result$k <- 0
#   } else {
#     result$k <- NCOL(mxreg)
#   }
#   result$df <- result$n - result$k
#   ##call felm if k > 0:
#   if( result$k > 0 ){
#
#
#     if(is.null(colnames(mxreg))){
#       if(!is.matrix(mxreg)){
#         mxreg <- as.matrix(mxreg)
#         colnames(mxreg) <- paste0("x_felm_",1)
#       } else {
#         colnames(mxreg) <- paste0("x_felm_",1:ncol(mxreg))
#       }
#
#     }
#     yx <- data.frame(y,mxreg,time,individual = id)
#
#     parse_FE <- if(effect == "twoways"){
#       "time + individual"} else if(effect == "time"){
#         "time"} else if (effect == "individual"){
#           "individual"} else {
#             "0"}
#     parsed_formula <- as.formula(paste0("y ~ ",paste0(colnames(mxreg),collapse = " + "),"|",parse_FE))
#     tmp <- feols(fml = parsed_formula,data = yx,panel.id = c("time","individual"), notes=FALSE) #, se = yx[,cluster])
#     result$coefficients <- coef(tmp)
#     result$vcov <- vcov(tmp)
#     result$logl <- as.numeric(logLik(tmp))
#   } else {
#     result$coefficients <- NULL
#     result$vcov <- NULL
#     result$logl <- sum(dnorm(y, sd = sqrt(var(y)), log = TRUE))
#   }
#   ##return result:
#   return(result)
# }


