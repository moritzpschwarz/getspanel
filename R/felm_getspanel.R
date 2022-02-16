#' Internal lfe/felm Estimation Method
#'
#' @param y dependent variable
#' @param x matrix of regressors
#' @param effect Fixed Effect specification
#' @param time Character vector of name of the time variable
#' @param id Character vector of the name of the group variable
#' @param cluster Character vector of the variable(s) to cluster Standard Errors at
#' @param ... Further arguments to pass to gets::isat
#'
#' @export
#' @return List to be used by gets::isat
#'

felmFun <- function (y, x, effect, time, id, cluster = "individual", ...) {
  #browser()
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
    # in arx, all column names of a matrix are removed
    # here we use a generic x_ because we only use the coefficient positions anyway
    if(is.null(colnames(x)) && dim(x)[2]>1){
      x <- as.matrix(x)
      colnames(x) <- paste0("x_",1:dim(x)[2])}
    # in case there is only one regressor left, R converts it to a nameless matrix
    # so colnames would be NULL
    if(is.null(colnames(x)) && dim(x)[2]==1){
      x <- as.matrix(x)
      colnames(x) <- "x"}

    est_df <- data.frame(y,x,individual = as.factor(id),time = as.factor(time))

    # fixest always uses the first FE to cluster the standard errors.
    # When twoways, we need to arrange them in the right order to match
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
      stop("No Fixed Effect Specification was selected.
           Choose from 'twoways', 'time', 'individual', or 'none'")
    }
    #browser()

    if(cluster=="none"){cluster <- "0"}

    if(!cluster %in% c("individual","time", "0")){
      stop("Please only use 'none', 'individual' or 'time' for the cluster variable.
           Other specifications have not yet been implmented.")
    }

    # Check if the cluster variable is a fixed effect
    if((!cluster %in% c("twoway","twoways","0","none")) && !grepl(cluster,parse_FE)){
      stop("The cluster variable is not selected as a Fixed Effect.
           This is currently not recommended.")
    }

    parsed_formula <- as.formula(paste0("y ~ ",paste0(colnames(x),collapse = " + "),
                                        ifelse(effect == "none","-1",""),
                                        "|",parse_FE,"| 0 |",cluster))

    suppressWarnings(tmp <- lfe::felm(formula = parsed_formula,data = est_df))

    parsed_formula <- as.formula(
      paste0("y ~ ",paste0(row.names(na.omit(tmp$coefficients)),collapse = " + "),
             ifelse(effect == "none","-1",""),
             "|",parse_FE,"| 0 |",cluster)
      )
    tmp <- lfe::felm(formula = parsed_formula,data = est_df)

    out$coefficients <- coef(tmp)
    out$vcov <- vcov(tmp)
    #out$vcov <- if(is.null(cluster)){vcov(tmp,se="standard")}
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
