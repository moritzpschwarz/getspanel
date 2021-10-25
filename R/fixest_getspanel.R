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
#' @return List to be used by gets::isat
#' @importFrom stats as.formula coef complete.cases dnorm lm logLik model.matrix model.response na.omit time var vcov reshape fitted.values deviance



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

    # fixest always uses the first FE to cluster the standard errors. When twoways, we need to arrange them in the right order to match
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


    # Check if the cluster variable is a fixed effect
    #if((!cluster=="twoway" ||!cluster=="twoways") && !grepl(cluster,parse_FE)){
    #  stop("The cluster variable is not selected as a Fixed Effect. This is currently not recommended.")
    #}
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
  if(ncol(x) > length(out$coefficients)){
    original <- data.frame(term = colnames(x),
                           index = 1:length(colnames(x)), row.names = NULL)
    outcome <- data.frame(term = names(coef(tmp)),
                          coef = coef(tmp),
                          row.names = NULL)
    merged <- merge(original, outcome, by = "term", all = TRUE, sort = FALSE)
    index_kept <- order(merged$index)
    merged <- merged[index_kept,]

    out$coefficients <- merged$coef

    vcov_mat_orig <- matrix(nrow = length(colnames(x)), ncol = length(colnames(x)), NA,
           dimnames = list(colnames(x), colnames(x)))

    vcov_mat_orig[rownames(vcov_mat_orig) %in% rownames(out$vcov),
                  colnames(vcov_mat_orig) %in% colnames(out$vcov)] <-
      out$vcov[rownames(out$vcov) %in% rownames(vcov_mat_orig),
               colnames(out$vcov) %in% colnames(vcov_mat_orig)]

    out$vcov <- vcov_mat_orig



    # test <- structure(rep(NA,ncol(x)), names=colnames(x))
    # out$coefficients[names(out$coefficients) %in% names(test)]
    #
    #
    #
    # aux_out <- dynGet("aux")
    #
    # aux_out$mXnames <- names(out)
    # for(i in 1:sys.nframe()){
    #   if(any(names(sys.frame(i)) %in% "aux")){right_frame <- i}
    # }
    # #browser()
    # right_pos <- (sys.nframe() - right_frame) #* -1
    # #assign("aux",aux_out,pos = right_pos)
    # assign("aux",aux_out,pos = 2)
    #

    #aux$mXnames <<- names(out$coefficients)
  }
  return(out)
}
