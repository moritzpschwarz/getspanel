#' plm Function to estimate isatpanel
#'
#' @param y Dependent Variable
#' @param x matrix or data.frame of regressors
#' @param time Vector of time variable
#' @param id Vector of group variable
#' @param cluster cluster specification
#' @param effect effect specification
#' @param model model specification
#' @param ... Further arguments passed to plm
#'
#' @return A list to be used by gets::isat
#'
#'
#'
plmFun <- function(y, x, time, id, cluster, effect, model = "pooling", ...){
  out <- list()
  ##n, k and df:
  out$n <- length(y)

  if( is.null(x) || NCOL(x) == 0 ){
    out$k <- 0
  } else {
    out$k <- NCOL(x)
  }
  out$df <- out$n - out$k
  if(out$k > 0){
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
    est_df <- plm::pdata.frame(est_df,index = c("individual","time"))

    parsed_formula <- as.formula(paste0("y ~ ",paste0(paste0("`",colnames(x),"`"),collapse = " + "),""))
    tmp <- plm::plm(formula = parsed_formula,data = est_df, effect = effect, model = model)

    #tmp_clustered <- coeftest(tmp, vcov=vcovHC(tmp,type="HC0",cluster=if(cluster=="individual"){"group"}else{cluster}))

    out$fit <- stats::fitted.values(tmp)
    out$coefficients <- coef(tmp)
    out$vcov <- if (cluster == "individual") {
      plm::vcovHC(tmp, type = "HC0", cluster = "group")
    } else if (cluster == "time") {
      plm::vcovHC(tmp, type = "HC0", cluster = "time")
    } else if (cluster == "none") {
      vcov(tmp)
    } else{
      stop("Using PLM with a cluster specification that is not 'time', 'individual' or 'none' is not possible")
    }
    out$logl <- as.numeric(logLik(tmp))
  } else {
    out$coefficients <- NULL
    out$vcov <- NULL
    out$logl <- sum(dnorm(y, sd = sqrt(var(y)), log = TRUE))
  }


  # # Adapt mXnames in arx when felm removes collinearity
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

  }

  ##return result:
  return(out)
}
