#' Run a threshold LASSO based on targeting minimum effect (coefficient) size
#'
#' @param lasso_obj An isatpanel object that was run with engine = 'lasso'
#' @param min_coef Numeric.
#' @param scale_min_coef Numeric. Scales the minimum coefficient, therefore recommended between 0 and 1.
#' @param direction Must be either 'negative' or 'positive'. This determines whether the min_coef is the upper limit (then 'negative' e.g. when min_coef is negative and we want to find coefficients that are even more negative) or the lower limit (then 'positive'). When absolute = TRUE will be ignored.
#'
#' @return A list
#' @export
#'
#'
threshold.coef_lasso <- function(lasso_obj, min_coef, scale_min_coef = 1, absolute = FALSE, direction = "negative"){

  if(!is(lasso_obj, "isatpanel")){stop("Argument 'lasso_obj' must be an isatpanel object.")}
  if(!is.numeric(min_coef)){stop("Argument 'min_coef' must be numeric.")}
  if(!is.numeric(scale_min_coef)){}
  if(!direction %in% c("negative", "positive")){stop("Argument 'direction' can either be 'positive' or 'negative'. If 'absolute' is TRUE, this will be ignored.")}

  if(is.null(lasso_obj$lasso_output$secondstage)){
    glmnet_obj <- lasso_obj$lasso_output$firststage
    lambda <- lasso_obj$lasso_output$firststage.lambda
  } else {
    glmnet_obj <- lasso_obj$lasso_output$secondstage
    lambda <- lasso_obj$lasso_output$secondstage.lambda
  }

  orig_lass_coef <- coef(glmnet_obj, s = lambda)
  lass_names <- row.names(orig_lass_coef)[-1] # -1 to remove intercept
  lass_coef <- as.numeric(orig_lass_coef)[-1] # -1 to remove intercept
  names(lass_coef) <- lass_names
  lass_vars <- length(lass_coef[lass_coef != 0])

  xregs <- lasso_obj$estimateddata[,!names(lasso_obj$estimateddata) %in% c("id","time","y")]

  if(any(grepl("^iis", lass_names))){
    xregs_ready <- cbind(xregs, lasso_obj$indicator_matrix$fesis, lasso_obj$indicator_matrix$iis)[,lass_coef!=0]
    } else {
    xregs_ready <- cbind(xregs, lasso_obj$indicator_matrix$fesis)[,lass_coef!=0]
  }


  suppressWarnings(
    arx_obj <- arx(y = lasso_obj$estimateddata$y, mxreg = xregs_ready, mc = FALSE)
  )

  if(absolute){
    retain_vars <- names(coef(arx_obj)[abs(coef(arx_obj)) > abs(min_coef)*scale_min_coef])
  } else if(direction == "negative"){
    retain_vars <- names(coef(arx_obj)[coef(arx_obj) < min_coef*scale_min_coef])
  } else if(direction == "positive"){
    retain_vars <- names(coef(arx_obj)[coef(arx_obj) > min_coef*scale_min_coef])
  }

  final_vars <- unique(c(names(xregs), retain_vars))

  xregs_final <- xregs_ready[,names(xregs_ready) %in% final_vars]

  new_lasso_obj <- lasso_obj
  new_lasso_obj$isatpanel.result <- suppressWarnings(arx_obj_final <- arx(y = lasso_obj$estimateddata$y, mxreg = xregs_final, mc = FALSE))
  new_lasso_obj$isatpanel.result$aux$mX <- xregs_final[,colnames(xregs_final) %in% new_lasso_obj$isatpanel.result$aux$mXnames]

  out <- list()
  out$lasso_obj <- lasso_obj
  out$new_lasso_obj <- new_lasso_obj
  out$lambda <- lambda
  return(out)
}
