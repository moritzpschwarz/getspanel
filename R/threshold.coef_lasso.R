#' Run a threshold LASSO based on targeting minimum effect (coefficient) size
#'
#' @param lasso_obj
#' @param min_coef Numeric.
#' @param scale_min_coef Numeric. Scales the minimum coefficient, therefore recommended between 0 and 1.
#' @param target_neg_breaks
#' @param target_overall_breaks
#' @param plot
#'
#' @return
#' @export
#'
#' @examples
#'
threshold.coef_lasso <- function(lasso_obj, min_coef, scale_min_coef = 1, absolute = FALSE, direction = "negative", plot = TRUE){

  if(!is.numeric(scale_min_coef)){}
  if(!direction %in% c("negative", "positive")){stop("Argument 'direction' can either be 'positive' or 'negative'. If 'absolute' is TRUE, this will be ignored.")}

  if(!is.null(lasso_obj$lasso_output$secondstage)){
    glmnet_obj <- lasso_obj$lasso_output$firststage
    lambda <- lasso_obj$lasso_output$firststage.lambda
  } else {
    glmnet_obj <- lasso_obj$lasso_output$secondstage
    lambda <- lasso_obj$lasso_output$secondstage.lambda
  }

  orig_lass_coef <- coef(glmnet_obj, s = lambda)
  lass_names <- row.names(orig_lass_coef)[-1] # -1 to remove intercept
  lass_coef <- as.numeric(orig_lass_coef)[-1] # -1 to remove intercept
  lass_vars <- length(lass_coef[lass_coef != 0])

  xregs <- lasso_obj$estimateddata[,!names(lasso_obj$estimateddata) %in% c("id","time","y")]
  xregs_ready <- cbind(xregs, lasso_obj$indicator_matrix$fesis)[,lass_coef!=0]

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

  xregs_final <- cbind(xregs, lasso_obj$indicator_matrix$fesis)[,final_vars]

  new_lasso_obj <- lasso_obj
  new_lasso_obj$isatpanel.result <- suppressWarnings(arx_obj_final <- arx(y = lasso_obj$estimateddata$y, mxreg = xregs_final, mc = FALSE))
  new_lasso_obj$isatpanel.result$aux$mX <- xregs_final[,colnames(xregs_final) %in% new_lasso_obj$isatpanel.result$aux$mXnames]


  # if(plot){
  #   plot_df <-lass_coef_collection_lasso %>%
  #     rename(`Negative Breaks (in LASSO obj)` = neg_breaks_lasso,
  #            `Positive Breaks (in LASSO obj)` = pos_breaks_lasso,
  #            `Total Breaks (in LASSO obj)` = total_breaks_lasso,
  #            `Total Estimated Variables` = lass_vars) %>%
  #     bind_cols(lass_coef_collection_ols %>%
  #                 select(neg_breaks_ols, pos_breaks_ols, total_breaks_ols)) %>%
  #     rename(`Negative Breaks (in OLS obj)` = neg_breaks_ols,
  #            `Positive Breaks (in OLS obj)` = pos_breaks_ols,
  #            `Total Breaks (in OLS obj)` = total_breaks_ols)
  #   plot_df %>%
  #     pivot_longer(-lambda) %>%
  #     ggplot(aes(x = lambda, y = value, color = name)) +
  #     geom_hline(aes(yintercept = 0)) +
  #     geom_vline(aes(xintercept = lambda_to_use)) +
  #     geom_line() +
  #     theme_minimal() +
  #     labs(x = "Lambda", y = "Number of variables", title = "Threshold LASSO based on break number")+
  #     scale_color_discrete(name = "Data Series") -> p1
  #
  #   #plot(new_lasso_obj) -> p2
  #   plot_grid(new_lasso_obj) -> p3
  #
  #   print(cowplot::plot_grid(p1,
  #                            #p2,
  #                            p3, ncol = 1))
  #
  # }

  out <- list()
  out$lasso_obj <- lasso_obj
  out$new_lasso_obj <- new_lasso_obj
  out$lambda <- lambda
  return(out)
}
