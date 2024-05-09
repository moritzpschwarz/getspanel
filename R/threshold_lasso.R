#' Run a threshold LASSO based on targeting a number of breaks
#'
#' @param lasso_obj An isatpanel object that was run with engine = 'lasso'
#' @param breaks_in_ols_or_lasso Whether to target the number of breaks in the glmnet (LASSO) object or in the OLS object. The ols object often contains fewer breaks due to collinearity.
#' @param target_neg_breaks Integer. Number of negative breaks to target. You can only choose either target_neg_breaks or target_overall_breaks but not both.
#' @param target_overall_breaks Integer. Number of breaks to target. You can only choose either target_neg_breaks or target_overall_breaks but not both.
#' @param plot Logical. Default is TRUE.
#'
#' @return A list
#' @export
#'
#'
threshold_lasso <- function(lasso_obj, breaks_in_ols_or_lasso = "ols", target_neg_breaks, target_overall_breaks = NULL, plot = TRUE){

  if(!is.null(target_neg_breaks) & !is.null(target_overall_breaks)){stop("Can only either use neg_breaks or overall_breaks. One of them must be NULL, the other must be numeric.")}

  lass_coef_collection_lasso <- tibble()
  lass_coef_collection_ols <- tibble()
  if(is.null(lasso_obj$lasso_output$secondstage)){
    pot_lambdas <- lasso_obj$lasso_output$firststage$lambda
    glmnet_obj <- lasso_obj$lasso_output$firststage
  } else {
    pot_lambdas <- lasso_obj$lasso_output$secondstage$lambda
    glmnet_obj <- lasso_obj$lasso_output$secondstage
  }

  for(i in pot_lambdas){
    # i = pot_lambdas[1]
    orig_lass_coef <- coef(glmnet_obj, s = i)
    lass_names <- row.names(coef(glmnet_obj, s = i))[-1] # -1 to remove intercept
    lass_coef <- as.numeric(orig_lass_coef)[-1] # -1 to remove intercept
    lass_vars <- length(lass_coef[lass_coef != 0])

    # LASSO ---------
    neg_breaks_lasso <- tibble(name = lass_names,
                               coef = lass_coef) %>%
      filter(!name %in% c("Intercept",names(lasso_obj$estimateddata))) %>%
      filter(coef < 0) %>%
      nrow

    pos_breaks_lasso <- tibble(name = lass_names,
                               coef = lass_coef) %>%
      filter(!name %in% c("Intercept",names(lasso_obj$estimateddata))) %>%
      filter(coef > 0) %>%
      nrow

    tibble(lambda = i,
           lass_vars = lass_vars,
           neg_breaks_lasso = neg_breaks_lasso,
           pos_breaks_lasso = pos_breaks_lasso,
           total_breaks_lasso = pos_breaks_lasso+neg_breaks_lasso) %>%
      bind_rows(lass_coef_collection_lasso, .) -> lass_coef_collection_lasso

    # OLS -------
    xregs <- lasso_obj$estimateddata[,!names(lasso_obj$estimateddata) %in% c("id","time", "y")]
    xregs_ready <- cbind(xregs, lasso_obj$indicator_matrix$fesis)[,lass_coef!=0]

    suppressWarnings(
      arx_obj <- arx(y = lasso_obj$estimateddata$y, mxreg = xregs_ready, mc = FALSE)
    )


    neg_breaks_ols <- tibble(name = names(coef(arx_obj)),
                             coef = as.numeric(coef(arx_obj))) %>%
      filter(!name %in% c("mconst", "Intercept",names(lasso_obj$estimateddata))) %>%
      filter(coef < 0) %>%
      nrow

    pos_breaks_ols <- tibble(name = names(coef(arx_obj)),
                             coef = as.numeric(coef(arx_obj))) %>%
      filter(!name %in% c("mconst", "Intercept",names(lasso_obj$estimateddata))) %>%
      filter(coef > 0) %>%
      nrow


    tibble(lambda = i,
           ols_vars = length(coef(arx_obj)),
           neg_breaks_ols = neg_breaks_ols,
           pos_breaks_ols = pos_breaks_ols,
           total_breaks_ols = pos_breaks_ols+neg_breaks_ols) %>%
      bind_rows(lass_coef_collection_ols, .) -> lass_coef_collection_ols

  }

  if(breaks_in_ols_or_lasso == "lasso"){
    if(!is.null(target_neg_breaks)){
      lass_coef_collection_lasso %>%
        mutate(diff_to_gets = abs(neg_breaks_lasso - target_neg_breaks)) %>%
        filter(diff_to_gets == min(diff_to_gets)) %>%
        filter(lambda == min(lambda)) %>%
        pull(lambda) -> lambda_to_use
    } else if(!is.null(target_overall_breaks)){
      lass_coef_collection_lasso %>%
        mutate(diff_to_gets = abs(total_breaks_lasso - target_overall_breaks)) %>%
        filter(diff_to_gets == min(diff_to_gets)) %>%
        filter(lambda == min(lambda)) %>%
        pull(lambda) -> lambda_to_use
    }

  } else if(breaks_in_ols_or_lasso == "ols"){
    if(!is.null(target_neg_breaks)){
      lass_coef_collection_ols %>%
        mutate(diff_to_gets = abs(neg_breaks_ols - target_neg_breaks)) %>%
        filter(diff_to_gets == min(diff_to_gets)) %>%
        filter(lambda == min(lambda)) %>%
        pull(lambda) -> lambda_to_use
    } else if(!is.null(target_overall_breaks)){
      lass_coef_collection_ols %>%
        mutate(diff_to_gets = abs(total_breaks_ols - target_overall_breaks)) %>%
        filter(diff_to_gets == min(diff_to_gets)) %>%
        filter(lambda == min(lambda)) %>%
        pull(lambda) -> lambda_to_use
    }
  }

  coef_to_use <- coef(glmnet_obj, s = lambda_to_use)
  lasso_ret_names <- row.names(coef_to_use)[as.numeric(coef_to_use) != 0]

  lasso_obj$estimateddata %>%
    bind_cols(lasso_obj$indicator_matrix$fesis) %>%
    as_tibble() %>%
    select(all_of(lasso_ret_names)) -> dat_for_new_estimate

  new_lasso_obj <- lasso_obj
  new_lasso_obj$isatpanel.result <- suppressWarnings(arx(y = lasso_obj$inputdata$y, mxreg = dat_for_new_estimate))
  new_lasso_obj$isatpanel.result$aux$mX <- dat_for_new_estimate[,colnames(dat_for_new_estimate) %in% new_lasso_obj$isatpanel.result$aux$mXnames]


  if(plot){
    plot_df <-lass_coef_collection_lasso %>%
      rename(`Negative Breaks (in LASSO obj)` = neg_breaks_lasso,
             `Positive Breaks (in LASSO obj)` = pos_breaks_lasso,
             `Total Breaks (in LASSO obj)` = total_breaks_lasso,
             `Total Estimated Variables` = lass_vars) %>%
      bind_cols(lass_coef_collection_ols %>%
                  select(neg_breaks_ols, pos_breaks_ols, total_breaks_ols)) %>%
      rename(`Negative Breaks (in OLS obj)` = neg_breaks_ols,
             `Positive Breaks (in OLS obj)` = pos_breaks_ols,
             `Total Breaks (in OLS obj)` = total_breaks_ols)
    plot_df %>%
      pivot_longer(-lambda) %>%
      ggplot(aes(x = lambda, y = value, color = name)) +
      geom_hline(aes(yintercept = 0)) +
      geom_vline(aes(xintercept = lambda_to_use)) +
      geom_line() +
      theme_minimal() +
      labs(x = "Lambda", y = "Number of variables", title = "Threshold LASSO based on break number")+
      scale_color_discrete(name = "Data Series") -> p1

    #plot(new_lasso_obj) -> p2
    plot_grid(new_lasso_obj) -> p3

    print(cowplot::plot_grid(p1,
                             #p2,
                             p3, ncol = 1))

  }

  out <- list()
  out$lasso_obj <- lasso_obj
  out$new_lasso_obj <- new_lasso_obj
  out$lambda <- lambda_to_use
  return(out)
}
