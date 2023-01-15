#' Get robust Standard Errors for the isatpanel result
#'
#' @param object An isatpanel object
#' @param robust Logical (TRUE or FALSE). Should the Standard Errors be robustified for Heterogeneity?
#' @param HAC Should Heteroscedasticity and Autocorrelation Robust Standard Errors be used?
#' @param lag Number of Lags to be used with HAC in coeftest. Cannot be specified when HAC = FALSE.
#' @param effect The effects introduced into the plm model, one of "individual", "time", "twoways" (default), or "nested"
#' @param type Type of Robust procedure e.g. HC0 for White SE or HC3 for Lang
#' @param cluster 'group' or 'time' or FALSE
#'
#' @return A list with robust estimates
#' @export
#'
#' @examples
#' \donttest{
#'data(EU_emissions_road)
#'
#'# Group specification
#'EU15 <- c("Austria", "Germany", "Denmark", "Spain", "Finland", "Belgium",
#'          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg",
#'          "Netherlands", "Greece", "Portugal", "Sweden")
#'
#'# Prepare sample and data
#'EU_emissions_road_short <- EU_emissions_road[
#'EU_emissions_road$country %in% EU15 &
#' EU_emissions_road$year >= 2000,
#' ]
#'
#'# Run
#' result <- isatpanel(
#'   data = EU_emissions_road_short,
#'   formula = ltransport.emissions ~ lgdp + I(lgdp^2) + lpop,
#'   index = c("country", "year"),
#'   effect = "twoways",
#'   fesis = TRUE,
#'   plot = FALSE,
#'   t.pval = 0.01
#' )
#' plot(result)
#' plot_grid(result)
#' robust.isatpanel(result)
#'}
#'
robust.isatpanel <- function(object,
                             robust = TRUE,
                             HAC = FALSE,
                             lag = NULL,
                             effect = "twoways",
                             type = "HC0",
                             cluster = "group"){
  if(!is.logical(cluster)){docluster <- TRUE}
  if(cluster == TRUE){stop("Please specify the cluster setting i.e. either 'group' or 'time'. TRUE is not allowed. To disable clustering use FALSE. ")}

  if(!is.null(lag)&!HAC){stop("You cannot specify a lag when you select HAC = FALSE.")}
  out <- list()

  df <- object$finaldata
  pdata <- plm::pdata.frame(df,index = c("id","time"))

  # Let's look at the plm output
  formula <- as.formula(paste0("y ~ ",paste0(names(pdata[,!names(pdata) %in% c("y","id","time")]),collapse = " + ")))
  plm_object <- plm::plm(formula = formula,data = pdata, effect = effect,model = "within")

  out$plm_object <- plm_object

  #summary(plm_object)
  # This should be exactly the same output as the one from isatpanel


  if(robust){
    if(docluster){
      out$robust <- lmtest::coeftest(plm_object, vcov=sandwich::vcovHC(plm_object,type=type,cluster=cluster))
    }else{
      out$robust <- lmtest::coeftest(plm_object, vcov=sandwich::vcovHC(plm_object,type=type))
    }
  }


  if(HAC){
    lm_mod <- lm(y~.-1,df)
    out$HAC <- lmtest::coeftest(lm_mod, vcov=sandwich::vcovHAC(lm_mod, cluster=cluster, lag = lag))
  }

  #
  # # The following also for autocorrelation
  # # Heteroskedasticity- and autocorrelation-consistent (HAC)
  # coeftest(plm_object, vcovHC(plm_object, method = "arellano"))
  #
  # lm_mod <- lm(y~.,pdata)
  # coeftest(lm_mod, vcov=vcovHAC(lm_mod, cluster="individual"))
  #
  # # All of the below are unlikely to affect the S.E. significantly, but for completeness
  # coeftest(lm_mod, vcov=vcovHAC(lm_mod,prewhite = FALSE,cluster = "individual"))
  # coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1,cluster = "individual"))
  # coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1,cluster = "time"))
  #
  # coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 3,cluster = "individual"))
  # coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 3,cluster = "time"))
  #
  # coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1:3,cluster = "individual"))
  # coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1:3,cluster = "time"))


  #coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1:3,cluster = c("individual","time")))

  return(out)

}
