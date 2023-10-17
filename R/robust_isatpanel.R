#' Get robust Standard Errors for the isatpanel result
#'
#' @param object An isatpanel object
#' @param robust Logical (TRUE or FALSE). Should the Standard Errors be robustified for Heterogeneity? This uses [plm::vcovHC] with the specified type (default is "HC0").
#' @param HAC Should Heteroscedasticity and Autocorrelation Robust Standard Errors be used? This uses [plm::vcovNW], which uses the Newey-West estimator.
#' @param lag Maximum Number of Lags to be used with [plm::vcovNW] using the Newey-West estimator. Cannot be specified when HAC = FALSE. Default is \code{NULL}.
#' @param type Character string. Type of Robust procedure e.g. 'HC0' for White SE or 'HC3' for Lang.
#' @param cluster Should an object with clustered S.E. be included? Choose between 'group' or 'time' or FALSE. Uses [plm::vcovHC] with the cluster argument.
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
#' robust_isatpanel(result)
#'}
#'
robust_isatpanel <- function(object,
                             robust = TRUE,
                             HAC = FALSE,
                             lag = NULL,
                             effect = "twoways",
                             type = "HC0",
                             cluster = "group"){

  # Checks
  if(cluster == TRUE){stop("Please specify the cluster setting i.e. either 'group' or 'time'. TRUE is not allowed. To disable clustering use FALSE.")}
  if(!is.null(lag)&!HAC){stop("You cannot specify a lag when you select HAC = FALSE.")}


  # Set-up
  df <- object$finaldata
  df$id <- as.factor(df$id)
  df$time <- as.factor(df$time)
  effect <- object$arguments$effect
  pdata <- plm::pdata.frame(df,index = c("id","time"))

  # parse FE
  parse_FE <- if (effect == "twoways") {
    "| id + time"
  } else if (effect == "time")  {
    " + time"
  } else if (effect == "individual") {
    " + id"
  } else if (effect == "none") {
    ""
  } else {
    stop("No Fixed Effect Specification was selected. Choose from 'twoways', 'time', 'individual', or 'none'")
  }

  # Let's look at the plm output
  formula <- as.formula(paste0("y ~ ",paste0(names(pdata[,!names(pdata) %in% c("y","id","time")]),collapse = " + ")))
  plm_object <- plm::plm(formula = formula, data = pdata, effect = effect, model = "within")


  out <- list()
  out$plm_object <- plm_object

  out$robust <- lmtest::coeftest(plm_object, vcov = plm::vcovHC(plm_object, type = type))

  if(cluster != FALSE){
    out$cluster <- lmtest::coeftest(plm_object, vcov = plm::vcovHC(plm_object, type = type, cluster = cluster))
  }

  if(HAC){
    out$HAC <- lmtest::coeftest(plm_object, vcov = plm::vcovNW(plm_object, type = type, maxlag = lag))
  }





  #summary(plm_object)
  # This should be exactly the same output as the one from isatpanel


  # if(robust){
  #   if(docluster){
  #     out$robust <- lmtest::coeftest(plm_object, vcov=sandwich::vcovHC(plm_object,type=type,cluster=cluster))
  #   }else{
  #     out$robust <- lmtest::coeftest(plm_object, vcov=sandwich::vcovHC(plm_object,type=type))
  #   }
  # }
  #
  # if(HAC){
  #
  # }

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
