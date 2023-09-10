#' Extract the retained indicators from an `isatpanel` object
#'
#' @param object An object produced by the isatpanel function.
#' @param uis_breaks A string with the names of user-specified indicators.
#'
#' @return A list of indicators.
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
#'
#' # print the retained indicators
#' get_indicators(result)
#' }
get_indicators <- function(object, uis_breaks = NULL){

  df <- object$estimateddata
  indicators <- object$isatpanel.result$aux$mX
  indicators <- indicators[,!colnames(indicators) %in% names(df), drop = FALSE]
  df <- cbind(df,indicators)

  if(!is.null(object$arguments$uis)){
    uis_breaks <- colnames(object$arguments$uis)
  } else {
    uis_breaks <- NULL
  }

  identify_indicator_timings(df, uis_breaks = uis_breaks)
}

