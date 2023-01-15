#' Plot Residuals from 'isatpanel' against OLS
#'
#' @param isatpanelobject An output from the 'isatpanel' function
#'
#' @return A ggplot2 plot that plots an 'isatpanel' object and shows the residuals over time in comparison to an OLS model.
#' @export
#'
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs theme element_blank element_rect element_line geom_hline
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
#' plot_residuals(result)
#'}

plot_residuals <- function(isatpanelobject){
  finaldata <- isatpanelobject$finaldata
  finaldata$id <- as.factor(finaldata$id)
  finaldata$time <- as.factor(finaldata$time)

  finaldata_noindicators <- finaldata[,!grepl("fesis|csis|cfesis|jiis|jsis", names(finaldata))]

  saturation <- lm(y ~ . , finaldata)
  counterfactual <- lm(y ~ . , finaldata_noindicators)

  residual_df <- data.frame(finaldata[,c("id","time")],
                            diff = saturation$residuals - counterfactual$residuals)

  residual_df$time <- as.numeric(residual_df$time)

  ggplot(residual_df) +
    aes_(x = ~time, group = ~id, y = ~diff) +
    facet_wrap("id") +
    geom_line() +
    geom_hline(aes(yintercept = 0))+
    labs(x = NULL, y = "Residual Difference", title = "Difference in Residuals between OLS and isatpanel object") +
    theme(panel.background = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(size = 0.5, colour = "black", fill =  NA),
          panel.grid.major.y = element_line(colour = "grey", linetype = 2))

}
