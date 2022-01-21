#' Plot Residuals from 'isatpanel' against OLS
#'
#' @param isatpanelobject An output from the 'isatpanel' function
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' data <- pandata_simulated
#' outcome <- isatpanel(data = data, gdp ~ temp, index = c("country","year"),
#' effect="twoways",iis=FALSE,fesis=TRUE,t.pval=0.01,engine = "fixest")
#' plot_residuals.isatpanel(outcome)
#'}
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs theme element_blank element_rect element_line geom_hline
#'
plot_residuals.isatpanel <- function(isatpanelobject){
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
    aes_string(x = "time", group = "id", y = "diff") +
    facet_wrap("id") +
    geom_line() +
    geom_hline(aes(yintercept = 0))+
    labs(x = NULL, y = "Residual Difference", title = "Difference in Residuals between OLS and isatpanel object") +
    theme(panel.background = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(size = 0.5, colour = "black", fill =  NA),
          panel.grid.major.y = element_line(colour = "grey", linetype = 2))

}
