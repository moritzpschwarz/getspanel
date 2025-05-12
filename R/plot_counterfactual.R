#' Plot the Counterfactual Path
#'
#' @param x An object produced by the isatpanel function
#' @param plus_t Number of time periods for the counterfactual to be displayed (default = 5).
#' @param facet.scales To be passed to ggplot2::facet_wrap. Default is "free" (i.e. a separate y axis for each panel group/id). Alternatives are: "fixed", "fixed_y", and "fixed_x".
#' @param title Plot title. Must be a character vector.
#' @param zero_line Plot a horizontal line at y = 0. Default is FALSE.
#' @param regex_exclude_indicators A regex character vector to exclude the inclusion of certain indicators in the plot. Default = NULL. Use with care, experimental.
#'
#' @return A ggplot2 plot that plots an 'isatpanel' object and shows the counterfactuals for each break.
#' @export
#'
#' @importFrom ggplot2 geom_ribbon guides geom_rect
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
#' plot_counterfactual(result)
#'}

plot_counterfactual <- function(x, plus_t = 5, facet.scales = "free", title = NULL, zero_line = FALSE, regex_exclude_indicators = NULL){

  df <- x$estimateddata
  indicators <- x$isatpanel.result$aux$mX
  indicators <- indicators[,!colnames(indicators) %in% names(df)]

  if(!is.null(regex_exclude_indicators)){
    indicators <- indicators[,!grepl(regex_exclude_indicators,colnames(indicators)),drop = FALSE]
  }

  df <- cbind(df,indicators)

  df_ident_fesis <- identify_indicator_timings(df)$fesis

  if(is.null(x$isatpanel.result$fit)){
    fitted <- as.numeric(x$isatpanel.result$mean.fit)
  } else {
    fitted <- as.numeric(x$isatpanel.result$fit)
  }

  max_times <- aggregate(x$estimateddata$time,by = list(x$estimateddata$id),FUN = function(x){max(x, na.rm = TRUE)})
  names(max_times) <- c("id","maxtime")

  df_ident <- break_uncertainty(x)
  df_ident <- merge(df_ident, max_times, by = "id")
  df_ident$origtime <- df_ident$time

  # make sure the preceding observation collapses on the last observation
  df_ident_start <- df_ident
  df_ident_start$time <- df_ident_start$time - 1
  df_ident_start$coef <- 0
  df_ident_start$sd <- 0
  df_ident_start$tci <- NA

  df_ident_overall <- rbind(df_ident_start, df_ident)
  for(i in 1:plus_t){
    intermed <- df_ident
    intermed$time <- intermed$time + i
    intermed$time <- ifelse(intermed$time > intermed$maxtime, intermed$maxtime, intermed$time)
    df_ident_overall <- rbind(df_ident_overall, intermed)
  }
  df_ident_overall <- df_ident_overall[order(df_ident_overall$name, df_ident_overall$time),]
  df_ident_overall <- df_ident_overall[!duplicated(df_ident_overall),]


  effects <- merge(x$estimateddata, df_ident_overall, by = c("id","time"), all.x = TRUE)
  # time-id pairs can also be duplicated if there are multiple rows at a time
  # this is why effects can have more rows than the original data (and we need to use merge in the row below)
  effects <- merge(effects,data.frame(x$estimateddata[,c("id","time")], fitted), by = c("id","time"))

  effects$cf <-  (effects$coef * (-1)) +  effects$fitted
  effects$cf_upr <- ((effects$coef + (1.96 * effects$sd)) * (-1)) +  effects$fitted
  effects$cf_lwr <- ((effects$coef - (1.96 * effects$sd)) * (-1)) +  effects$fitted
  effects$cf_upr99 <- ((effects$coef + (2.57 * effects$sd)) * (-1)) +  effects$fitted
  effects$cf_lwr99 <- ((effects$coef - (2.57 * effects$sd)) * (-1)) +  effects$fitted

  effects$start_rect <- effects$origtime - effects$tci
  effects$end_rect <- effects$origtime + effects$tci

  effects$cf_upr[is.na(effects$cf_upr)] <- effects$fitted[is.na(effects$cf_upr)]
  effects$cf_lwr[is.na(effects$cf_lwr)] <- effects$fitted[is.na(effects$cf_lwr)]

  sub_title <- NULL


  ggplot(df, aes(
    x = .data$time,
    y = fitted,
    group = .data$id
  )) -> g



  if(zero_line){g = g + geom_hline(aes(yintercept = 0))}

  g +
    geom_line(aes(y = .data$y, color = "black"), size = 0.7) +

    geom_rect(data = effects, aes(xmin = .data$start_rect, xmax = .data$end_rect, ymin = -Inf, ymax = Inf, group = .data$name),fill = "grey",alpha = 0.1, na.rm = TRUE) +

    geom_line(aes(color = "blue"),linetype = 1, size = 0.5) +

    # fesis
    geom_vline(data = df_ident_fesis, aes(xintercept = .data$time,color="red")) +

    geom_ribbon(data = effects, aes(ymin = .data$cf_lwr, ymax = .data$cf_upr, fill = "red", group = .data$name), alpha = 0.5, na.rm = FALSE) +

    geom_line(data = effects, aes(y = .data$cf, color = "red", group = .data$name), na.rm = TRUE) +

    # Faceting
    facet_wrap("id", scales = facet.scales) +

    scale_color_identity(name = NULL,
                         breaks = c("black", "blue", "grey", "purple", "red","darkgreen", "orange"),
                         labels = c("y","Fitted","IIS","SIS","FESIS","CFESIS", "CSIS"),
                         guide = "legend") +

    scale_linetype(name = "Variable") +
    guides(fill = "none") +

    theme(
      strip.background = element_blank(),
      legend.key = element_rect(fill = NA),
      panel.border = element_rect(colour = "grey",fill = NA),
      panel.background = element_blank()#,
    ) +

    labs(title = title, subtitle = sub_title, y = NULL, x = NULL) -> plotoutput

  return(plotoutput)

}
