#' Plot the Counterfactual Path
#'
#' @param x An object produced by the isatpanel function
#' @param facet.scales To be passed to ggplot2::facet_wrap. Default is "free" (i.e. a separate y axis for each panel group/id). Alternatives are: "fixed", "fixed_y", and "fixed_x".
#' @param plus_t Number of time periods for the counterfactual to be displayed (default = 5).
#'
#' @return
#' @export
#'
#' @importFrom ggplot2 geom_ribbon guides
#'
plot_counterfactual <- function(x, facet.scales = "free", plus_t = 5){

  df <- x$estimateddata
  indicators <- x$isatpanel.result$aux$mX
  indicators <- indicators[,!colnames(indicators) %in% names(df)]
  df <- cbind(df,indicators)

  if(is.null(x$isatpanel.result$fit)){
    fitted <- as.numeric(x$isatpanel.result$mean.fit)
  } else {
    fitted <- as.numeric(x$isatpanel.result$fit)
  }

  #
  # df_identified <- identify_indicator_timings(df)
  #
  # df_identified_coef <- merge(df_identified$fesis,
  #                             data.frame(name = names(coef(x$isatpanel.result)),
  #                                        coef = coef(x$isatpanel.result),
  #                                        sd = sqrt(diag(vcov(x$isatpanel.result)))), by = "name")

  max_times <- aggregate(x$estimateddata$time,by = list(x$estimateddata$id),FUN = function(x){max(x, na.rm = TRUE)})
  names(max_times) <- c("id","maxtime")

  df_ident <- break_uncertainty(x)
  df_ident <- merge(df_ident, max_times, by = "id")
  df_ident$origtime <- df_ident$time
  df_ident_overall <- df_ident
  for(i in 1:plus_t){
    intermed <- df_ident
    intermed$time <- intermed$time + i
    intermed$time <- ifelse(intermed$time > intermed$maxtime, intermed$maxtime, intermed$time)
    df_ident_overall <- rbind(df_ident_overall, intermed)
  }
  df_ident_overall <- df_ident_overall[order(df_ident_overall$name, df_ident_overall$time),]
  df_ident_overall <- df_ident_overall[!duplicated(df_ident_overall),]


  effects <- merge(x$estimateddata, df_ident_overall, by = c("id","time"), all.x = TRUE)
  effects$fitted <- fitted
  effects$cf <-  (effects$coef * (-1)) +  effects$fitted
  effects$cf_upr <-   ((effects$coef + (1.96*effects$sd)) * (-1)) +  effects$fitted
  effects$cf_lwr <-   ((effects$coef - (1.96*effects$sd)) * (-1)) +  effects$fitted
  effects$cf_upr99 <- ((effects$coef + (2.57*effects$sd)) * (-1)) +  effects$fitted
  effects$cf_lwr99 <- ((effects$coef - (2.57*effects$sd)) * (-1)) +  effects$fitted


  # as.list(df_ident[,c("id","time")])
  # df_ident$endtime <- df_ident$time + plus_t
  # df_ident$time:df_ident$endtime
  #
  #
  # max_times <- aggregate(x$estimateddata$time,by = list(x$estimateddata$id),FUN = function(x){max(x, na.rm = TRUE)})
  # names(max_times) <- c("id","maxtime")
  #
  # max_times_breaks <- merge(df_ident[, c("id", "time")], max_times, by = c("id"))
  # max_times_breaks$end <- max_times_breaks$time + plus_t
  # max_times_breaks$end <- ifelse(max_times_breaks$end > max_times_breaks$maxtime, max_times_breaks$maxtime, max_times_breaks$end)
  #
  # #plot_df <- merge(x$estimateddata, max_times, by = "id")
  # plot_df <- merge(x$estimateddata, max_times_breaks[,c("id","end")], by = "id")
  # plot_df_coef <- merge(plot_df, df_ident[,names(df_ident) %in% c("id","time","coef")], by = c("id","time"), all.x = TRUE)
  #
  #

  sub_title <- NULL

  ggplot(df, aes_(
    x = ~time,
    y = ~fitted,
    group = ~id
  )) -> g

  # fesis
  #if(!is.null(df_identified$fesis)){
  g = g + geom_vline(data = identify_indicator_timings(df)$fesis, aes_(xintercept = ~time,color="red"))
  #}

  # ggplot(effects, aes(x = time)) +
  #   geom_line(aes(y = y)) +
  #   geom_line(aes(y = fitted), color = "blue") +
  #   ggplot2::geom_ribbon(aes(ymin = cf_lwr, ymax = cf_upr), fill = "red", alpha = 0.5)+
  #   geom_line(aes(y = cf), color = "red") +
  #   # geom_line(aes(y = cf_upr), color = "red") +
  #   # geom_line(aes(y = cf_lwr), color = "red") +
  #
  #   facet_wrap(~id)

  g +
    geom_line(aes_(y = ~y, color = "black"), size = 0.7) +
    geom_line(aes(color = "blue"),linetype = 1, size = 0.5) +
    geom_hline(aes(yintercept = 0)) +

    geom_ribbon(data = effects, aes_(ymin = ~cf_lwr, ymax = ~cf_upr, fill = "red"), alpha = 0.5, na.rm = FALSE) +
    geom_line(data = effects, aes_(y = ~cf, color = "red", group = 1), na.rm = TRUE) +

    # Faceting
    facet_wrap("id", scales = facet.scales) +

    scale_color_identity(name = NULL,
                         breaks = c("black", "blue", "grey", "purple", "red","darkgreen", "orange"),
                         labels = c("y","Fitted","IIS","SIS","FESIS","CFESIS", "CSIS"),
                         guide = "legend") +

    scale_linetype(name = "Variable") +
    guides(fill = "none") +

    theme(#legend.position = "none",
      strip.background = element_blank(),
      legend.key = element_rect(fill = NA),
      panel.border = element_rect(colour = "grey",fill = NA),
      panel.background = element_blank()#,
      #panel.grid.major.y = element_line(colour = "grey",size = 0.1)
    ) +

    labs(title = title, subtitle = sub_title, y = NULL, x = NULL) -> plotoutput

  return(plotoutput)

}
