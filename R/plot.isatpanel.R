#' Plotting an isatpanel object
#'
#' @param x An object produced by the isatpanel function
#' @param max.id.facet The resulting plot will be faceted for each individual in the panel. Beyond a certain number, this might result in unreadable figures. Default set at 16.
#' @param facet.scales To be passed to ggplot2::facet_wrap. Default is "free" (i.e. a separate y axis for each panel group/id). Alternatives are: "fixed", "fixed_y", and "fixed_x".
#' @param title Plot title. Must be a character vector.
#' @param zero_line Plot a horizontal line at y = 0. Default is FALSE.
#' @param ... Further arguments to be passed to ggplot2.
#'
#' @return A ggplot2 plot that plots an 'isatpanel' object and shows observed data, the fitted values, and all identified breaks and impulses.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs theme element_blank element_rect element_line geom_hline geom_vline scale_color_identity scale_linetype
#'
plot.isatpanel <- function(x, max.id.facet = 16, facet.scales = "free", title = NULL, zero_line = FALSE, ...){

  #interactive = TRUE, currently not implemented. Roxygen: Logical (TRUE or FALSE). Default is TRUE. When True, plot will be passed to plotly using ggplotly.


  df <- x$estimateddata
  indicators <- x$isatpanel.result$aux$mX
  indicators <- indicators[,!colnames(indicators) %in% names(df), drop = FALSE]
  df <- cbind(df,indicators)

  if(is.null(x$isatpanel.result$fit)){
    fitted <- as.numeric(x$isatpanel.result$mean.fit)
  } else {
    fitted <- as.numeric(x$isatpanel.result$fit)
  }

  df_identified <- get_indicators(x)

  sub_title <- NULL

  ggplot(df, aes(
    x = .data$time,
    y = fitted,
    group = .data$id
  )) -> g

  # Impulses
  if(!is.null(df_identified$iis)){
    g = g + geom_vline(data = df_identified$iis,aes(xintercept = .data$time,color="grey"))
  }
  # Steps
  if(!is.null(df_identified$steps)){
    g = g + geom_vline(data = df_identified$steps, aes(xintercept = .data$time,color="purple"))
  }

  # uis
  if(!is.null(df_identified$uis_breaks)){
    g = g + geom_vline(data = df_identified$uis_breaks, aes(xintercept = .data$time, color="violetred4"))
  }

  # fesis
  if(!is.null(df_identified$fesis)){
    g = g + geom_vline(data = df_identified$fesis, aes(xintercept = .data$time,color="red"))
  }

  # tis
  if(!is.null(df_identified$tis)){
    g = g + geom_vline(data = df_identified$tis, aes(xintercept = .data$time,color="lightblue"))
  }

  # cfesis
  if(!is.null(df_identified$cfesis)){
    g = g + geom_vline(data = df_identified$cfesis, aes(xintercept = .data$time, color="darkgreen", linetype = .data$variable))
  }

  # csis
  if(!is.null(df_identified$csis)){
    g = g + geom_vline(data = df_identified$csis, aes(xintercept = .data$time, color="orange", linetype = .data$variable))
  }


  if(zero_line){g = g + geom_hline(aes(yintercept = 0))}
  g +
    geom_line(aes(y = .data$y,color="black"), linewidth = 0.7) +
    geom_line(aes(color = "blue"),linetype = 1, linewidth = 0.5) +



    # Faceting
    facet_wrap("id", scales = facet.scales) +

    scale_color_identity(name = NULL,
                         breaks = c("black", "blue", "grey", "purple", "red","lightblue","darkgreen", "orange", "violetred4"),
                         labels = c("y","Fitted","IIS","JSIS","FESIS","TIS","CFESIS", "CSIS", "User-Specified"),
                         guide = "legend")+

    scale_linetype(name = "Variable") +

  theme(#legend.position = "none",
    strip.background = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.border = element_rect(colour = "grey",fill = NA),
    panel.background = element_blank()#,
    #panel.grid.major.y = element_line(colour = "grey",linewidth = 0.1)
  ) +

    labs(title = title,subtitle = sub_title, y = NULL, x = NULL) -> plotoutput

  # # cfesis
  # if(!is.null(cfesis)){
  #   g = g + geom_vline(data = cfesis, aes(xintercept = .data$time, linetype = .data$variable, color="green"))
  # }#
  # # browser
  # #   if(interactive){
  # #     plotoutput <- plotly::ggplotly(p = plotoutput)
  # #   }
  #
  return(plotoutput)
}
