#' Plotting an isatpanel object
#'
#' @param x An object produced by the isatpanel function
#' @param max.id.facet The resulting plot will be faceted for each individual in the panel. Beyond a certain number, this might result in unreadable figures. Default set at 16.
#' @param facet.scales To be passed to ggplot2::facet_wrap. Default is "free" (i.e. a separate y axis for each panel group/id). Alternatives are: "fixed", "fixed_y", and "fixed_x".
#' @param title Plot title. Must be a character vector.
#' @param ... Further arguments to be passed to ggplot2.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs theme element_blank element_rect element_line geom_hline geom_vline aes_
#'
plot.isatpanel <- function(x, max.id.facet = 16, facet.scales = "free", title = NULL, ...){

  #interactive = TRUE, currently not implemented. Roxygen: Logical (TRUE or FALSE). Default is TRUE. When True, plot will be passed to plotly using ggplotly.


  df <- x$estimateddata
  indicators <- x$isatpanel.result$aux$mX
  indicators <- indicators[,!colnames(indicators) %in% names(df)]
  df <- cbind(df,indicators)

  if(is.null(x$isatpanel.result$fit)){
    fitted <- as.numeric(x$isatpanel.result$mean.fit)
  } else {
    fitted <- as.numeric(x$isatpanel.result$fit)
  }

  df_identified <- identify_indicator_timing(df)

  sub_title <- NULL

  ggplot(df, aes_(
    x = ~time,
    y = ~fitted,
    group = ~id
  )) -> g


  # Impulses
  if(nrow(df_identified$impulses)>0){
    g = g + geom_vline(data = df_identified$impulses,aes_(xintercept = ~time,color="grey"))
  }
  # Steps
  if(nrow(df_identified$steps)>0){
    g = g + geom_vline(data = df_identified$steps, aes_(xintercept = time,color="purple"))
  }
  # fesis
  if(!is.null(df_identified$fesis)){
    g = g + geom_vline(data = df_identified$fesis, aes_(xintercept = ~time,color="red"))
  }

  # cfesis
  if(!is.null(df_identified$cfesis)){
    g = g + geom_vline(data = df_identified$cfesis, aes_(xintercept = ~time, color="darkgreen", linetype = ~name))
  }

  # csis
  if(!is.null(df_identified$csis)){
    g = g + geom_vline(data = df_identified$csis, aes_(xintercept = ~time, color="orange", linetype = ~name))
  }

  g +
    geom_line(aes_(y = ~y,color="black"), size = 0.7) +
    geom_line(aes(color = "blue"),linetype = 1, size = 0.5) +
    geom_hline(aes(yintercept = 0)) +

    # Faceting
    facet_wrap("id", scales = facet.scales) +

    scale_color_identity(name = NULL,
                         breaks = c("black", "blue", "grey", "purple", "red","darkgreen", "orange"),
                         labels = c("y","Fitted","IIS","SIS","FESIS","CFESIS", "CSIS"),
                         guide = "legend")+

    scale_linetype(name = "Variable") +

  theme(#legend.position = "none",
    strip.background = element_blank(),
    legend.key = element_rect(fill = NA),
    panel.border = element_rect(colour = "grey",fill = NA),
    panel.background = element_blank()#,
    #panel.grid.major.y = element_line(colour = "grey",size = 0.1)
  ) +

    labs(title = title,subtitle = sub_title, y = NULL, x = NULL) -> plotoutput

  # # cfesis
  # if(!is.null(cfesis)){
  #   g = g + geom_vline(data = cfesis, aes_(xintercept = ~time, linetype = ~variable, color="green"))
  # }#
  # # browser
  # #   if(interactive){
  # #     plotoutput <- plotly::ggplotly(p = plotoutput)
  # #   }
  #
  return(plotoutput)
}
