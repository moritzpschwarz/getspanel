#' Plotting an isatpanel object
#'
#' @param isatpanelobject An object produced by the isatpanel function
#' @param max.id.facet The resulting plot will be faceted for each individual in the panel. Beyond a certain number, this might result in unreadable figures. Default set at 16.
#' @param facet.scales To be passed to ggplot2::facet_wrap. Default is "free" (i.e. a separate y axis for each panel group/id). Alternatives are: "fixed", "fixed_y", and "fixed_x".
#' @param title Plot title. Must be a character vector.
#' @param ... Further arguments to be passed to ggplot2.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs theme element_blank element_rect element_line geom_hline geom_vline
#'
plot.isatpanel <- function(isatpanelobject, max.id.facet = 16, facet.scales = "free", title = "Panel Saturation", ...){

  #interactive = TRUE, currently not implemented. Roxygen: Logical (TRUE or FALSE). Default is TRUE. When True, plot will be passed to plotly using ggplotly.


  df <- isatpanelobject$estimateddata
  indicators <- isatpanelobject$isatpanel.result$aux$mX
  indicators <- indicators[,!colnames(indicators) %in% names(df)]
  df <- cbind(df,indicators)

  if(is.null(isatpanelobject$isatpanel.result$fit)){
    fitted <- as.numeric(isatpanelobject$isatpanel.result$mean.fit)
  } else {
    fitted <- as.numeric(isatpanelobject$isatpanel.result$fit)
  }

  df <- cbind(df,fitted)


  df %>%
    tidyr::pivot_longer(cols = -c(id,time,y,fitted)) %>%
    dplyr::filter(grepl("iis",name)) %>%
    dplyr::filter(value == 1) -> impulses

  df %>%
    tidyr::pivot_longer(cols = -c(id,time,y,fitted)) %>%
    dplyr::filter(grepl("sis",name)) %>%
    dplyr::filter(!grepl("fesis",name)) %>%
    dplyr::filter(value == 1) -> steps

  if(any(grepl("^fesis",names(df)))){
    df %>%
      dplyr::select(starts_with("fesis")) %>%
      tidyr::pivot_longer(cols = dplyr::everything()) %>%
      tidyr::separate(col = name,sep = "\\.",into = c("id","time")) %>%
      dplyr::mutate(id = gsub("fesis","",id),
                    time = as.numeric(time)) %>%
      dplyr::select(-value) %>%
      dplyr::distinct(time,id) -> fesis
  } else {fesis <- NULL}

  if(any(grepl("cfesis",names(df)))){
    df %>%
      dplyr::select(contains("cfesis")) %>%
      tidyr::pivot_longer(cols = everything()) %>%
      tidyr::separate(col = name,sep = "\\.",into = c("variable","id","time")) %>%
      dplyr::mutate(id = gsub("cfesis","",id),
                    time = as.numeric(time)) %>%
      dplyr::select(-value) %>%
      dplyr::distinct(variable, time,id) -> cfesis
  } else {cfesis <- NULL}


  ggplot(df, aes(
    x = time,
    y = fitted,
    group = id,
  )) +


    # Impulses
    geom_vline(data = impulses,aes(xintercept = time),color="grey",size = 0.1) +

    # Steps
    geom_vline(data = steps, aes(xintercept = time),color="darkgreen",size = 0.1) -> g

  # fesis
  if(!is.null(fesis)){
    g = g + geom_vline(data = fesis, aes(xintercept = time),color="blue",size = 0.1)
  }

  # cfesis
  if(!is.null(cfesis)){
    g = g + geom_vline(data = cfesis, aes(xintercept = time,linetype = variable),color="blue",size = 0.1)
  }


  g +
    geom_line(aes(y = y),size = 1, linetype = 1, color="black") +
    geom_line(linetype = 1, color="blue") +

    # Faceting
    facet_wrap( ~ id, scales = facet.scales) +

    theme(legend.position = "none",
                   strip.background = element_blank(),
                   panel.border = element_rect(colour = "grey",fill = NA),
                   panel.background = element_blank(),
                   panel.grid.major.y = element_line(colour = "grey",size = 0.1)) +

    labs(title = title,subtitle = "Grey: Impulse - Blue: FE Steps - Green: Steps\nBlue line fitted", y = NULL, x = NULL) -> plotoutput


  # browser
  #   if(interactive){
  #     plotoutput <- plotly::ggplotly(p = plotoutput)
  #   }

  return(plotoutput)
}
