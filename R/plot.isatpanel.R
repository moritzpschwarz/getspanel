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
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs theme element_blank element_rect element_line geom_hline geom_vline aes_string
#'
plot.isatpanel <- function(x, max.id.facet = 16, facet.scales = "free", title = "Panel Saturation", ...){

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

  varying_vars <- names(df)[!names(df)%in% c("id","time","y","fitted")]

  df_l <- reshape(df,
                  varying = varying_vars,
                  idvar = c("id","time"),
                  v.names = "value",
                  timevar = "name",
                  times = varying_vars,
                  direction = "long")

  impulses <- df_l[grepl("iis",df_l$name) & df_l$value == 1,]
  steps <- df_l[grepl("sis",df_l$name) & df_l$value == 1 & !grepl("fesis", df_l$name),]


  if(any(grepl("^fesis",names(df)))){
    fesis_wide <- df[,grepl("^fesis", names(df))]
    fesis_l <- reshape(fesis_wide,
                       direction = "long",
                       varying = names(fesis_wide),
                       times = names(fesis_wide),
                       v.names = "value",
                       timevar = "name")


    split_list <- strsplit(x = fesis_l$name, split = "\\.")

    fesis_l$id <- unlist(lapply(split_list, `[[`, 1))
    fesis_l$id <- gsub("fesis","",fesis_l$id)
    fesis_l$time <- unlist(lapply(split_list, `[[`, 2))
    fesis_l$time <- as.numeric(fesis_l$time)

    fesis_l <- fesis_l[c("id","time","name")]

    fesis <- fesis_l[!duplicated(fesis_l),]

  } else {fesis <- NULL}

  if(any(grepl("cfesis",names(df)))){

    cfesis_wide <- df[,grepl("^cfesis", names(df))]
    cfesis_l <- reshape(cfesis_wide,
                       direction = "long",
                       varying = names(cfesis_wide),
                       times = names(cfesis_wide),
                       v.names = "value",
                       timevar = "name")


    split_list <- strsplit(x = cfesis_l$name, split = "\\.")

    cfesis_l$id <- unlist(lapply(split_list, `[[`, 1))
    cfesis_l$id <- gsub("cfesis","",cfesis_l$id)
    cfesis_l$time <- unlist(lapply(split_list, `[[`, 2))
    cfesis_l$time <- as.numeric(cfesis_l$time)

    cfesis_l <- cfesis_l[c("id","time","name")]

    cfesis <- cfesis_l[!duplicated(cfesis_l),]

    # df %>%
    #   select(contains("cfesis")) %>%
    #   pivot_longer(cols = everything()) %>%
    #   separate(col = "name",sep = "\\.",into = c("variable","id","time")) %>%
    #   mutate(id = gsub("cfesis","",id),
    #                 time = as.numeric(time)) %>%
    #   select(-"value") %>%
    #   distinct(across(c("variable", "time", "id"))) -> cfesis
  } else {cfesis <- NULL}

  ggplot(df, aes_string(
    x = "time",
    y = "fitted",
    group = "id",
  )) +


    # Impulses
    geom_vline(data = impulses,aes_string(xintercept = "time"),color="grey",size = 0.1) +

    # Steps
    geom_vline(data = steps, aes_string(xintercept = "time"),color="darkgreen",size = 0.1) -> g

  # fesis
  if(!is.null(fesis)){
    g = g + geom_vline(data = fesis, aes_string(xintercept = "time"),color="blue",size = 0.1)
  }

  # cfesis
  if(!is.null(cfesis)){
    g = g + geom_vline(data = cfesis, aes_string(xintercept = "time", linetype = "variable"),color="blue",size = 0.1)
  }


  g +
    geom_line(aes_string(y = "y"),size = 1, linetype = 1, color="black") +
    geom_line(linetype = 1, color="blue") +

    # Faceting
    facet_wrap("id", scales = facet.scales) +

    theme(legend.position = "none",
                   strip.background = element_blank(),
                   panel.border = element_rect(colour = "grey",fill = NA),
                   panel.background = element_blank(),
                   panel.grid.major.y = element_line(colour = "grey",size = 0.1)) +

    labs(title = title,subtitle = "Grey: Impulse - Blue: FE Steps - Green: Steps\nBlue line fitted", y = NULL, x = NULL) -> plotoutput

  #
  # # browser
  # #   if(interactive){
  # #     plotoutput <- plotly::ggplotly(p = plotoutput)
  # #   }
  #
  return(plotoutput)
}
