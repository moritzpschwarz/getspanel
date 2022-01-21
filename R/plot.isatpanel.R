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

  varying_vars <- names(df)[!names(df)%in% c("id","time","y","fitted")]

  df_l <- reshape(df,
                  varying = varying_vars,
                  idvar = c("id","time"),
                  v.names = "value",
                  timevar = "name",
                  times = varying_vars,
                  direction = "long")

  # Impulses and Steps
  impulses <- df_l[grepl("iis",df_l$name) & df_l$value == 1,]
  steps <- df_l[grepl("sis",df_l$name) & df_l$value == 1 & !grepl("fesis", df_l$name) & !grepl("csis", df_l$name),]

  # FESIS
  if(any(grepl("^fesis",names(df)))){
    fesis_wide <- df[,grepl("^fesis", names(df)), drop = FALSE]
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

  # CFESIS
  if(any(grepl("cfesis",names(df)))){

    cfesis_wide <- df[,grepl("cfesis", names(df)), drop = FALSE]
    cfesis_l <- reshape(cfesis_wide,
                        direction = "long",
                        varying = names(cfesis_wide),
                        times = names(cfesis_wide),
                        v.names = "value",
                        timevar = "name")


    split_list <- strsplit(x = cfesis_l$name, split = "\\.")

    cfesis_l$name <- unlist(lapply(split_list, `[[`, 1))

    cfesis_l$id <- unlist(lapply(split_list, `[[`, 2))
    cfesis_l$id <- gsub("cfesis","",cfesis_l$id)

    cfesis_l$time <- unlist(lapply(split_list, `[[`, 3))
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

  # CSIS

  if(any(grepl("csis",names(df)))){

    csis_wide <- df[,grepl("csis", names(df)), drop = FALSE]
    csis_l <- reshape(csis_wide,
                        direction = "long",
                        varying = names(csis_wide),
                        times = names(csis_wide),
                        v.names = "value",
                        timevar = "name")

    split_list <- strsplit(x = csis_l$name, split = "\\.")

    csis_l$name <- unlist(lapply(split_list, `[[`, 1))
    csis_l$time <- unlist(lapply(split_list, `[[`, 2))
    csis_l$time <- gsub("csis","",csis_l$time)
    csis_l$time <- as.numeric(csis_l$time)

    csis_l <- csis_l[c("time","name")]

    csis <- csis_l[!duplicated(csis_l),]

    # df %>%
    #   select(contains("cfesis")) %>%
    #   pivot_longer(cols = everything()) %>%
    #   separate(col = "name",sep = "\\.",into = c("variable","id","time")) %>%
    #   mutate(id = gsub("cfesis","",id),
    #                 time = as.numeric(time)) %>%
    #   select(-"value") %>%
    #   distinct(across(c("variable", "time", "id"))) -> cfesis
  } else {csis <- NULL}

  sub_title <- NULL

  ggplot(df, aes_(
    x = ~time,
    y = ~fitted,
    group = ~id
  )) -> g


  # Impulses
  if(nrow(impulses)>0){
    g = g + geom_vline(data = impulses,aes_(xintercept = ~time,color="grey"))
  }
  # Steps
  if(nrow(steps)>0){
    g = g + geom_vline(data = steps, aes_(xintercept = time,color="purple"))
  }
  # fesis
  if(!is.null(fesis)){
    g = g + geom_vline(data = fesis, aes_(xintercept = ~time,color="red"))
  }

  # cfesis
  if(!is.null(cfesis)){
    g = g + geom_vline(data = cfesis, aes_(xintercept = ~time, color="darkgreen", linetype = ~name))
  }

  # csis
  if(!is.null(csis)){
    g = g + geom_vline(data = csis, aes_(xintercept = ~time, color="orange", linetype = ~name))
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

  # cfesis
  if(!is.null(cfesis)){
    g = g + geom_vline(data = cfesis, aes_(xintercept = ~time, linetype = ~variable, color="green"))
  }#
  # # browser
  # #   if(interactive){
  # #     plotoutput <- plotly::ggplotly(p = plotoutput)
  # #   }
  #
  return(plotoutput)
}
