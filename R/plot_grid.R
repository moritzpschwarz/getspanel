#' Plotting an isatpanel object
#'
#' @param x An object produced by the isatpanel function
#' @param title Plot title. Must be a character vector.
#' @param regex_exclude_indicators A regex character vector to exclude the inclusion of certain indicators in the plot. Default = NULL. Use with care, experimental.
#' @param ... Further arguments to be passed to ggplot2.
#'
#' @return A ggplot2 plot that plots an 'isatpanel' object and shows all indicators as a grid to give a good and quick overview.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs theme element_blank element_rect element_line geom_hline geom_vline geom_tile scale_x_continuous scale_y_discrete scale_y_discrete theme_bw scale_fill_gradient2 .data scale_x_date
#' @importFrom stats aggregate
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
#'}

plot_grid <- function(x, title = NULL, regex_exclude_indicators = NULL, ...){

  #interactive = TRUE, currently not implemented. Roxygen: Logical (TRUE or FALSE). Default is TRUE. When True, plot will be passed to plotly using ggplotly.
  df <- x$estimateddata
  indicators <- x$isatpanel.result$aux$mX
  indicators <- indicators[,!colnames(indicators) %in% names(df), drop = FALSE]
  indicators <- indicators[,!grepl("^id|^time",colnames(indicators)), drop = FALSE]

  if(!is.null(regex_exclude_indicators)){
    indicators <- indicators[,!grepl(regex_exclude_indicators,colnames(indicators)),drop = FALSE]
  }

  df <- cbind(df,indicators)

  if(dim(indicators)[2] != 0){

    if(is.null(x$isatpanel.result$fit)){
      fitted <- as.numeric(x$isatpanel.result$mean.fit)
    } else {
      fitted <- as.numeric(x$isatpanel.result$fit)
    }

    # df_identified <- identify_indicator_timings(df)
    # impulses <- df_identified$impulses[names(df_identified$impulses) %in% c("id","time","value")]
    # impulses <- df_identified$impulses[names(df_identified$impulses) %in% c("id","time","value")]
    #
    # fesis <- df_identified$fesis[names(df_identified$fesis) %in% c("id","time")]
    # fesis$value <- 1
    # merge_fesis <- merge(x$estimateddata, fesis, by = c("id","time"), all.x = TRUE)
    # merge_fesis[is.na(merge_fesis$value),"value"] <- 0
    # merge_fesis$id <- as.factor(merge_fesis$id)
    # aggregate(merge_fesis$value, by = list(merge_fesis$id), cumsum)
    #
    #
    # steps <- df_identified$steps[names(df_identified$impulses) %in% c("id","time","value")]
    #
    # merge(x$estimateddata, impulses, by = c("id","time"), all.x = TRUE)
    indicators_df <- cbind(df[,names(df) %in% c("id","time")],indicators)
    varying_vars <- names(indicators_df)[!names(indicators_df)%in% c("id","time","y","fitted")]

    indicators_l <- reshape(indicators_df,
                            varying = varying_vars,
                            idvar = c("id","time"),
                            v.names = "value",
                            timevar = "name",
                            times = varying_vars,
                            direction = "long")

    # introduce facets
    default_facet_name <- "Intercept (IIS, FESIS, TIS)"
    #default_facet_name <- "Intercept (IIS, FESIS)"
    indicators_l$facet <- default_facet_name

    # Deal with TIS within facets
    #indicators_l[grepl("^tis",indicators_l$name),"value"] <- ifelse(indicators_l[grepl("^tis",indicators_l$name),"value"] != 0, 1, 0)
    #indicators_l[grepl("^tis",indicators_l$name),"facet"] <- "TIS"

    # Deal with CSIS within facets
    indicators_l[grepl("\\.csis[0-9]+",indicators_l$name),"value"] <- ifelse(indicators_l[grepl("\\.csis[0-9]+",indicators_l$name),"value"] != 0, 1, 0)
    indicators_l[grepl("\\.csis[0-9]+",indicators_l$name),"facet"] <- paste0("CSIS: ",gsub("\\.csis[0-9]+","",indicators_l[grepl("\\.csis[0-9]+",indicators_l$name),"name"]))

    # Deal with CFESIS within facets
    indicators_l[grepl("\\.cfesis.*[0-9]+",indicators_l$name),"value"] <- ifelse(indicators_l[grepl("\\.cfesis.*[0-9]+",indicators_l$name),"value"] != 0, 1, 0)
    indicators_l[grepl("\\.cfesis.*[0-9]+",indicators_l$name),"facet"] <- paste0("CFESIS: ",gsub("\\.[0-9]+$","",gsub("\\.cfesis.*[0-9]+","",indicators_l[grepl("\\.cfesis.*[0-9]+",indicators_l$name),"name"])))

    # Control the order of the facets
    facet_order <- unique(indicators_l$facet)
    facet_order <- c(default_facet_name, facet_order[!facet_order %in% default_facet_name])
    indicators_l$facet <- factor(indicators_l$facet, levels = facet_order)

    indicators_l_merged <- merge(indicators_l,
                                 data.frame(name = names(coef(x$isatpanel.result)),
                                            coef = coef(x$isatpanel.result)),
                                 by = "name", all.x = TRUE)

    indicators_l_merged$effect <-  indicators_l_merged$value*indicators_l_merged$coef

    indicators_toplot <- aggregate(indicators_l_merged$effect, by = list(indicators_l_merged$time, indicators_l_merged$id, indicators_l_merged$facet), function(x){sum(x,na.rm = TRUE)})
    names(indicators_toplot) <- c("time","id","facet","effect")
    indicators_toplot[indicators_toplot$effect == 0,"effect"] <- NA
    indicators_toplot$id <- factor(indicators_toplot$id, levels = rev(unique(indicators_toplot$id))) # swapping the order of the factors to make sure they are in alphabetical order in the plot

    # Figure out the colours if there is only one break
    col_limits <- c(min(indicators_toplot$effect, na.rm = TRUE), max(indicators_toplot$effect, na.rm = TRUE))

    if(col_limits[1] == col_limits[2]){
      if(col_limits[1]<0){col_limits[2] <- col_limits[2] * -1} else {col_limits[1] <- col_limits[1] * -1}
    } else if(sign(col_limits[1]) == sign(col_limits[2])){
      if(sign(col_limits[1]) < 0){col_limits[2] <- col_limits[1]*-1} else {col_limits[1] <- col_limits[2]*-1}
    }

    x_axis <- if(is.numeric(indicators_toplot$time)){
      list(scale_x_continuous(expand = c(0,0)))
    #} else if(is(indicators_toplot$time, class2 = "Date")){
    } else if(inherits(indicators_toplot$time, "Date")){
      list(scale_x_date(expand = c(0,0)))
    }

    ggplot(indicators_toplot, aes(x = .data$time, y = .data$id, fill = .data$effect)) +
      geom_tile(na.rm = TRUE) +
      #scale_fill_viridis_c(na.value = NA) +
      scale_fill_gradient2(limits = col_limits, na.value = NA, name = "Effect", midpoint = 0) +
      x_axis +
      scale_y_discrete(expand = c(0, 0)) +
      facet_wrap( ~ .data$facet)+
      theme_bw() +
      theme(panel.grid = element_blank(),
            panel.border = element_rect(fill = NA),
            strip.background = element_blank()) +
      labs(x = NULL, y = NULL)

  } else {message("No indicators identified in the isatpanel object. No plot produced.")}


  #
  # varying_vars <- names(df)[!names(df)%in% c("id","time","y","fitted")]
  #
  # df_l <- reshape(df,
  #                 varying = varying_vars,
  #                 idvar = c("id","time"),
  #                 v.names = "value",
  #                 timevar = "name",
  #                 times = varying_vars,
  #                 direction = "long")
  #
  #
  #
  #
  # # Impulses and Steps
  # impulses <- df_l[grepl("iis",df_l$name) & df_l$value == 1,]
  # steps <- df_l[grepl("sis",df_l$name) & df_l$value == 1 & !grepl("fesis", df_l$name) & !grepl("csis", df_l$name),]
  #
  # # FESIS
  # if(any(grepl("^fesis",names(df)))){
  #   fesis_wide <- df[,grepl("^fesis", names(df)), drop = FALSE]
  #   fesis_l <- reshape(fesis_wide,
  #                      direction = "long",
  #                      varying = names(fesis_wide),
  #                      times = names(fesis_wide),
  #                      v.names = "value",
  #                      timevar = "name")
  #
  #
  #   split_list <- strsplit(x = fesis_l$name, split = "\\.")
  #
  #   fesis_l$id <- unlist(lapply(split_list, `[[`, 1))
  #   fesis_l$id <- gsub("fesis","",fesis_l$id)
  #   fesis_l$time <- unlist(lapply(split_list, `[[`, 2))
  #   fesis_l$time <- as.numeric(fesis_l$time)
  #
  #   fesis_l <- fesis_l[c("id","time","name")]
  #
  #   fesis <- fesis_l[!duplicated(fesis_l),]
  #
  # } else {fesis <- NULL}
  #
  # # CFESIS
  # if(any(grepl("cfesis",names(df)))){
  #
  #   cfesis_wide <- df[,grepl("cfesis", names(df)), drop = FALSE]
  #   cfesis_l <- reshape(cfesis_wide,
  #                       direction = "long",
  #                       varying = names(cfesis_wide),
  #                       times = names(cfesis_wide),
  #                       v.names = "value",
  #                       timevar = "name")
  #
  #
  #   split_list <- strsplit(x = cfesis_l$name, split = "\\.")
  #
  #   cfesis_l$name <- unlist(lapply(split_list, `[[`, 1))
  #
  #   cfesis_l$id <- unlist(lapply(split_list, `[[`, 2))
  #   cfesis_l$id <- gsub("cfesis","",cfesis_l$id)
  #
  #   cfesis_l$time <- unlist(lapply(split_list, `[[`, 3))
  #   cfesis_l$time <- as.numeric(cfesis_l$time)
  #
  #   cfesis_l <- cfesis_l[c("id","time","name")]
  #
  #   cfesis <- cfesis_l[!duplicated(cfesis_l),]
  #
  #   # df %>%
  #   #   select(contains("cfesis")) %>%
  #   #   pivot_longer(cols = everything()) %>%
  #   #   separate(col = "name",sep = "\\.",into = c("variable","id","time")) %>%
  #   #   mutate(id = gsub("cfesis","",id),
  #   #                 time = as.numeric(time)) %>%
  #   #   select(-"value") %>%
  #   #   distinct(across(c("variable", "time", "id"))) -> cfesis
  # } else {cfesis <- NULL}
  #
  # # CSIS
  #
  # if(any(grepl("csis",names(df)))){
  #
  #   csis_wide <- df[,grepl("csis", names(df)), drop = FALSE]
  #   csis_l <- reshape(csis_wide,
  #                     direction = "long",
  #                     varying = names(csis_wide),
  #                     times = names(csis_wide),
  #                     v.names = "value",
  #                     timevar = "name")
  #
  #   split_list <- strsplit(x = csis_l$name, split = "\\.")
  #
  #   csis_l$name <- unlist(lapply(split_list, `[[`, 1))
  #   csis_l$time <- unlist(lapply(split_list, `[[`, 2))
  #   csis_l$time <- gsub("csis","",csis_l$time)
  #   csis_l$time <- as.numeric(csis_l$time)
  #
  #   csis_l <- csis_l[c("time","name")]
  #
  #   csis <- csis_l[!duplicated(csis_l),]
  #
  #   # df %>%
  #   #   select(contains("cfesis")) %>%
  #   #   pivot_longer(cols = everything()) %>%
  #   #   separate(col = "name",sep = "\\.",into = c("variable","id","time")) %>%
  #   #   mutate(id = gsub("cfesis","",id),
  #   #                 time = as.numeric(time)) %>%
  #   #   select(-"value") %>%
  #   #   distinct(across(c("variable", "time", "id"))) -> cfesis
  # } else {csis <- NULL}
  #
  # sub_title <- NULL
  #
  # ggplot(df, aes(
  #   x = .data$time,
  #   y = fitted,
  #   group = .data$id
  # )) -> g
  #
  #
  # # Impulses
  # if(nrow(impulses)>0){
  #   g = g + geom_vline(data = impulses,aes(xintercept = .data$time,color="grey"))
  # }
  # # Steps
  # if(nrow(steps)>0){
  #   g = g + geom_vline(data = steps, aes(xintercept = time,color="purple"))
  # }
  # # fesis
  # if(!is.null(fesis)){
  #   g = g + geom_vline(data = fesis, aes(xintercept = .data$time,color="red"))
  # }
  #
  # # cfesis
  # if(!is.null(cfesis)){
  #   g = g + geom_vline(data = cfesis, aes(xintercept = .data$time, color="darkgreen", linetype = .data$name))
  # }
  #
  # # csis
  # if(!is.null(csis)){
  #   g = g + geom_vline(data = csis, aes(xintercept = .data$time, color="orange", linetype = .data$name))
  # }
  #
  # g +
  #   geom_line(aes(y = .data$y,color="black"), size = 0.7) +
  #   geom_line(aes(color = "blue"),linetype = 1, size = 0.5) +
  #   geom_hline(aes(yintercept = 0)) +
  #
  #   # Faceting
  #   facet_wrap("id", scales = facet.scales) +
  #
  #   scale_color_identity(name = NULL,
  #                        breaks = c("black", "blue", "grey", "purple", "red","darkgreen", "orange"),
  #                        labels = c("y","Fitted","IIS","SIS","FESIS","CFESIS", "CSIS"),
  #                        guide = "legend")+
  #
  #   scale_linetype(name = "Variable") +
  #
  #   theme(#legend.position = "none",
  #     strip.background = element_blank(),
  #     legend.key = element_rect(fill = NA),
  #     panel.border = element_rect(colour = "grey",fill = NA),
  #     panel.background = element_blank()#,
  #     #panel.grid.major.y = element_line(colour = "grey",size = 0.1)
  #   ) +
  #
  #   labs(title = title,subtitle = sub_title, y = NULL, x = NULL) -> plotoutput
  #
  # # cfesis
  # if(!is.null(cfesis)){
  #   g = g + geom_vline(data = cfesis, aes(xintercept = .data$time, linetype = .data$variable, color="green"))
  # }#
  # # browser
  # #   if(interactive){
  # #     plotoutput <- plotly::ggplotly(p = plotoutput)
  # #   }
  #
  # return(plotoutput)

}
