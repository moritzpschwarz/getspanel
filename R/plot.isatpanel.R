plot.isatpanel <- function(isatpanelobject, max.id.facet = 16, facet.scales = "free", ...){

  df <- isatpanelobject$estimateddata
  indicators <- isatpanelobject$isatpanel.result$aux$mX
  indicators <- indicators[,!colnames(indicators) %in% names(df)]
  df <- cbind(df,indicators)

  if(is.null(isatpanelobject$isatpanel.result$fit)){
    fitted <- isatpanelobject$isatpanel.result$mean.fit
  } else {
    fitted <- isatpanelobject$isatpanel.result$fit
  }

  df <- cbind(df,fitted)


  df %>%
    #dplyr::select(-mxbreak) %>%
    tidyr::pivot_longer(cols = -c(id,time,y,fitted)) %>%
    dplyr::filter(grepl("iis",name)) %>%
    dplyr::filter(value == 1) -> impulses

  df %>%
    #dplyr::select(-mxbreak) %>%
    tidyr::pivot_longer(cols = -c(id,time,y,fitted)) %>%
    dplyr::filter(grepl("sis",name)) %>%
    dplyr::filter(!grepl("fesis",name)) %>%
    dplyr::filter(value == 1) -> steps

  if(any(grepl("^fesis",names(df)))){
    df %>%
      dplyr::select(starts_with("fesis")) %>%
      tidyr::pivot_longer(cols = everything()) %>%
      tidyr::separate(col = name,sep = "\\.",into = c("id","time")) %>%
      dplyr::mutate(id = gsub("fesis","",id),
             time = as.numeric(time)) %>%
      dplyr::select(-value) %>%
      #mutate(time = time + min(df$time)-1) %>%
      dplyr::distinct(time,id) -> fesis
  } else {fesis <- NULL}

  if(any(grepl("cfesis",names(df)))){
    df %>%
      dplyr::select(contains("cfesis")) %>%
      tidyr::pivot_longer(cols = everything()) %>%
      tidyr::separate(col = name,sep = "\\.",into = c("variable","id","time")) %>%
      dplyr::mutate(id = gsub("fesis","",id),
                    time = as.numeric(time)) %>%
      dplyr::select(-value) %>%
      #mutate(time = time + min(df$time)-1) %>%
      dplyr::distinct(variable, time,id) -> cfesis
  } else {cfesis <- NULL}


  ggplot2::ggplot(df, ggplot2::aes(
    x = time,
    y = fitted,
    group = id,
    #color = id
  )) +


    # Impulses
    ggplot2::geom_vline(data = impulses,ggplot2::aes(xintercept = time),color="grey",size = 0.1) +

    # Steps
    ggplot2::geom_vline(data = steps, ggplot2::aes(xintercept = time),color="darkgreen",size = 0.1) -> g

  # fesis
  if(!is.null(fesis)){
    g = g + ggplot2::geom_vline(data = fesis, ggplot2::aes(xintercept = time),color="blue",size = 0.1)
  }

  # cfesis
  if(!is.null(cfesis)){
    g = g + ggplot2::geom_vline(data = cfesis, ggplot2::aes(xintercept = time,linetype = variable),color="blue",size = 0.1)
  }


  g +
    ggplot2::geom_line(ggplot2::aes(y = y),size = 1, linetype = 1, color="black") +
    ggplot2::geom_line(linetype = 1, color="blue") +

    # Faceting
    ggplot2::facet_wrap( ~ id, scales = facet.scales) +

    ggplot2::theme(legend.position = "none",
          strip.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(colour = "grey",size = 0.1)) +

    ggplot2::labs(title = "Panel Saturation",subtitle = "Grey: Impulse - Blue: FE Steps - Green: Steps\nBlue line fitted") -> plot

  return(plot)
}
