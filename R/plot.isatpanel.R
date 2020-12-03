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
    filter(grepl("iis",name)) %>%
    filter(value == 1) -> impulses

  df %>%
    #dplyr::select(-mxbreak) %>%
    tidyr::pivot_longer(cols = -c(id,time,y,fitted)) %>%
    filter(grepl("sis",name)) %>%
    filter(!grepl("fesis",name)) %>%
    filter(value == 1) -> steps

  if(any(grepl("fesis",names(df)))){
    df %>%
      dplyr::select(starts_with("fesis")) %>%
      pivot_longer(cols = everything()) %>%
      separate(col = name,sep = "\\.",into = c("id","time")) %>%
      mutate(id = gsub("fesis","",id),
             time = as.numeric(time)) %>%
      select(-value) %>%
      #mutate(time = time + min(df$time)-1) %>%
      distinct(time,id) -> fesis
  } else {fesis <- NULL}




  ggplot2::ggplot(df, aes(
    x = time,
    y = fitted,
    group = id,
    #color = id
  )) +


    # Impulses
    geom_vline(data = impulses,aes(xintercept = time),color="grey",size = 0.1) +

    # Steps
    geom_vline(data = steps, aes(xintercept = time),color="darkgreen",size = 0.1) -> g

  # fesis
  if(!is.null(fesis)){
    g = g + geom_vline(data = fesis, aes(xintercept = time),color="blue",size = 0.1)
  }


  g +
    geom_line(aes(y = y),size = 1, linetype = 1, color="black") +
    geom_line(linetype = 1, color="blue") +

    # Faceting
    facet_wrap( ~ id, scales = facet.scales) +

    theme(legend.position = "none",
          strip.background = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(colour = "grey",size = 0.1)) +

    labs(title = "Panel Saturation",subtitle = "Grey: Impulse - Blue: mxbreak - Green: Steps\nBlue line fitted") -> plot

  return(plot)
}
