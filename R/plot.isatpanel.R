plot.isatpanel <- function(isatpanelobject, max.id.facet = 16, facet.scales = "fixed", ...){

  df <- isatpanelobject$inputdata
  indicators <- isatpanelobject$isatpanel.result$aux$mX
  indicators <- indicators[,!colnames(indicators) %in% names(df)]
  df <- cbind(df,indicators)

  fitted <- isatpanelobject$isatpanel.result$fit
  df <- cbind(df,fitted)


  df %>%
    dplyr::select(-mxbreak) %>%
    tidyr::pivot_longer(cols = -c(id,time,y,fitted)) %>%
    filter(grepl("iis",name)) %>%
    filter(value == 1) -> impulses

  df %>%
    dplyr::select(-mxbreak) %>%
    tidyr::pivot_longer(cols = -c(id,time,y,fitted)) %>%
    filter(grepl("sis",name)) %>%
    filter(value == 1) -> steps

  df %>%
    dplyr::select(starts_with("mxbreak"),-mxbreak) %>%
    pivot_longer(cols = everything()) %>%
    separate(col = name,sep = "id",into = c("time","id")) %>%
    mutate(time = as.numeric(gsub("mxbreak1t","",time))) %>%
    select(-value) %>%
    mutate(time = time + min(df$time)-1) %>%
    distinct(time,id) -> mxbreak


  ggplot2::ggplot(df, aes(
    x = time,
    y = fitted,
    group = id,
    #color = id
  )) +


    # Impulses
    geom_vline(data = impulses,aes(xintercept = time),color="grey",size = 0.1) +

    # Steps
    geom_vline(data = steps, aes(xintercept = time),color="green",size = 0.1) +

    # mxbreaks
    geom_vline(data = mxbreak, aes(xintercept = time),color="blue",size = 0.1) +


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
