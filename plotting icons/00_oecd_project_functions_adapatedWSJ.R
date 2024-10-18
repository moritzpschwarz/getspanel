library(data.table)
library(dplyr)
library(tidyr)
library(xlsx)
library(stringr)
library(gets)
library(getspanel)
library(countrycode)
library(vtable)
library(cowplot)
library(ggplot2)
library(pheatmap)
library(cowplot)
library(gsubfn)
library(data.table)
library(tidyverse)
library(openxlsx)
library(gets)
library(getspanel)
library(here)
library(doParallel)
library(gridExtra)
library(conflicted)
library(viridis)
library(gplots)
library(devtools)
library(RColorBrewer)
library(Polychrome)
library(grid)
library(scales)
library("rnaturalearth")
library("rnaturalearthdata")
library(wesanderson)
library(seriation)
library(ggpubr)

conflict_prefer("filter", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("here","here")
conflicts_prefer(ggpubr::get_legend)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(lubridate::year)

## takes an isatpanel object, uses break_uncertainty() in getspanel to format output and get uncertainty range

get_breaks_list <- function(res){
  out = break_uncertainty(res)
  out$country_code = countrycode(out$id,'country.name','iso3c')
  out[out$id=='SouthKorea','country_code'] = 'KOR'
  out[out$id=='SouthAfrica','country_code'] = 'ZAF'
  if(any(is.na(out$country_code))){
    print('Warning! Unmatched country names in out. Investigate!')
  }
  out$min_year = as.numeric(out$time)-as.numeric(out$tci)
  out$max_year = as.numeric(out$time)+as.numeric(out$tci)
  #filter negative breaks
  out = out[out$coef <0,]
  return(out)
}

## takes the preprocessed oecd data (as data), the formated break detection data (out), a sector (module) + optional parameters
## automatically extracts for each break and module the policies that fall between the max and min year
## uses the tci in out for break range unless a fixed-interval is specified (as an int)
## if tci_interval is true, it takes for each break the maximum of the specified fixed interval and the tci
## if introductions_only is true, the oecd data is filtered to only match introductions

match_oecd_policies <- function(data,out,module,fixed_interval = 0, tci_interval = FALSE, introductions_only = FALSE){


  if(tci_interval == TRUE){
    #in this case, we keep the maximum of the tci interval and the fixed interval
    breaks_modify = which(out$tci<= fixed_interval)

    out$min_year[breaks_modify] = out$time[breaks_modify]-fixed_interval
    out$max_year[breaks_modify] = out$time[breaks_modify]+fixed_interval

  }else if(fixed_interval>0){
    out$min_year = out$time-fixed_interval
    out$max_year = out$time+fixed_interval

  }

  if(introductions_only == TRUE){
    #in this case we only want to keep introductions and add ons
    add_ons = data[data$source == 'add-on', ]
    data = data[data$introduction == 1,]
    data = data[!is.na(data$ISO),]

    data = rbind(data,add_ons)
  }

  ##match break policies
  policy_store = data.frame()
  rownames(out) <- NULL
  for(i in 1:nrow(out)){
    policy_match = filter_oecd(data,out$country_code[i],out$min_year[i],out$max_year[i],module)
    if(nrow(policy_match)>0){
      policy_match$coeff = out$coef[i]
      policy_match$min_year = out$min_year[i]
      policy_match$max_year = out$max_year[i]
      policy_match$unique_break_identifier = paste(out$country_code[i],out$min_year[i],out$max_year[i],sep='_')}
    policy_store = rbind(policy_store,policy_match)
  }
  return(policy_store)
}

#Takes the "out" output and computes overlapping breaks. Overlapping means two breaks happen in the same sector and country
# s.t. the confidence interval of one completely contains the other break. In that case, we keep the one with the wider confidence
#interval

##NOTE: THE BREAK OVERLAP IS DONE SOLELY ON THE TCI INTERVAL ATM AND DOES NOT CHANGE FOR FIXED CONFIDENCE INTS

filter_break_overlap <- function(df){
  df$included_count <- 0  # Initialize overlap count for each event

  for (i in 1:nrow(df)) {
    # Extract the current event's country and confidence interval
    current_country <- df$id[i]
    current_min_year <- df$min_year[i]
    current_max_year <- df$max_year[i]

    # Find other events in the same country
    same_country_events <- df[df$id == current_country, ]

    # Check for overlap with other events
    for (j in 1:nrow(same_country_events)) {
      if (df$time[i] != same_country_events$time[j]) {
        other_min_year <- same_country_events$min_year[j]
        other_max_year <- same_country_events$max_year[j]

        # Check if the intervals overlap
        if (current_min_year >= other_min_year && current_max_year <= other_max_year) {
          df$included_count[i] <- df$included_count[i] + 1
          print(paste('The intervals overlap for',as.character(current_country),as.character(current_min_year),as.character(current_max_year),'and',as.character(current_country),as.character(other_min_year),as.character(other_max_year),sep=' '))
        }
      }
    }
  }
  df_filtered <- df[df$included_count ==0,]
  return(df_filtered)
}

## PLOTTING FOR FIGS 2 + 3

##function for model picking
f <- function(k) {
  step <- k
  function(y) seq(floor(min(y)), ceiling(max(y)), by = step)
}

#adjusted from getspanel package for this project

plot_counterfactual <- function(x, country, out, plus_t = 5, facet.scales = "free", title = NULL, zero_line = FALSE,int_size=2){
  df <- x$estimateddata
  indicators <- x$isatpanel.result$aux$mX
  indicators <- indicators[,!colnames(indicators) %in% names(df)]
  df <- cbind(df,indicators)

  if(is.null(x$isatpanel.result$fit)){
    fitted <- as.numeric(x$isatpanel.result$mean.fit)
  } else {
    fitted <- as.numeric(x$isatpanel.result$fit)
  }
  df$fitted = fitted

  df_ident <- out
  df_ident$maxtime = max(out$max_year)
  df_ident$origtime <- df_ident$time

  # make sure the preceding observation collapses on the last observation
  df_ident_start <- df_ident
  df_ident_start$time <- df_ident_start$time - 1
  df_ident_start$coef <- 0
  df_ident_start$sd <- 0
  df_ident_start$tci <- NA

  df_ident_overall <- rbind(df_ident_start, df_ident)
  for(i in 1:plus_t){
    intermed <- df_ident
    intermed$time <- intermed$time + i
    intermed$time <- ifelse(intermed$time > intermed$maxtime, intermed$maxtime, intermed$time)
    df_ident_overall <- rbind(df_ident_overall, intermed)
  }
  df_ident_overall <- df_ident_overall[order(df_ident_overall$name, df_ident_overall$time),]
  df_ident_overall <- df_ident_overall[!duplicated(df_ident_overall),]


  effects <- merge(x$estimateddata, df_ident_overall, by = c("id","time"), all.x = TRUE)
  effects <- merge(effects,data.frame(x$estimateddata[,c("id","time")], fitted), by = c("id","time"))

  effects$cf <-  (effects$coef * (-1)) +  effects$fitted
  effects$cf_upr <- ((effects$coef + (1.96 * effects$sd)) * (-1)) +  effects$fitted
  effects$cf_lwr <- ((effects$coef - (1.96 * effects$sd)) * (-1)) +  effects$fitted
  effects$cf_upr99 <- ((effects$coef + (2.57 * effects$sd)) * (-1)) +  effects$fitted
  effects$cf_lwr99 <- ((effects$coef - (2.57 * effects$sd)) * (-1)) +  effects$fitted

  effects$start_rect <- effects$origtime - effects$tci
  effects$end_rect <- effects$origtime + effects$tci

  effects$cf_upr[is.na(effects$cf_upr)] <- effects$fitted[is.na(effects$cf_upr)]
  effects$cf_lwr[is.na(effects$cf_lwr)] <- effects$fitted[is.na(effects$cf_lwr)]

  df <- subset(df,id==country)
  effects <- subset(effects, id==country)
  effects_error = effects
  effects_error <- effects_error[!is.na(effects_error$start_rect),]
  effects_error <- effects_error[!duplicated(effects_error$name),]
  effects_2y <- effects_error
  effects_2y$min_year = effects_2y$time-int_size
  effects_2y$max_year = effects_2y$time+int_size
  out_plot <- subset(out, id==country)
  out_plot <- as.data.frame(out_plot)

  title = trimws(gsub("([A-Z])", " \\1", country, perl = TRUE))
  if(title == "Czech Republic"){
    title = "Czechia"
  }
  if(title == "Slovak Republic"){
    title = "Slovakia"
  }

  ggplot(df, aes_(
    x = ~time,
    y = ~fitted,
    group = ~id
  )) -> g



  if(zero_line){g = g + geom_hline(aes(yintercept = 0))}

  g +
    geom_line(aes_(y = ~y, color = "black"), size = 0.7, show.legend=FALSE) +

    geom_rect(data = effects_error, aes(xmin = min_year, xmax = max_year, ymin = -Inf, ymax = Inf),fill = "grey",alpha = 0.5, na.rm = TRUE, show.legend=FALSE) +

    geom_rect(data = effects_2y, aes(xmin = min_year, xmax = max_year, ymin = -Inf, ymax = Inf),fill = "grey",alpha = 0.3, na.rm = TRUE, show.legend=FALSE) +

    geom_line(aes(color = "blue"),linetype = 1, size = 0.5,show.legend = FALSE) +

    # fesis
    #geom_rect(data = out_plot, aes(xmin = ~min_year, xmax = ~max_year, ymin = -Inf, ymax = Inf, group = ~name),fill = "grey",alpha = 0.3, na.rm = TRUE, show.legend=FALSE)+

    geom_vline(data = out_plot, aes_(xintercept = ~time,color="red"),show.legend = FALSE) -> g1


  for(i in unique(effects$name)){
    # i = unique(effects$name)[2]
    if(is.na(i)){next}
    effects %>%
      filter(name == i) -> dat

    g1 + geom_ribbon(data = dat, aes(x = time,
                                     ymin = cf_lwr, ymax = cf_upr, fill = "red",
                                     group = .data$name), alpha = 0.5) -> g1
  }


  g1 +

    geom_line(data = effects, aes_(y = ~cf, color = "red", group = ~name), na.rm = TRUE) +

    scale_color_identity(name = NULL,
                         breaks = c("black", "blue", "grey", "purple", "red","darkgreen", "orange"),
                         labels = c("y","Fitted","IIS","SIS","FESIS","CFESIS", "CSIS"),
                         guide = "legend") +

    scale_linetype(name = "Variable") +
    guides(fill = "none",color='none') +
    scale_y_continuous(breaks = pretty_breaks(n=3))+
    xlim(c(1998,max(max(out$time)+2,max(out$max_year),2022)))+
    theme(
      plot.title = element_text(size=25),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.background = element_blank(),
      panel.border = element_rect(colour = "grey",fill = NA),
      panel.background = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.y = element_line(),
      axis.text.y = element_text(size=15, face="bold"),
      axis.title.y = element_text(size=12, face="bold"),
      legend.key = element_rect(fill = NA),
      legend.key.size = unit(2,'cm'),
      legend.text = element_text(size=15),
    ) +

    labs(title = title, subtitle = NULL, y = NULL, x = NULL) -> plotoutput


  out <- list()

  out$plotoutput <- plotoutput

  effects %>%
    mutate(min_year_2y = case_when(!is.na(min_year) ~ time - int_size, TRUE ~ NA),
           max_year_2y = case_when(!is.na(max_year) ~ time + int_size, TRUE ~ NA)) %>%

    select(id, time, y, name, fitted, cf, cf_upr, cf_lwr,cf_upr99, cf_lwr99,
           min_year, max_year, min_year_2y, max_year_2y) -> effects_data
  out$data <- effects_data

  return(out)

}

#produces Fig. 2+3
plot_ts_example_with_policy <- function(country,res,out,policy_match,label_df,cube_size=5,symbol_size=3, ylim=c(0,3),policy_plot_prop=1, sector = "x",int_size=2,legend=TRUE){

  p_output <- plot_counterfactual(res,country,out,int_size=int_size)
  p <- p_output$plotoutput
  effects_data <- p_output$data

  iso = countrycode(country,origin='country.name',destination='iso3c')

  if(country == 'SouthAfrica'){
    iso = 'ZAF'
  }
  if(country == 'SouthKorea'){
    iso = 'KOR'
  }
  policy_match_country <- policy_match[policy_match$ISO==iso,]
  if(sector != "x"){
    policy_match_country = policy_match_country[policy_match_country$Module==sector,]
  }

  policy_match_country <- policy_match_country %>%
    group_by(year) %>%
    mutate(enumeration = row_number()) %>% ungroup

  policy_match_plot <- policy_match_country[c('year','Module','Policy_name_fig_2_3',"enumeration")]


  label_df_sub = label_df[label_df$country == country & label_df$Module==sector,]
  label_df_sub = select(label_df_sub,-c('country'))

  if(nrow(label_df_sub)>0){
    #EU_flags = data.frame(year = unique(label_df_sub$year), indicator = 0.9, icon = "\U1F1EA\U1F1FA")
    EU_flags = data.frame(year = unique(label_df_sub$year), indicator = 0.9, icon = "EU")

    label_df_sub$icon <- case_when(label_df_sub$Policy_name == "EU-MEPS" ~ "\u2699",  # Manufacturing wheel icon
                                   label_df_sub$Policy_name == "EU-Labels" ~ "\U0001f3f7",  # Label icon
                                   label_df_sub$Policy_name == "EU-ETS" ~ "€",  # Euro sign icon
                                   label_df_sub$Policy_name == 'EU' ~ "EU", #EU flag
                                   TRUE ~ "")

    label_df_sub$family <- case_when(label_df_sub$Policy_name == "EU-MEPS" ~ "Noto",  # Manufacturing wheel icon
                                     label_df_sub$Policy_name == "EU-Labels" ~ "Noto",  # Label icon
                                     label_df_sub$Policy_name == "EU-ETS" ~ "Arial",  # Euro sign icon
                                     label_df_sub$Policy_name == 'EU' ~ "Arial", #EU flag
                                     TRUE ~ "")

  }

  ##enumerate the labels in each year s.t. they do not overlap in case of duplications, start at 2 bc we plot eu sign on 1
  label_df_sub <- label_df_sub %>%
    group_by(year) %>%
    mutate(enumeration = row_number()) %>% ungroup
  label_df_sub$enumeration = (label_df_sub$enumeration+1)-0.2


  out_sub = out[out$id == country,]



  p_policy <- ggplot() +
    scale_fill_manual(values = color_dict)+
    ylab('')+
    ylim(ylim)+
    scale_x_continuous(breaks=seq(2000, 2020, 10),limits = c(1998,max(max(out$time)+2,max(out$max_year),2022)))+
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size=15, face="bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(),
      axis.line.y = element_line(),
      axis.ticks.y = element_line(color='white'),
      axis.text.y = element_text(size=15,color='white'),
      axis.title.y = element_blank(),
      legend.key.size = unit(0.2,'cm'),
      legend.text = element_text(size=10),
      legend.title = element_blank(),
      legend.position = 'none')

  if(nrow(policy_match_plot)>0){
    policy_match_plot <- policy_match_plot %>%
      complete(Policy_name_fig_2_3,year = 1998:max(max(out$max_year),2022), fill = list(enumeration = 0)) %>% as.data.frame()
    p_policy <- p_policy +
      geom_rect(data = out_sub, aes(xmin = min_year, xmax = max_year, ymin = -Inf, ymax = Inf),fill = "grey",alpha = 0.5, na.rm = TRUE, show.legend=FALSE) +
      geom_rect(data = out_sub, aes(xmin = time-int_size, xmax = time+int_size, ymin = -Inf, ymax = Inf),fill = "grey",alpha = 0.3, na.rm = TRUE, show.legend=FALSE) +
      geom_point(data = policy_match_country, aes(x=year, y=enumeration, fill=Policy_name_fig_2_3),shape=22,size=cube_size)
  }

  if(nrow(label_df_sub) > 0){
    p_policy <- p_policy+geom_text(data = label_df_sub, aes(x = year, y = enumeration, label = icon, family = family),
                                   hjust = 0, size = symbol_size) +
      geom_text(data = EU_flags, aes(x = year, y = indicator, label = icon),
                hjust = 0, size = symbol_size*0.9)

  }

  if(legend==FALSE){
    p_policy <- p_policy + theme(legend.position="none")
  }
  out <- list()
  p_combined <- cowplot::plot_grid(plotlist=list(p,NULL, p_policy), ncol=1,nrow=3,
                                   rel_heights=c(3,-0.05,policy_plot_prop), align="v",axis='bt')+theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))


  out$p_policy <- p_policy
  out$effects_data <- effects_data

  return(out)
}


##creates legend for Fig. 2 and 3

create_fig_2_3_legend <- function(key_size = 1, font_size = 15, CI_width = 2){

  legend_data_1 <- data.frame('x' = 1:10, 'y' = 1:10, color = 'Model fit ')
  legend_data_2 <- data.frame('x' = 1:10, 'y' = 1:10, color = 'Observed emissions')
  legend_data_3 <- data.frame('x' = 1:10, 'y' = 1:10,lower = 0:9,upper=2:11, color = 'Counterfactual emissions\nand 95% CI')
  legend_data_4 <- data.frame('x' = 1:10, 'y' = 1:10,color = 'Break with 99% CI')

  g_1 <- ggplot(legend_data_1)+geom_line(aes(y=y,x=x,color=color),linewidth=1)+
    geom_line(data = legend_data_2,aes(y=y,x=x,color=color),linewidth=1)+
    scale_color_manual(values=c('blue','black'),name='')+
    theme(legend.title = element_blank(),
          legend.key = element_rect(fill=NA),
          legend.key.size = unit(key_size,'cm'),
          legend.text = element_text(size=font_size))
  l_1 <- get_legend(g_1)
  g_2 <- ggplot(legend_data_3) +
    geom_ribbon(aes(ymin=lower, ymax=upper, x=x, fill=color), alpha = 0.5)+
    geom_line(aes(y=y, x=x,color = color),linewidth=1)+
    theme(legend.title = element_blank(),
          legend.key.size = unit(key_size,'cm'),
          legend.text = element_text(size=font_size))
  l_2 <- get_legend(g_2)
  g_3 <- ggplot(legend_data_4,aes(x=x,y=y,color=color)) +
    geom_rect( aes(xmin = -1, xmax = 2, ymin = -Inf, ymax = Inf,fill = color),color=NA,alpha = 0.8, na.rm = TRUE)+
    geom_vline(aes(xintercept =1,color = color),linewidth=1)+
    scale_color_manual(values=c('red'))+scale_fill_manual(values=c('grey'))+
    theme(legend.title = element_blank(),
          legend.key.size = unit(key_size,'cm'),
          legend.text = element_text(size=font_size))
  l_3 <- get_legend(g_3)
  g_4 <- ggplot()+geom_rect(aes(xmin = -1, xmax = 2, ymin = -Inf, ymax = Inf,fill = '2 year interval'),alpha=0.3)+
    scale_fill_manual(values=c('grey'))+
    theme(legend.title = element_blank(),
          legend.key.size = unit(key_size,'cm'),
          legend.text = element_text(size=font_size),
          plot.margin = unit(c(0,0,0,0),"cm"))
  l_4 <- get_legend(g_4)

  my_legend = cowplot::plot_grid(plotlist = list(l_1,l_2,l_3,l_4),nrow=4,align='hv')+theme(plot.margin = unit(c(0,0,0,0),"cm"))

  return(my_legend)
}

##match across sectors

sector_policy_match <- function(df, spec){
  sector_policy_match = tibble()
  counter=1
  for(s in c('Buildings','Electricity','Industry','Transport')){
    for(sp in spec){
      policy_out_sub = df[df$sector==s,]
      combined_out = rbind(policy_out_sub[1,sp][[1]][[1]],policy_out_sub[2,sp][[1]][[1]])

      spec_tibble = tibble(sector_policy_match = list(combined_out),
                           sector = s,
                           spec = sp)
      sector_policy_match = rbind(sector_policy_match, spec_tibble)

      #sector_policy_match[[counter]] = combined_out
      #counter = counter+1
    }
  }
  return(sector_policy_match)
}

##get effect size means for Fig. 4

get_effect_size_means <- function(out){
  #filter slow increase policies to avoid double counting
  out <- out[out$label!= "slow_increase",]
  #I'm using the Fig 1 policy names here because two different policies in the broader fig 4 taxation category should still count
  out <- out[!duplicated(out[, c("unique_break_identifier", "Policy_name_fig_1")]), ]
  #add a new column which indicate whether the break is single policy or not

  out <- out %>%
    group_by(unique_break_identifier) %>%
    mutate(Count = n()) %>%
    ungroup() %>%
    mutate(SinglePolicy = if_else(Count == 1, 1, 0))

  out$coef_percent <- (exp(out$coeff)-1)*100

  mean_df <- out %>%
    group_by(Policy_name_fig_4, SinglePolicy, Cluster_categories) %>%
    summarize(Average = mean(coef_percent),
              FirstQuartile = quantile(coef_percent, 0.25),
              ThirdQuartile = quantile(coef_percent, 0.75)) %>% as.data.frame()
  return(mean_df)
}

get_effect_size_means_pricing <- function(out){
  #filter slow increase policies to avoid double counting
  out <- out[out$label!= "slow_increase",]
  #I'm using the Fig 1 policy names here because two different policies in the broader fig 4 taxation category should still count
  out <- out[!duplicated(out[, c("unique_break_identifier", "Policy_name_fig_1")]), ]
  #add a new column which indicate whether the break is single policy or not

  out <- out %>%
    group_by(unique_break_identifier) %>%
    mutate(Count = n()) %>%
    ungroup() %>%
    mutate(SinglePolicy = if_else(Count == 1, 1, 0))

  #only keep mixes
  out <- out[out$SinglePolicy == 0,]

  out$coef_percent <- (exp(out$coeff)-1)*100

  #label as pricing mix/no pricing mix

  out <- out %>%
    group_by(unique_break_identifier) %>%
    mutate(Pricing_indicator = ifelse("Pricing" %in% Cluster_categories, 1,0)) %>%
    ungroup()

  mean_df <- out %>%
    group_by(Policy_name_fig_4, Pricing_indicator, Cluster_categories) %>%
    summarize(Average = mean(coef_percent),
              FirstQuartile = quantile(coef_percent, 0.25),
              ThirdQuartile = quantile(coef_percent, 0.75)) %>% as.data.frame()
  return(mean_df)
}

venn_diagram_plot_basic <- function(policy_match, sector, title, shape = 'circle'){

  if(sector == 'Transport' & title == 'Developed economies'){
    shape = 'ellipse'
  }

  policy_no_dups <- policy_match %>% group_by(unique_break_identifier) %>% distinct(Cluster_categories, .keep_all = TRUE)
  policy_no_dups$coeff <- (exp(policy_no_dups$coeff)-1)*100

  colors <- c('#f8f3e8','#f8dbb8','#dfebeb','#ffcbcb')
  names(colors) <- c('Subsidy','Regulation','Pricing','Information')

  sector_colors = c("#EB5600" , "#E7C019","#BAC36B","#3B9AB2")
  names(sector_colors) = c('Buildings','Electricity','Industry','Transport')

  euler_input <- policy_no_dups %>%
    group_by(unique_break_identifier) %>%
    arrange(unique_break_identifier, Cluster_categories) %>%
    summarize(combination = paste0(Cluster_categories, collapse = "&"), .groups = "drop") %>% as.data.frame()

  ##merge in coeffs
  coeff_sub = policy_no_dups[c('unique_break_identifier','coeff')]
  coeff_sub = coeff_sub[!duplicated(coeff_sub),]

  euler_input = merge(euler_input, coeff_sub,by='unique_break_identifier',all.x=TRUE)

  euler_input = euler_input %>% group_by(combination) %>% summarize(n = n(), mean_coeff = mean(coeff))

  euler_input$percent = round((euler_input$n / sum(euler_input$n))*100,1)

  euler_input$percent = paste(euler_input$percent,'%', sep = '')

  euler_input$label = euler_input$percent

  euler_plot <- round(euler_input$n/sum(euler_input$n),2)*100

  names(euler_plot) <- euler_input$combination

  euler_plot <- euler_plot[order(names(euler_plot))]

  #order the colors properly
  #get order in which categories appear
  category_order <- names(euler_plot) %>%
    paste(collapse = " ") %>%
    gsub("&", " ", .) %>%
    strsplit(" ") %>%
    unlist() %>%
    unique()


  colors_plot = colors[category_order]

  fit <- euler(euler_plot,
               shape = shape)

  labels = data.frame(combination = names(fit$fitted.values))
  labels$label = ""
  labels_store = data.frame(combination = names(fit$fitted.values))

  #have to do this by hand because the eulerr package sometimes changes the order of categories in the labels
  for(k in 1:nrow(labels)){
    matching_indices <- which(sapply(euler_input$combination, function(x) identical(sort(unlist(strsplit(labels$combination[k], ""))), sort(unlist(strsplit(x, ""))))))
    if(length(matching_indices)>0){
      labels$label[k] = euler_input$label[matching_indices[1]]
    }
  }


  labels <- labels[match(labels_store$combination, labels$combination), ]

  p <- plot(fit,labels = list(cex=1.7,padding=grid::unit(20, "mm")),quantities = list(labels = labels$label,cex = 2,padding = grid::unit(20, "mm")),fills=colors_plot,adjust_labels = TRUE)

  ##transform into ggplot object to finalize

  p <- cowplot::plot_grid(plotlist=list(p)) + ggtitle(title)+theme(plot.title = element_text(size=40,face = 'bold',margin=margin(0,0,30,0)),plot.margin = unit(c(0,0,0,0), "cm"))


  return(list(p, euler_input))
}

