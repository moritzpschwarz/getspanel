library(showtext)
library(ggimage)

font_add("Noto", "C:/Users/morit/Downloads/Noto_Emoji/NotoEmoji-VariableFont_wght.ttf")
font_add("Arial", "Arial.ttf")
showtext_auto()


### Plotting for Fig. 2 + Fig. 3

setwd("~/Github/getspanel/plotting icons")
#setwd("C:\\Users\\stecheme.PIKACCOUNTS\\Documents\\Break_detection\\OECD_project_code_first_submission\\Public_code_zenodo\\")

source('00_oecd_project_functions_adapatedWSJ.R')
conflicts_prefer(ggpubr::get_legend)

policy_out <- readRDS("Policy_out.RDS")
oecd_grouped = read.csv('OECD_data_preprocessed_august_23.csv')

#set the color palette for the policies
palette <- c("#e6194b","#f58231","#f032e6","#991eb4","#ffe119","#bfef45","#3cb44b","#4363d8","#fabed4","#42d4f4","#ffd8b1","#fffac8","#aaffc3","#dcbeff","#800000","#9a6324","#808000","#000075","#469990","#000000","#a9a9a9","tan","aquamarine")
names(palette) <- sort(unique(oecd_grouped$Policy_name_fig_2_3))

color_dict = palette

#load the label_df to visualize years with EU policies we control for

label_df <- read.csv('EU_policies_label_df.csv')

## make panels for each sector
sector_plots = list()
ncol = c(6,3,3,6)
box_size = c(3,4,3,3)
ylims = list(c(0.5,3.1),c(0.2,3.5),c(0.5,3),c(0.3,3.5))
prop = c(0.65,1.05,0.9,0.6)
icon_links = c("Logos\\Buildings.png","Logos\\Electricity.png","Logos\\Industry.png","Logos\\Transport.png")
i=1

overall_data <- tibble()
overall_policies <- tibble()
for(s in unique(policy_out$sector)){

  if(s == 'Buildings'){
    next
  }

  #s = unique(policy_out$sector)[1]
  policy_out_sub = policy_out[policy_out$sector == s,]
  out = rbind(policy_out_sub$out[[1]],policy_out_sub$out[[2]])
  #out <- out[out$id %in% c("UnitedKingdom","China","UnitedStates"),]

  policy_match = oecd_grouped
  myplots = list()
  myplots_core = list()
  counter = 1
  #myplots[[counter]] = logo
  countries = unique(out$id)
  hi_countries = unique(policy_out_sub$out[[1]]$id)
  li_countries = unique(policy_out_sub$out[[2]]$id)

  for(c in countries){
    if(c %in% hi_countries){
      res = policy_out_sub[1,]$is[[1]]
    }else{
      res = policy_out_sub[2,]$is[[1]]
    }
    #see oecd project functions for this function
    p_out <- plot_ts_example_with_policy(c,res,out,policy_match,sector = s,label_df = label_df,ylim = ylims[[i]], symbol_size = 4,cube_size = box_size[i],policy_plot_prop = prop[i])
    myplots[[counter]] <- p_out$p_combined

    overall_data <- bind_rows(overall_data,
                              bind_rows(ggplot2::ggplot_build(p_out$p)$data) %>%
                                mutate(country = c, .before = colour))

    overall_policies <- bind_rows(overall_policies,
                                  bind_rows(ggplot2::ggplot_build(p_out$p_policy)$data) %>%
                                    mutate(country = c, .before = colour))
    counter = counter+1
  }

  sector_policies = data.frame(sector_policies = oecd_grouped[oecd_grouped$Module == s,c('Policy_name_fig_2_3')])
  sector_policies = sector_policies[!duplicated(sector_policies),]
  sector_policies = data.frame(Policy_name_fig_2_3 = sector_policies)
  p_legend <- ggplot(sector_policies,aes(x=Policy_name_fig_2_3,fill=Policy_name_fig_2_3))+
    geom_bar(color='black',size = 0.02)+
    scale_fill_manual('',values = color_dict)+
    theme(legend.key.size = unit(0.7, 'cm'),
          legend.title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.margin=margin(l = 3, unit='cm'),
          rect = element_rect(fill = "transparent"),
          legend.position = 'bottom')
  legend <- ggpubr::get_legend(p_legend)

  legend_1 <- create_fig_2_3_legend()

  if(i<3){
    myplots[[counter]]<-legend_1
  }

  p <- cowplot::plot_grid(plotlist=myplots,ncol=2,rel_widths = c(1,0.15))
  p_final <- cowplot::plot_grid(plotlist = list(p,legend),nrow=2, rel_heights = c(1,0.15))
  sector_plots[[i]] <- p_final
  i=i+1
}

# modifying overall data
overall_data %>%
  select(-flipped_aes, -PANEL, -linetype) %>%
  relocate(x, .before = y) %>%
  mutate(type = case_when(fill == "grey" & alpha == 0.5 ~ "Uncertainty Around Break Timing (Estimated)",
                          fill == "grey" & alpha == 0.3 ~ "Uncertainty Around Break Timing (2yr)",
                          colour == "black" ~ "Observed Data",
                          colour == "blue" ~ "Fitted Values",
                          colour == "red" & !is.na(xintercept) ~ "Break Time",
                          colour == "red" ~ "Policy Effect",
                          is.na(colour) & fill == "#F8766D" ~ "Policy Effect Uncertainty") ,.before = colour) %>%

  mutate(ymin = case_when(fill == "grey" & ymin == -Inf ~ 0,
                          TRUE ~ ymin)) %>%

  relocate(colour, fill, .after = y) %>%
  rename(linecolour = colour, areafill = fill) %>%
  mutate(country = case_when(country == "UnitedStates" ~ "United States",
                             country == "UnitedKingdom" ~ "United Kingdom",
                             TRUE ~ country)) -> overall_data_fin





overall_data_fin %>%
  select(-linecolour, -areafill, -linewidth,-alpha, -group) %>%
  mutate(y = case_when(type == "Policy Effect Uncertainty" ~ NA,
                       TRUE ~ y)) %>%

  #filter(country == "United Kingdom" & x %in% c(2015,2016,2017) & type == "Policy Effect Uncertainty") %>%
  #rowwise %>%
  mutate(break.id = case_when(country == "United Kingdom" &
                                x %in% c(2014, 2015,2016,2017) &
                                round(ymin,5) %in% round(c(11.92662,12.0836811856302, 11.7951584565687, 11.7683041760083),5) ~ "UK.2015",
                              country == "United Kingdom" &
                                x %in% c(2015,2016,2017) &
                                round(ymin,5) %in% round(c(11.7190153523294, 11.861134832519, 11.8342805519586),5) ~ "UK.2016",
                              !is.na(xintercept) & country == "United Kingdom" ~ paste0("UK.",xintercept),
                              !is.na(ymin) ~ country)) %>%

  pivot_longer(-c(country, type, x, break.id)) %>%
  drop_na(value) %>%
  mutate(break.id = case_when(!name %in% c("y") ~ paste0(break.id, "_", name),
                              TRUE ~ break.id)) %>%
  pivot_wider(id_cols = c(country, x, break.id), names_from = type, values_from = value)  %>% View


  pivot_wider(id_cols = c(country, x, break.id), names_from = name, values_from = value)

pivot_wider(id_cols = c(country, group, x, alpha), names_from = name, values_from = value)



writexl::write_xlsx(overall_data_fin, path = "Data for WSJ.xlsx")


dictionary <- tibble(name = names(color_dict),
                     value = color_dict)


overall_policies %>%

  filter(!(fill == "grey" & alpha == 0.5),
         !(fill == "grey" & alpha == 0.3)) %>%

  relocate(country, x,y) %>%
  select(country, x, fill) %>%

  rename(areafill = fill) %>%

  left_join(dictionary, by = c("areafill" = "value")) %>%

  mutate(country = case_when(country == "UnitedStates" ~ "United States",
                             country == "UnitedKingdom" ~ "United Kingdom",
                             TRUE ~ country)) -> overall_policies_fin


write_csv(overall_policies_fin, file = "Policy Data for WSJ policies.csv")
