# library(cowplot)
#
# library(ggnewscale)

library(extrafont)
library(ggalt)
library(RColorBrewer)


# Plotting Map ----

overall_df <- read_csv(here("data-raw","projections",paste0("all_models_iso_quantiles_sq.csv")))


overall_df %>%
  mutate(baseline = case_when(baseline=="Mueller"~"MSW",
                              TRUE~baseline)) %>%
  mutate(across(c(where(is.double),-final_temp),~.-1)) %>%
  rename(pc.025 = vlow,
         pc.05 = low,
         pc.25 = midl,
         pc.5 = med,
         pc.75 = midh,
         pc.95 = high,
         pc.975 = vhigh) -> df


df %>%
  mutate(sig.50 = ifelse(sign(pc.75)==sign(pc.25),1,0),
         sig.90 = ifelse(sign(pc.95)==sign(pc.05),1,0),
         sig.95 = ifelse(sign(pc.975)==sign(pc.025),1,0)) -> significance_all


df %>%
  mutate(map_position = round(final_temp),
         map_position = ifelse(final_temp < 1.75,1,map_position)) %>%

  # average over the map position
  group_by(baseline,model,specification,scenario,map_position,iso) %>%
  summarise(across(where(is.numeric),.fns = mean),.groups = "drop") %>%
  mutate(sig.50 = ifelse(sign(pc.75)==sign(pc.25),1,0),
         sig.90 = ifelse(sign(pc.95)==sign(pc.05),1,0),
         sig.95 = ifelse(sign(pc.975)==sign(pc.025),1,0)) %>%
  select(-starts_with("pc."),pc.5) %>%

  # Joining on the iso codes
  mutate(region = countrycode::countrycode(sourcevar = iso,origin = "iso3c",destination = "country.name")) %>%
  relocate(region, .after = iso) %>%
  mutate(region = case_when(region=="Myanmar (Burma)"~"Myanmar",
                            region=="Bosnia & Herzegovina"~"Bosnia and Herzegovina",
                            region=="Congo - Brazzaville"~"Republic of Congo",
                            region=="Congo - Kinshasa"~"Democratic Republic of the Congo",
                            TRUE~region)) -> map_values

map_data("world") %>%
  filter(!region=="Antarctica") %>%
  mutate(region = case_when(region=="USA"~"United States",
                            region=="UK"~"United Kingdom",
                            region=="Czech Republic"~"Czechia",
                            region=="Ivory Coast"~"Côte d’Ivoire",
                            TRUE~region)) -> world_df




# Maps

## Manual: gets and LASSO combined

# Main Figure for PNAS Submission Oct 2020
# <2°C
# 3.5°C
# 4.5°C


df %>%
  mutate(map_position = NA,
         map_position = case_when(final_temp < 2~"Below 2°C",
                                  final_temp>=3&final_temp<4~"3.5°C",
                                  final_temp>=4.5~">4.5°C")) %>%

  # average over the map position
  group_by(baseline,model,specification,scenario,map_position,iso) %>%
  summarise(across(where(is.numeric),.fns = mean),.groups = "drop") %>%
  mutate(sig.50 = ifelse(sign(pc.75)==sign(pc.25),1,0),
         sig.90 = ifelse(sign(pc.95)==sign(pc.05),1,0),
         sig.95 = ifelse(sign(pc.975)==sign(pc.025),1,0)) %>%
  select(-starts_with("pc."),pc.5) %>%

  # Joining on the iso codes
  mutate(region = countrycode::countrycode(sourcevar = iso,origin = "iso3c",destination = "country.name")) %>%
  relocate(region, .after = iso) %>%
  mutate(region = case_when(region=="Myanmar (Burma)"~"Myanmar",
                            region=="Bosnia & Herzegovina"~"Bosnia and Herzegovina",
                            region=="Congo - Brazzaville"~"Republic of Congo",
                            region=="Congo - Kinshasa"~"Democratic Republic of the Congo",
                            TRUE~region)) %>%

  mutate(model = factor(model,
                        levels = c("Standard","Adaptation","AdaptationL1"),
                        labels = c("Standard","Adaptation","Adaptation\nwith Lagged GDP"))) -> LevelData





# GGplot ------------------------------------------------------------------

# default theme
default_ggplot_options <- list(geom_polygon(aes(x=long,y=lat,group=group),fill="grey",inherit.aes = FALSE, data = world_df),
                                 geom_polygon(aes(x=long,y=lat,group=group,fill=pc.5)) ,
                                 coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") ,


                                 #coord_quickmap(),
                                 scale_fill_gradientn(colours = brewer.pal(name = "RdBu",n=11),
                                                      breaks = seq(from = -1, to = 1, by = 0.2),
                                                      guide = guide_colourbar(title = "Level Percentage Difference to baseline",
                                                                              title.position = "top"),
                                                      labels=c(as.character(scales::percent(seq(from = -1, to = 0.8, by = 0.2))),">100%"),
                                                      limits=c(-1,1),
                                                      oob=scales::squish,
                                                      na.value = "grey") ,
                                 labs(x=NULL,y=NULL),

                                 theme(legend.position = "bottom",
                                       panel.background = element_blank(),
                                       strip.background = element_blank(),
                                       panel.border = element_rect(colour = "grey",fill=NA),
                                       text = element_text(family = "Georgia"),
                                       axis.ticks = element_blank(),
                                       axis.text = element_blank(),
                                       legend.key.size = unit(0.25, "cm"),
                                       legend.key.width = unit(1.5,"cm"),
                                       legend.text = element_text(family = "Georgia",size=8)))


LevelData %>%
  ## HERE change from Schwarz and Pretis
  filter(baseline=="MSW",
         specification == "Base") %>%
  # specification=="standard",
  # model %in% c("LASSO","gets"))

  full_join(world_df, by = "region") %>%
  # if 90% not significant, then NA
  mutate(pc.5 = ifelse(sig.90 != 1, NA, pc.5)) -> Base_level_data



Base_level_data %>%
  filter(!is.na(map_position)) %>%
  mutate(map_position = factor(map_position,levels = c("Below 2°C","3.5°C",">4.5°C"))) %>%
  ggplot() +
  facet_grid(map_position~model)+
  default_ggplot_options -> plot

ggsave(plot, filename = here("data-raw/projections/Map_Base_Level.pdf"),device = cairo_pdf,height = 6,width = 8)


# Isat Level --------------------------------------------------------------



LevelData %>%
  ## HERE change from Schwarz and Pretis
  filter(baseline=="MSW",
         specification == "IIS") %>%
  # specification=="standard",
  # model %in% c("LASSO","gets"))

  full_join(world_df, by = "region") %>%
  # if 90% not significant, then NA
  mutate(pc.5 = ifelse(sig.90 != 1, NA, pc.5)) -> IIS_level_Data



IIS_level_Data %>%
  filter(!is.na(map_position)) %>%
  mutate(map_position = factor(map_position,levels = c("Below 2°C","3.5°C",">4.5°C"))) %>%
  ggplot() +
  facet_grid(map_position~model)+
  default_ggplot_options -> plot

ggsave(plot, filename = here("data-raw/projections/Map_IIS_Level.pdf"),device = cairo_pdf,height = 6,width = 8)




# Difference --------------------------------------------------------------

LevelData %>%
  #select(model, specification, iso, region, map_position, pc.5) %>%
  #pivot_wider(id_cols = c(model, iso, region, map_position), names_from = specification, values_from = pc.5) %>%
  pivot_wider(id_cols = c(everything(), -contains("sig"), sig.90,-pc.5, -specification), names_from = specification, values_from = pc.5) %>%
  mutate(pc.5 = IIS - Base) %>% View


  full_join(world_df, by = "region") %>%
  # if 90% not significant, then NA
  mutate(pc.5 = ifelse(sig.90 != 1, NA, pc.5)) -> IISBase_diff_Data



IISBase_diff_Data %>%
  filter(!is.na(map_position)) %>%
  mutate(map_position = factor(map_position,levels = c("Below 2°C","3.5°C",">4.5°C"))) %>%
  ggplot() +
  facet_grid(map_position~model)+
  default_ggplot_options +
  labs(title = "Country-level differences between Base and IIS",
       subtitle = "Large Changes in Northern Hemisphere countries due to high projected increase of GDP.")-> plot

ggsave(plot, filename = here("data-raw/projections/Map_IISBase_diff.pdf"),device = cairo_pdf,height = 6,width = 8)

