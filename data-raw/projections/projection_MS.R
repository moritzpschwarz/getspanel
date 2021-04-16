


library(tidyverse)
# library(data.table)
library(gets)
library(here)
library(vroom)
library(MASS)

rm(list = ls())
select <- dplyr::select
here <- here::here


# Load Mueller et al ------------------------------------------------------

mueller <- readxl::read_excel(here("data-raw","projections","Predictive_ Distributions_by_Country.xlsx"),
                              skip = 6,
                              col_names = c("iso","empty1","value_2017","empty2","fifty_mean","fifty_0.05",
                                            "fifty_0.16","fifty_0.5","fifty_0.84","fifty_0.95",
                                            "empty3","hundred_mean","hundred_0.05","hundred_0.16","hundred_0.5","hundred_0.84",
                                            "hundred_0.95", "empty4","unknown")) %>% select(-contains("empty"))

mueller_mean <- mueller %>%
  select(iso, value_2017, contains("mean"))

mueller_mean %>%
  mutate(year = 2017,
         gdp_cap = exp(value_2017),
         gdp_cap_fifty = gdp_cap,
         gdp_cap_fifty_climate = gdp_cap_fifty,
         gdp_cap_hundred = gdp_cap,
         gdp_cap_hundred_climate = gdp_cap_hundred,
         fifty_mean = (fifty_mean/100)+1,
         hundred_mean = (hundred_mean/100)+1,
         value_2017 = NULL,
         gdp_cap = NULL) %>%
  select(iso,year,gdp_cap_fifty, gdp_cap_hundred,everything()) -> mueller_df


source(here("data-raw/projections/projection_functions.R"))

climate_path <- here("data-raw","projections","corrected_anomaly_climatedata.csv")
climate <- vroom(climate_path)

# Load model files
load(here("data-raw/projections/m2.RData"))
load(here("data-raw/projections/am2.RData"))
load(here("data-raw/projections/m2.isat.RData"))
load(here("data-raw/projections/am2.isat.RData"))
load(here("data-raw/projections/am2_L1.RData"))
load(here("data-raw/projections/am2.isat_L1.RData"))


# # Test
# final_temp_selection <- climate %>% distinct(final_temp) %>% slice(1,100, n()) %>% pull(final_temp)
# iso_selection <- c("AUT","IND","FIN", "USA")
#
# climate_subset <- climate %>% filter(final_temp %in% final_temp_selection & iso %in% iso_selection)
# mueller_df_subset <- mueller_df %>% filter(iso %in% iso_selection)
#
# project(stdmodel = m2,
#         adaptmodel = am2,
#         modelname = "am2",
#         adaptation = TRUE,
#         climate = climate_subset,
#         socioprojections = mueller_df_subset,
#         coefsamples = 1,
#         parallel = FALSE)


# Projections

project(stdmodel = m2,
        modelname = "m2",
        adaptation = FALSE,
        climate = climate,
        socioprojections = mueller_df,
        seed = 123)

project(stdmodel = m2.isat,
        modelname = "m2.isat",
        adaptation = FALSE,
        climate = climate,
        socioprojections = mueller_df,
        seed = 123)

# Adaptation

project(stdmodel = m2,
        adaptmodel = am2,
        modelname = "am2",
        adaptation = TRUE,
        climate = climate,
        socioprojections = mueller_df,
        seed = 123)

project(stdmodel = m2.isat,
        adaptmodel = am2.isat,
        modelname = "am2.isat",
        adaptation = TRUE,
        climate = climate,
        socioprojections = mueller_df,
        seed = 123)

# Adaptation Lagged GDP

project(stdmodel = m2,
        adaptmodel = am2_L1,
        modelname = "am2_L1",
        adaptation = TRUE,
        climate = climate,
        socioprojections = mueller_df,
        seed = 123)

project(stdmodel = m2.isat,
        adaptmodel = am2.isat_L1,
        modelname = "am2.isat_L1",
        adaptation = TRUE,
        climate = climate,
        socioprojections = mueller_df,
        seed = 123)

# combine -----------------------------------------------------------------


rm(list = ls())
select <- dplyr::select


socio <- c("Mueller")
model<- c(
  "m2",
  "m2.isat",
  "am2",
  "am2.isat",
  "am2.isat_L1",
  "am2_L1",
  NULL
)


for(i in socio){
  for(j in model){
    #i = "Mueller"
    #j = "m2"

    print(paste("full",i,j,sep="_"))
    #print(length(list.files(here("data","temp","projections"),pattern = paste("full",i,j,sep="_"))))

    # Carry out the merging of the files
    indv_files <- list.files(here("data-raw","projections","projfiles"),pattern = paste(i,j,sep="_"),full.names = TRUE)
    indv_files <- indv_files[!grepl("massive",indv_files)]

    if(j == "m2"){indv_files <- indv_files[!grepl("m2.isat",indv_files)]}
    if(j == "m2"){indv_files <- indv_files[!grepl("am2",indv_files)]}
    if(j == "am2"){indv_files <- indv_files[!grepl("am2.isat|am2_L1",indv_files)]}
    if(j == "am2.isat"){indv_files <- indv_files[!grepl("am2.isat_L1",indv_files)]}

    massive_overall <- tibble()
    for(k in seq_along(indv_files)){
      print(k)
      load(indv_files[k])

      done %>%
        filter(year > 2089) %>%

        mutate(diff = gdp_cap_hundred_climate / gdp_cap_hundred) %>%
        #drop_na %>%

        group_by(iso,final_temp,realisation) %>%

        summarise(diff = mean(diff),.groups =  "drop") %>%
        ungroup %>%
        bind_rows(massive_overall,.) -> massive_overall

      rm(done)
    }
    save(massive_overall, file=here("data-raw","projections","projfiles",paste0(i,"_",j,"_massive_EOC.RData")))
  }
}




# Quantiles ---------------------------------------------------------------


library(tidyverse)
# library(data.table)
library(gets)
library(here)
library(vroom)
# library(MASS)
library(quantreg)

rm(list = ls())
select <- dplyr::select
here <- here::here

type = "sq"
files  <- list.files(here("data-raw","projections","projfiles"),pattern = "massive_EOC", full.names = T)
files <- grep("_L1",files,value=T,ignore.case = FALSE, invert = TRUE)
#files <- grep("revision|altrestr1",files,value=T,ignore.case = FALSE)
#files <- grep("Mueller",files,value=T,ignore.case = FALSE)

overall_df <- tibble()
for(i in 1:length(files)){
  print(gsub("_massive_EOC", "",gsub(".RData", "", gsub(paste0(here("data-raw","projections","projfiles"),"/"),"",files[i]), fixed = T)))

  load(files[i])

  tibble(name = gsub("_EOC", "",
                     gsub(".RData", "", gsub(paste0(here("data-raw","projections","projfiles"),"/"),"",files[i]), fixed = T))) %>%
    mutate(name = case_when(name=="Mueller_m2_massive"~"Mueller_Standard_Base",
                            name=="Mueller_m2.isat_massive"~"Mueller_Standard_IIS",
                            name=="Mueller_am2_massive"~"Mueller_Adaptation_Base",
                            name=="Mueller_am2.isat_massive"~"Mueller_Adaptation_IIS",
                            name=="Mueller_am2_L1_massive"~"Mueller_AdaptationL1_Base",
                            name=="Mueller_am2.isat_L1_massive"~"Mueller_AdaptationL1_IIS",
                            TRUE~name)) %>%
    separate(name, sep = "_", into = c("baseline", "model","spec"),fill = "right") -> name_df

  baseline <- name_df$baseline
  model <- name_df$model
  specification <- name_df$spec


  success <- FALSE

  massive_overall %>%
    distinct(final_temp)  -> temperature_axis


  while (!success) {
    try({

      form <- "diff ~ final_temp"
      if(type=="sq"){form <- paste0(form,"+ I(final_temp*final_temp)")}
      if(type=="cub"){form <- paste0(form," + I(final_temp*final_temp) + I(final_temp*final_temp*final_temp)")}
      form <- as.formula(form)

      vlow <- rq(formula = form, data = massive_overall,method = "pfn",tau = 0.025)
      low <- rq(formula = form, data = massive_overall,method = "pfn",tau = 0.05)
      midl <- rq(formula = form, data = massive_overall,method = "pfn",tau = 0.25)
      med <- rq(formula = form, data = massive_overall,method = "pfn",tau = 0.5)
      midh <- rq(formula = form, data = massive_overall,method = "pfn",tau = 0.75)
      high <- rq(formula = form, data = massive_overall,method = "pfn",tau = 0.95)
      vhigh <- rq(formula = form, data = massive_overall,method = "pfn",tau = 0.975)


      tibble(
        final_temp = temperature_axis$final_temp,
        baseline = baseline,
        model = model,
        specification = specification,
        scenario = "Hundred",
        vlow  = predict(vlow, newdata = temperature_axis),
        low  =  predict(low, newdata = temperature_axis),
        midl  = predict(midl, newdata = temperature_axis),
        med  =  predict(med, newdata = temperature_axis),
        midh  = predict(midh, newdata = temperature_axis),
        high  = predict(high, newdata = temperature_axis),
        vhigh = predict(vhigh, newdata = temperature_axis)
      ) %>%
        bind_rows(overall_df, .) -> overall_df

      success <- TRUE

    },silent = FALSE)
  }


}



overall_df %>% arrange(baseline,model,specification,scenario,final_temp) -> overall_df

write_csv(overall_df,here("data-raw","projections",paste0("all_models_quantiles_",type,".csv")))



# Plotting ----------------------------------------------------------------


overall_df <- read_csv(here("data-raw","projections",paste0("all_models_quantiles_sq.csv")))


overall_df %>%
  mutate(across(c(vlow, low, midl, med, midh, high, vhigh),~.-1)) %>%
  ggplot(aes(x = final_temp, color = specification, fill = specification, group = paste0(specification, model))) +

  geom_ribbon(aes(ymin = midl, ymax = midh),alpha = 0.3) +
  geom_ribbon(aes(ymin = low, ymax = high),alpha = 0.3) +
  geom_line(aes(y  = med), size = 1) +
  geom_hline(aes(yintercept = 0), size = 1)+
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = function(x){paste0(x,"°C")})+
  theme_minimal() +
  #facet_wrap(~model)+
  coord_cartesian(ylim = c(-1,1))+
  labs(x = "Temperature Anomaly", y = "% Change to Baseline") +
  ggsave(here("data-raw/projections/out/projections.pdf"), height = 6, width = 8)


# Plotting Map ----

overall_df <- read_csv(here("data-raw","projections",paste0("all_models_quantiles_sq.csv")))


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

world_df %>%
  full_join(df %>%
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
                                        TRUE~region))  %>%

              ## HERE change from Schwarz and Pretis
              filter(baseline=="MSW",
                     specification=="standard",
                     model %in% c("LASSO","gets")),by="region") %>%
  # if 90% not significant, then NA
  mutate(pc.5 = ifelse(sig.90 != 1, NA, pc.5)) -> intermed

intermed %>%
  filter(!is.na(map_position)) %>%
  mutate(map_position = factor(map_position,levels = c("Below 2°C","3.5°C",">4.5°C"))) %>%
  ggplot() +
  facet_grid(map_position~model)+
  geom_polygon(aes(x=long,y=lat,group=group),fill="grey",inherit.aes = FALSE,data = world_df)+
  geom_polygon(aes(x=long,y=lat,group=group,fill=pc.5)) +
  coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +


  #coord_quickmap()+
  scale_fill_gradientn(colours = brewer.pal(name = "RdBu",n=11),
                       breaks = seq(from = -1, to = 1, by = 0.2),
                       guide = guide_colourbar(title = "Level Percentage Difference to baseline",
                                               title.position = "top"),
                       labels=c(as.character(scales::percent(seq(from = -1, to = 0.8, by = 0.2))),">100%"),
                       limits=c(-1,1),
                       oob=scales::squish,
                       na.value = "grey") +
  labs(x=NULL,y=NULL)+

  theme(legend.position = "bottom",
        panel.background = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "grey",fill=NA),
        text = element_text(family = "Georgia"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.key.size = unit(0.25, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text = element_text(family = "Georgia",size=8)) +
  #ggsave(filename = here("output","figures","Map_MSW_getsLASSO_standard.jpg"),height = 6,width = 6) +
  NULL



