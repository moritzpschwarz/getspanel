


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


# # # Test scripts
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
#         parallel = FALSE,
#         max_GDP_restriction = TRUE,
#         no_worse_off_restriction = TRUE,
#         no_higher_baseline_restriction = TRUE)
#
#
# project(stdmodel = m2,
#         modelname = "m2",
#         adaptation = FALSE,
#         climate = climate_subset,
#         socioprojections = mueller_df_subset,
#         coefsamples = 1,
#         parallel = FALSE)
#
#
# load(here("data-raw","projections","projfiles", "Mueller_am2_1.RData"))
# done %>%
#   filter(year > 2089) %>%
#
#   mutate(diff = gdp_cap_hundred_climate / gdp_cap_hundred) %>%
#   #drop_na %>%
#
#   group_by(iso,final_temp,realisation) %>%
#
#   summarise(diff = mean(diff),.groups =  "drop") %>%
#   ungroup
# load(here("data-raw","projections","projfiles", "Mueller_m2_1.RData"))
#
# done %>%
#   filter(year > 2089) %>%
#
#   mutate(diff = gdp_cap_hundred_climate / gdp_cap_hundred) %>%
#   #drop_na %>%
#
#   group_by(iso,final_temp,realisation) %>%
#
#   summarise(diff = mean(diff),.groups =  "drop") %>%
#   ungroup


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




