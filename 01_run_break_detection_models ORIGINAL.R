# Master Runs
library(tidyverse)
library(devtools)
library(gets)
library(getspanel)
library(here)
library(doParallel)
library(readxl)
library(gdata)

# Country groupings
setwd("C:\\Users\\stecheme.PIKACCOUNTS\\Documents\\Break_detection\\OECD_project_code_first_submission\\Public_code_zenodo\\")

##load country groups 


#Developed economies = AC1
#Developing economies = AC6

AC6 <- read_excel("C:\\Users\\stecheme.PIKACCOUNTS\\Documents\\Break_detection\\OECD_project\\country_groupings.xlsx", sheet = 2)

AC1 <- read_excel("country_groupings.xlsx") %>% pull(Developed) %>% unlist
for(i in 1:nrow(AC6)){
  temp <- AC6 %>% slice(i) %>% pull(countries) %>% strsplit(., ", ") %>% unlist
  mv(from = "temp", to = paste0("AC6_", AC6$cat_abbrev[i]))
}

dfi <- readRDS("break_detection_regression_input.RDS")  

#exclude countries that don't have a minimum number of emissions 
excl_all <- dfi %>% group_by(country) %>% 
  summarise(excl_test = mean(log_total_emissions_co2)) %>% filter(excl_test < 10) %>% pull(country) %>% unique

df_excl <- dfi %>% filter(!(country %in% excl_all))

AC6 <- df_excl %>% filter(!(country %in% AC1)) %>% select(country)%>% distinct()
AC6 <- AC6$country
samples <- mget(c("AC1", "AC6"))


#specifcy basic model forms

tot_controls <- c(" ~ lgdp + lpop + lgdp_sq + hdd + cdd")

tot_core_deps <- c("log_buildings_co2",
                   "log_electricity_heat_co2", 
                   "log_industry_co2",
                   "log_transport_co2")
                  

tot_base_forms <- paste0(rep(tot_core_deps, each = length(tot_controls)), tot_controls)


# specify sector-specific controls (including EU dummies as we're looking at developed economies, all EU countries are in this group)
total_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009 + MEPS_Appliances_2013 + ETS_E_2005 + ETS_E_2018 + ETS_I_2005 + ETS_I_2018 + MEPS_T_2009 + MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015"

buildings_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009 + MEPS_Appliances_2013"

electricity_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + ETS_E_2005 + ETS_E_2018"

industry_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + ETS_I_2005 + ETS_I_2018 + MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015"

transport_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + MEPS_T_2009"


eu_control_forms <- c(
  paste0("log_buildings_co2", buildings_controls),
  paste0("log_electricity_heat_co2", electricity_controls), 
  paste0("log_industry_co2", industry_controls),
  paste0("log_transport_co2", transport_controls))

#create an ensemble of models to run

tot_forms <- c(tot_base_forms, eu_control_forms)

# Incorporating linear country-specific time trends
df_trends <- dfi %>% mutate(country = as.factor(country),
                             trend = year - 1999,
              )

df_restr_trends <- df_excl %>% mutate(country = as.factor(country),
                                      trend = year - 1999,
                                      trend_sq = trend^2)

# Base forms
tot_forms_trends <- c(paste0(tot_forms, " + country:trend"))


##run models for developing economies
rel_forms_trends <- tot_forms_trends[1:4]


#run models with getspanel

results_developing <- foreach(f = rel_forms_trends, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%
  foreach(ii = c(FALSE), .combine = rbind) %:%
  foreach(smpl = c("AC6"), .combine = rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind) %dopar% {
    dat <- df_restr_trends %>% filter(country %in% samples[[smpl]])
    is <- isatpanel(
      data = dat,
      formula = as.formula(f),
      index = c("country", "year"),
      effect = "twoways",
      iis = ii,
      fesis = TRUE,
      ar = 0,
      t.pval = p.value,
      max.block.size = 20
    )
    models = tibble(source = f, 
                    country_sample = smpl, 
                    year_range = paste0(min(dat$year),":",max(dat$year)), 
                    p_val = p.value, 
                    is = list(is),
                    iis = ii,
                    b_size = 20,
                    ar = 0)
  }

### run for developed economies

#only keep final models
rel_forms_trends <- tot_forms_trends[5:8]


#run models with getspanel

results_developed <- foreach(f = rel_forms_trends, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%
  foreach(ii = c(FALSE), .combine = rbind) %:%
  foreach(smpl = c("AC1"), .combine = rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind) %dopar% {
    dat <- df_restr_trends %>% filter(country %in% samples[[smpl]])
    is <- isatpanel(
      data = dat,
      formula = as.formula(f),
      index = c("country", "year"),
      effect = "twoways",
      iis = ii,
      fesis = TRUE,
      ar = 0,
      t.pval = p.value,
      max.block.size = 20
    )
    models = tibble(source = f, 
                    country_sample = smpl, 
                    year_range = paste0(min(dat$year),":",max(dat$year)), 
                    p_val = p.value, 
                    is = list(is),
                    iis = ii,
                    b_size = 20,
                    ar = 0)
  }

print(nrow(full_trends))

##combine results
results = rbind(results_developed, results_developing)

#save isatpanel objects and metadata
saveRDS(results,"Break_detection_results.RDS")

