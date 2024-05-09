library(conflicted)
library(tidyverse)
library(glmnet)
conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::first)

load("20240507 Saving Overall intermediate.RData")
source("~/GitHub/getspanel/R/threshold_lasso.R")


# gets_obj <- overall %>%
#   filter(engine == "gets", country_sample == "AC6", grepl("buildings",source)) %>%
#   pull(is) %>% first
#
# gets_coef <- coef(gets_obj$isatpanel.result)[get_indicators(gets_obj)$fesis$name]
# gets_coef_neg <- gets_coef[gets_coef < 0]
# no_neg_breaks <- length(gets_coef_neg)

#
# lasso_obj <- overall %>%
#   filter(engine == "lasso", country_sample == "AC6", grepl("buildings",source), scale_by == "country", !iis, lambda == "min") %>%
#   pull(is) %>% first
#
# plot(lasso_obj$lasso_output$firststage)




overall %>%
  filter(engine == "gets") %>%
  mutate(neg_breaks = map(is, function(x){
    gets_coef <- coef(x$isatpanel.result)[get_indicators(x)$fesis$name]
    gets_coef_neg <- gets_coef[gets_coef < 0]
    length(gets_coef_neg)
  })) %>%
  unnest(neg_breaks) %>%
  select(source, country_sample, neg_breaks) -> neg_breaks_gets

overall %>%
  filter(engine == "lasso", !iis) %>%
  full_join(neg_breaks_gets, by = join_by(source, country_sample)) %>%
  mutate(new_lass_model = pmap(.l = list(is, neg_breaks, row_number()), function(is, neg_breaks, i){
    result <- threshold_lasso(breaks_in_ols_or_lasso = "ols",is, target_neg_breaks = neg_breaks, plot = FALSE)
    result$new_lasso_obj
  })) -> overall_new_lasso




overall_new_lasso %>%
  full_join(overall %>%
              filter(engine == "gets") %>% select(source, country_sample, gets_obj = is)) %>%
  mutate(plt = pmap(.l = list(gets_obj, is, new_lass_model, adaptive, iis, lambda, source, row_number()),
                    function(g,l,l_new, adaptive, iis, lambda, source, i){
                      print(i)
                      sector <- case_when(grepl("buildings", source) ~ "Buildings",
                                          grepl("electricity", source) ~ "Electricity",
                                          grepl("industry", source) ~ "Industry",
                                          grepl("transport", source) ~ "Transport")

                      adaptive <- case_when(adaptive ~ "Adaptive LASSO",
                                            TRUE ~ "LASSO")
                      iis <- case_when(iis ~ "Using IIS",
                                       TRUE ~ "Not Using IIS")

                      gets_coef <- coef(g$isatpanel.result)[get_indicators(g)$fesis$name]
                      l_new_coef <- coef(l_new$isatpanel.result)[get_indicators(l_new)$fesis$name]
                      l_coef <- coef(l$isatpanel.result)[get_indicators(l)$fesis$name]

                      gets_coef_excl <- paste0(names(gets_coef[gets_coef > 0]), collapse = "|")
                      l_new_coef_excl <- paste0(names(l_new_coef[l_new_coef > 0]), collapse = "|")
                      l_coef_excl <- paste0("^iis|",paste0(names(l_coef[l_coef > 0]), collapse = "|"))

                      p <- cowplot::plot_grid(plot_grid(g, regex_exclude_indicators = gets_coef_excl) + labs(title = paste0(sector, "\ngets")),
                                              plot_grid(l_new, regex_exclude_indicators = l_new_coef_excl) + labs(title = "Threshold LASSO", subtitle = "Lambda chosen based on Number of Negative Breaks in gets"),
                                              plot_grid(l, regex_exclude_indicators = l_coef_excl) + labs(title = paste0(adaptive,", ", iis), subtitle = paste0("Lambda Selection: ", lambda), caption = "In all plots only negative breaks shown."),
                                              ncol = 1)
                      ggsave(p,file = paste0(i,"_threshold_lasso.png"), width = 8, height = 9)
                    }))



#
#
#
# # Run model again ---------------------------------------------------------
#
# # Master Runs
# library(tidyverse)
# library(devtools)
# library(gets)
# #library(getspanel)
# devtools::load_all()
# library(here)
# library(doParallel)
# library(readxl)
# library(gdata)
#
# # Country groupings
# setwd("~/GitHub/getspanel/Zenodo_code_oecd_project/Zenodo_code_oecd_project/")
#
# ##load country groups
#
#
# #Developed economies = AC1
# #Developing economies = AC6
#
# AC6 <- read_excel("country_groupings.xlsx", sheet = 2)
#
# AC1 <- read_excel("country_groupings.xlsx") %>% pull(Developed) %>% unlist
# for(i in 1:nrow(AC6)){
#   temp <- AC6 %>% slice(i) %>% pull(countries) %>% strsplit(., ", ") %>% unlist
#   mv(from = "temp", to = paste0("AC6_", AC6$cat_abbrev[i]))
# }
#
# dfi <- readRDS("break_detection_regression_input.RDS")
#
# #exclude countries that don't have a minimum number of emissions
# excl_all <- dfi %>% group_by(country) %>%
#   summarise(excl_test = mean(log_total_emissions_co2)) %>% filter(excl_test < 10) %>% pull(country) %>% unique
#
# df_excl <- dfi %>% filter(!(country %in% excl_all))
#
# AC6 <- df_excl %>% filter(!(country %in% AC1)) %>% select(country)%>% distinct()
# AC6 <- AC6$country
# samples <- mget(c("AC1", "AC6"))
#
#
# #specifcy basic model forms
#
# tot_controls <- c(" ~ lgdp + lpop + lgdp_sq + hdd + cdd")
#
# tot_core_deps <- c("log_buildings_co2",
#                    "log_electricity_heat_co2",
#                    "log_industry_co2",
#                    "log_transport_co2")
#
#
# tot_base_forms <- paste0(rep(tot_core_deps, each = length(tot_controls)), tot_controls)
#
#
# # specify sector-specific controls (including EU dummies as we're looking at developed economies, all EU countries are in this group)
# total_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009 + MEPS_Appliances_2013 + ETS_E_2005 + ETS_E_2018 + ETS_I_2005 + ETS_I_2018 + MEPS_T_2009 + MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015"
#
# buildings_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009 + MEPS_Appliances_2013"
#
# electricity_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + ETS_E_2005 + ETS_E_2018"
#
# industry_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + ETS_I_2005 + ETS_I_2018 + MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015"
#
# transport_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + MEPS_T_2009"
#
#
# eu_control_forms <- c(
#   paste0("log_buildings_co2", buildings_controls),
#   paste0("log_electricity_heat_co2", electricity_controls),
#   paste0("log_industry_co2", industry_controls),
#   paste0("log_transport_co2", transport_controls))
#
# #create an ensemble of models to run
#
# tot_forms <- c(tot_base_forms, eu_control_forms)
#
# # Incorporating linear country-specific time trends
# df_trends <- dfi %>% mutate(country = as.factor(country),
#                             trend = year - 1999,
# )
#
# df_restr_trends <- df_excl %>% mutate(country = as.factor(country),
#                                       trend = year - 1999,
#                                       trend_sq = trend^2)
#
# # Base forms
# tot_forms_trends <- c(paste0(tot_forms, " + country:trend"))
#
# ##run models for developing economies
# rel_forms_trends <- tot_forms_trends[1:4]
#
# f = rel_forms_trends[1]
# scale_by = "country"
# adpt = FALSE
# lambda = lambda_to_use
# iis = FALSE
#
#
# print(paste0("Scale by:",scale_by," Adaptive:",adpt, " Sector:", f, " Lambda:",lambda, " IIS:",iis))
#
#
# set.seed(123)
# dat <- df_restr_trends %>% filter(country %in% samples[["AC6"]])
# if(scale_by == "NULL"){
#   dat <- mutate(dat,across(c(where(is.numeric),-year), ~as.numeric(scale(.))))
# } else {
#   dat <- mutate(dat,across(c(where(is.numeric),-year), ~as.numeric(scale(.))), .by = country)
# }
#
#
# # dat %>%
# #   pivot_longer(-c(country,year)) %>%
# #   ggplot(aes(x = year, y = value, color = name)) +
# #   geom_line() +
# #   facet_wrap(~country)
#
# is <- isatpanel(
#   data = dat,
#   formula = as.formula(f),
#   index = c("country", "year"),
#   effect = "twoways",
#   iis = iis,
#   fesis = TRUE,
#   ar = 0,
#   engine = "lasso",
#   lasso_opts = list(adaptive = adpt, foldid = 0.25, s = "min", scale_by = "id")
# )
