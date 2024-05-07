# Master Runs
library(tidyverse)
library(devtools)
library(gets)
#library(getspanel)
devtools::load_all()
library(here)
library(doParallel)
library(readxl)
library(gdata)

# Country groupings
setwd("~/GitHub/getspanel/Zenodo_code_oecd_project/Zenodo_code_oecd_project/")

##load country groups


#Developed economies = AC1
#Developing economies = AC6

AC6 <- read_excel("country_groupings.xlsx", sheet = 2)

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



### run for developing economies LASSO ----------

overall <- tibble()
for(lambda in c("min","BIC")){
  for(scale_by in c("country")){
    for(adpt in c(TRUE)){
      for(iis in c(TRUE, FALSE)){

        # ##run models for developing economies
        # rel_forms_trends <- tot_forms_trends[1:4]
        #
        # for(f in rel_forms_trends){
        #
        #   print(paste0("Scale by:",scale_by," Adaptive:",adpt, " Sector:", f, " Lambda:",lambda, " IIS:",iis))
        #
        #
        #   set.seed(123)
        #   dat <- df_restr_trends %>% filter(country %in% samples[["AC6"]])
        #   if(scale_by == "NULL"){
        #     dat <- mutate(dat,across(c(where(is.numeric),-year), ~as.numeric(scale(.))))
        #   } else {
        #     dat <- mutate(dat,across(c(where(is.numeric),-year), ~as.numeric(scale(.))), .by = country)
        #   }
        #
        #
        #   # dat %>%
        #   #   pivot_longer(-c(country,year)) %>%
        #   #   ggplot(aes(x = year, y = value, color = name)) +
        #   #   geom_line() +
        #   #   facet_wrap(~country)
        #
        #   is <- isatpanel(
        #     data = dat,
        #     formula = as.formula(f),
        #     index = c("country", "year"),
        #     effect = "twoways",
        #     iis = iis,
        #     fesis = TRUE,
        #     ar = 0,
        #     engine = "lasso",
        #     lasso_opts = list(adaptive = adpt, foldid = 0.25, s = lambda)
        #   )
        #   tibble(source = f,
        #          country_sample = "AC6",
        #          year_range = paste0(min(dat$year),":",max(dat$year)),
        #          is = list(is),
        #          b_size = 20,
        #          engine = "lasso",
        #          iis = iis,
        #          adaptive = adpt,
        #          scale_by = scale_by,
        #          lambda = lambda,
        #          ar = 0) %>%
        #     bind_rows(overall,.) -> overall
        # }

        ### run for developed economies LASSO ----------

        ### run for developed economies
        rel_forms_trends <- tot_forms_trends[5:8]

        for(f in rel_forms_trends){
          dat <- df_restr_trends %>% filter(country %in% samples[["AC1"]])
          if(scale_by == "NULL"){
            dat <- mutate(dat,across(c(where(is.numeric),-year), ~as.numeric(scale(.))))
          } else {
            dat <- mutate(dat,across(c(where(is.numeric),-year), ~as.numeric(scale(.))), .by = country)
          }
          set.seed(123)
          is <- isatpanel(
            data = dat,
            formula = as.formula(f),
            index = c("country", "year"),
            effect = "twoways",
            iis = iis,
            fesis = TRUE,
            ar = 0,
            engine = "lasso",
            lasso_opts = list(adaptive = adpt, foldid = 0.25, s = lambda)
          )
          tibble(source = f,
                 country_sample = "AC1",
                 year_range = paste0(min(dat$year),":",max(dat$year)),
                 is = list(is),
                 adaptive = adpt,
                 b_size = 20,
                 iis = iis,
                 engine = "lasso",
                 scale_by = scale_by,
                 lambda = lambda,
                 ar = 0) %>% bind_rows(overall, .) -> overall
        }
      }
    }
  }
}

### run for developing economies gets ----------

##run models for developing economies
rel_forms_trends <- tot_forms_trends[1:4]

for(f in rel_forms_trends){
  dat <- df_restr_trends %>% filter(country %in% samples[["AC6"]])
  is <- isatpanel(
    data = dat,
    formula = as.formula(f),
    index = c("country", "year"),
    effect = "twoways",
    iis = FALSE,
    fesis = TRUE,
    ar = 0,
    t.pval = 0.01,
    max.block.size = 20
  )
  tibble(source = f,
         country_sample = "AC6",
         year_range = paste0(min(dat$year),":",max(dat$year)),
         p_val = 0.01,
         is = list(is),
         iis = FALSE,
         b_size = 20,
         engine = "gets",
         ar = 0) %>% bind_rows(overall,.) -> overall
}

### run for developed economies gets --------

### run for developed economies
rel_forms_trends <- tot_forms_trends[5:8]
for(f in rel_forms_trends){
  dat <- df_restr_trends %>% filter(country %in% samples[["AC1"]])
  is <- isatpanel(
    data = dat,
    formula = as.formula(f),
    index = c("country", "year"),
    effect = "twoways",
    iis = FALSE,
    fesis = TRUE,
    ar = 0,
    t.pval = 0.01,
    max.block.size = 20
  )
  tibble(source = f,
         country_sample = "AC1",
         year_range = paste0(min(dat$year),":",max(dat$year)),
         p_val = 0.01,
         is = list(is),
         iis = FALSE,
         b_size = 20,
         engine = "gets",
         ar = 0) %>%
    bind_rows(overall, .) -> overall
}

overall_bic <- overall
#save(overall, file = "Saving Overall intermediate.RData")
load("Saving Overall intermediate.RData")

overall %>%
  filter(engine == "gets") %>%
  bind_rows(overall_bic) -> overall


for(src in c("buildings","transport","electricity","industry")){
  for(ctry in c("AC6","AC1")){

    overall %>%
      filter(grepl(src,source),
             country_sample == ctry) %>%
      mutate(plts = pmap(.l = list(is, adaptive, scale_by, lambda), .f = function(x,y,z,lambda){
        titl <- paste0(case_when(y~"Adaptive - ",
                                 !y~"Not Adaptive - ",
                                 is.na(y)~"gets - "), src)

        sub_titl <- paste0(case_when(is.na(y) ~ "",
                                     z == "NULL"~"\nScaled across sample.",
                                     z == "country"~"\nScaled within countries."))

        if(!is.na(lambda) & lambda == "BIC"){sub_titl <- paste0(sub_titl,"\nSelected with BIC")}

        plot_grid(x, title = paste0(titl,sub_titl))
      })) %>%
      pull(plts) -> plot_list

    cowplot::plot_grid(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                       plot_list[[5]], plot_list[[6]], plot_list[[7]], ncol = 1) -> p
    ggsave(p, file = paste0(ctry,"_",src,".png"), width = 8, height = 15, bg = "white")
  }
}




for(src in c("buildings","transport","electricity","industry")){
  for(ctry in c("AC6","AC1")){

    overall %>%
      filter(grepl(src,source),
             country_sample == ctry,
             engine == "gets") %>%
      bind_rows(overall %>%
                  filter(grepl(src,source),
                         country_sample == ctry,
                         adaptive,
                         scale_by == "country")) %>%
      mutate(plts = pmap(.l = list(is, adaptive, scale_by, lambda, iis), .f = function(x,y,z,lambda, iis){
        titl <- paste0(case_when(y~"Adaptive - ",
                                 !y~"Not Adaptive - ",
                                 is.na(y)~"gets - "), src)

        sub_titl <- paste0(case_when(is.na(y) ~ "",
                                     z == "NULL"~"\nScaled across sample",
                                     z == "country"~"\nScaled within countries"))

        if(!is.na(lambda) & lambda == "BIC"){sub_titl <- paste0(sub_titl,"\nSelected with BIC")}
        if(iis){sub_titl <- paste0(sub_titl," using IIS.")}

        plot_grid(x, title = paste0(titl,sub_titl), regex_exclude_indicators = "^iis")
      })) %>%
      pull(plts) -> plot_list


    cowplot::plot_grid(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]], ncol = 1) -> p
    ggsave(p, file = paste0("2024-05-06 ",ctry,"_",src,".short.png"), width = 8, height = 15, bg = "white")
  }
}








#
#
# for(i in 1:nrow(results_developed)){
#   # i = 1
#   print(i)
#
#   results_developed %>%
#     slice(i) %>%
#     pull(source) %>%
#     str_split_fixed(pattern = " ~ ", n = 2) -> sector
#
#   results_developed %>%
#     slice(i) %>%
#     pull(is) %>%
#     first  -> is
#
#   results_developed_lasso %>%
#     slice(i) %>%
#     pull(is) %>%
#     first -> is_lass
#
#   cowplot::plot_grid(
#     plot_grid(is, title = "gets"),
#     plot_grid(is_lass, title = "LASSO"), nrow = 2, align = "hv", axis = "t") -> p
#
#
#   ggsave(paste0("developed_",sector[1,1],".png"), height = 8, width = 8)
#
#
#   results_developing %>%
#     slice(i) %>%
#     pull(source) %>%
#     str_split_fixed(pattern = " ~ ", n = 2) -> sector
#
#   results_developing %>%
#     slice(i) %>%
#     pull(is) %>%
#     first -> is
#
#   results_developing_lasso %>%
#     slice(i) %>%
#     pull(is) %>%
#     first  -> is_lass
#
#   cowplot::plot_grid(#plot(is[[1]]),
#     #plot(is_lass[[1]]),
#     plot_grid(is, title = "gets"),
#     plot_grid(is_lass, title = "LASSO"), nrow = 2, align = "hv", axis = "t") -> p
#
#
#   ggsave(paste0("developing_",sector[1,1],".png"), height = 8, width = 8)
#
# }
#





