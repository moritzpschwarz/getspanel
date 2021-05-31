

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
#files <- grep("_L1",files,value=T,ignore.case = FALSE, invert = TRUE)
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
    drop_na %>%
    distinct(final_temp)  -> temperature_axis

  massive_overall %>%
    drop_na -> massive_overall

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
  mutate(model = factor(model,
                        levels =  c("Standard","Adaptation","AdaptationL1"),
                        labels = c("Standard","Adaptation","Adaptation \nwith Lagged GDP"))) %>%
  mutate(across(c(vlow, low, midl, med, midh, high, vhigh),~.-1)) %>%
  ggplot(aes(x = final_temp, color = specification, fill = specification, group = paste0(specification, model))) +

  geom_ribbon(aes(ymin = midl, ymax = midh),alpha = 0.3) +
  geom_ribbon(aes(ymin = low, ymax = high),alpha = 0.3) +
  geom_line(aes(y  = med), size = 1) +
  geom_hline(aes(yintercept = 0), size = 1)+
  scale_color_viridis_d(name = "Specification") +
  scale_fill_viridis_d(name = "Specification") +
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = function(x){paste0(x,"°C")})+
  theme_minimal() +
  facet_wrap(~model, nrow = 1)+
  coord_cartesian(ylim = c(-1,1))+
  labs(x = "Temperature Anomaly", y = "% Change to Baseline") +
  ggsave(here("data-raw/projections/out/projections.pdf"), height = 6, width = 8)



overall_df %>%
  mutate(across(c(vlow, low, midl, med, midh, high, vhigh),~.-1)) %>%
  ggplot(aes(x = final_temp, color = model, fill = model, group = paste0(specification, model))) +

  geom_ribbon(aes(ymin = midl, ymax = midh),alpha = 0.3) +
  geom_ribbon(aes(ymin = low, ymax = high),alpha = 0.3) +
  geom_line(aes(y  = med), size = 1) +
  geom_hline(aes(yintercept = 0), size = 1)+
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = function(x){paste0(x,"°C")})+
  theme_minimal() +
  facet_wrap(~specification, nrow = 1)+
  coord_cartesian(ylim = c(-1,1))+
  labs(x = "Temperature Anomaly", y = "% Change to Baseline") +
  ggsave(here("data-raw/projections/out/projections_byspec.pdf"), height = 6, width = 8)




overall_df %>%
  mutate(across(c(vlow, low, midl, med, midh, high, vhigh),~.-1)) %>%
  ggplot(aes(x = final_temp, color = paste0(specification, model), fill = paste0(specification, model), group = paste0(specification, model))) +

  #geom_ribbon(aes(ymin = midl, ymax = midh),alpha = 0.1) +
  #geom_ribbon(aes(ymin = low, ymax = high),alpha = 0.1) +
  geom_line(aes(y  = med), size = 1) +
  geom_hline(aes(yintercept = 0), size = 1)+
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = function(x){paste0(x,"°C")})+
  theme_minimal() +
  #facet_wrap(~specification, nrow = 1)+
  coord_cartesian(ylim = c(-1,1))+
  labs(x = "Temperature Anomaly", y = "% Change to Baseline")

