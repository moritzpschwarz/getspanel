
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

cubic = FALSE
type = "sq"
files  <- list.files(here("data-raw","projections","projfiles"),pattern = "massive_EOC", full.names = T)

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


  form <- "diff ~ final_temp + I(final_temp*final_temp)"
  if(cubic){form <- paste0(form," + I(final_temp*final_temp*final_temp)")}
  form <- as.formula(form)

  for(country in massive_overall %>% distinct(iso) %>% pull(iso)){
    success <- FALSE
    while (!success) {
      try({
        print(country)
        intermed <- massive_overall %>% filter(iso==country)

        vlow <- rq(formula = form, data = intermed,method = "pfn",tau = 0.025)
        low <- rq(formula = form, data = intermed,method = "pfn",tau = 0.05)
        midl <- rq(formula = form, data = intermed,method = "pfn",tau = 0.25)
        med <- rq(formula = form, data = intermed,method = "pfn",tau = 0.5)
        midh <- rq(formula = form, data = intermed,method = "pfn",tau = 0.75)
        high <- rq(formula = form, data = intermed,method = "pfn",tau = 0.95)
        vhigh <- rq(formula = form, data = intermed,method = "pfn",tau = 0.975)

        tibble(
          final_temp = temperature_axis$final_temp,
          baseline = baseline,
          model = model,
          specification = specification,
          scenario = "Hundred",
          iso = country,
          vlow  = predict(vlow, newdata = temperature_axis),
          low  =  predict(low, newdata = temperature_axis),
          midl  = predict(midl, newdata = temperature_axis),
          med  =  predict(med, newdata = temperature_axis),
          midh  = predict(midh, newdata = temperature_axis),
          high  = predict(high, newdata = temperature_axis),
          vhigh = predict(vhigh, newdata = temperature_axis)
        ) %>%
          bind_rows(overall_df, .) -> overall_df

        rm(vlow,low,midh,midl,med,high,vhigh)

        success <- TRUE
      },silent = FALSE)
    }
  }
}

overall_df %>%
  arrange(baseline,model,specification,scenario,final_temp) -> overall_df

write_csv(overall_df,here("data-raw","projections",paste0("all_models_iso_quantiles_",type,".csv")))

