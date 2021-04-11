library(tidyverse)
# library(data.table)
library(gets)
library(here)
library(vroom)
library(MASS)

rm(list = ls())
select <- dplyr::select
here <- here::here




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

project_standard <- function(model,
                             climate,
                             modelname = "m2",
                             coefsamples = 1,
                             seed = 123,
                             parallel = TRUE,
                             verbose = TRUE,
                             save_dir_here = "data-raw/projections/projfiles"){


  ## Load Coefficients

  set.seed(seed)
  selection_coefs_standard <- MASS::mvrnorm(n = coefsamples, mu = model %>% coef(), Sigma = model %>% vcov()) %>%
    {if (is.vector(.)) t(.) else .} %>%
    data.frame() %>%
    rename_all(~ tolower(.)) %>%
    select(-starts_with(c("year", "iis", "time", "iso"))) %>%
    rename_all(~ paste0(., "_coef"))

  # If we are just using one coefsample, we simply use the mean estimate from the model
  if(coefsamples==1){
    model %>%
      coef %>%
      data.frame(variable = names(.),
                 coefficient = .,
                 row.names = NULL) %>%
      pivot_wider(names_from = "variable",values_from = "coefficient") %>%
      rename_all(~tolower(.)) %>%
      select(-starts_with(c("year","iis","time","iso"))) %>%
      rename_all(~paste0(.,"_coef")) -> selection_coefs_standard
  }



  ## Collect Relevant Information
  selection_coefs_standard %>%
    names %>%
    gsub("_coef","",.) %>%
    unique -> climate_vars

  selection_coefs_standard %>%
    names %>%
    gsub("_coef|_2","",.) %>%
    unique -> climate_vars_lin

  selection_coefs_standard %>%
    names %>%
    grep("_2",.,value = TRUE) %>%
    gsub("_coef|_2","",.) %>%
    unique -> climate_vars_sq



  ## Load climate data
  climate %>%
    # add the lags if needed
    {if(any(grepl("^l[0-9]\\.",climate_vars))){
      group_by(.,final_temp, iso) %>%
        mutate(across(.cols = all_of(grep("^l[0-9]\\.",climate_vars, invert = TRUE, value = TRUE)),
                      .fns = lag,
                      .names = "l1.{.col}")) %>%
        ungroup
    }else{.}} %>%
    select(model,rcp,ensemble,final_temp,iso,year,all_of(climate_vars_lin),
           all_of(paste0(climate_vars_sq,"_2"))) %>%
    drop_na -> climate_subset



  internal_fun <- function(v,
                           verbose,
                           climate_subset,
                           climate_vars,
                           climate_vars_lin,
                           climate_vars_sq,
                           selection_coefs_standard,
                           save_dir_here){


    # Calculate the climate effect
    effect_standard <- climate_subset
    for(var in climate_vars){
      #print(var)
      climate_subset %>%
        select(all_of(var)) %>%
        pull %>% "*"(selection_coefs_standard %>%
                       slice(v) %>%
                       select(all_of(paste0(var,"_coef"))) %>%
                       pull) %>%
        as_tibble %>%
        rename_all(~paste0(var,"_effect"))  %>%
        bind_cols(effect_standard,.) -> effect_standard
    }

    done <- mueller_df %>%
      select(-contains("fifty")) %>%
      filter(year==2017) %>%
      left_join(effect_standard %>%
                  filter(year == 2017) %>%
                  select(-model,-ensemble,-rcp),by = c("iso","year")) %>%
      mutate(realisation = v) %>%
      relocate(c(realisation,final_temp), .after = year)

    for(i in 2018:2099){
      if(verbose){print(paste0("Realisation ",v," Year: ",i))}
      done %>%
        filter(year == i-1) %>%
        select(iso, year, realisation, final_temp,contains("hundred")) %>%

        left_join(effect_standard %>%
                    filter(year == i-1) %>%
                    mutate(realisation = v) %>%
                    select(-model,-ensemble,-rcp),by = c("iso","year","final_temp","realisation")) %>%
        mutate(total_climate_effect = rowSums(select(., ends_with("effect")))) %>%
        mutate(gdp_cap_hundred = gdp_cap_hundred*hundred_mean,
               gdp_cap_hundred_climate = gdp_cap_hundred_climate*(hundred_mean + total_climate_effect),
               year = i) %>%
        bind_rows(done,.) -> done

    }

    save(done,file = here(save_dir_here,paste0("Mueller_",modelname,"_",v,".RData")))
  }



  if(parallel){
    library(doMC)
    registerDoMC(if(coefsamples < detectCores()){coefsamples} else {detectCores()-1})  # coefsamples if enough cores available - otherwise total-1
    foreach(v=1:coefsamples,.packages = loadedNamespaces()) %dopar% {
      if(verbose){print(v)}
      internal_fun(v,
                   verbose,
                   climate_subset,
                   climate_vars,
                   climate_vars_lin,
                   climate_vars_sq,
                   selection_coefs_standard,
                   save_dir_here)

    }
  } else {
    for (v in 1:coefsamples){
      if(verbose){print(v)}
      internal_fun(v,
                   verbose,
                   climate_subset,
                   climate_vars,
                   climate_vars_lin,
                   climate_vars_sq,
                   selection_coefs_standard,
                   save_dir_here)
    }
  }
}


climate_path <- here("data-raw","projections","corrected_anomaly_climatedata.csv")
climate <- vroom(climate_path)

# load(here("data-raw/projections/m2.RData"))
# project_standard(m2,climate, coefsamples = 100, modelname = "m2")
# rm(m2)

load(here("data-raw/projections/m2_L1.RData"))
project_standard(model = m2_L1,climate, coefsamples = 100, modelname = "m2_L1")
rm(m2_L1)

# load(here("data-raw/projections/m2.isat.RData"))
# project_standard(m2.isat,climate, coefsamples = 100, modelname = "m2.isat")
# rm(m2.isat)

load(here("data-raw/projections/m2.isat_L1.RData"))
project_standard(m2.isat_L1,climate, coefsamples = 100, modelname = "m2_L1.isat")
rm(m2.isat_L1)





# combine -----------------------------------------------------------------


rm(list = ls())
select <- dplyr::select


socio <- c("Mueller")
model<- c(
  "m2_[0-9]",
  "m2.isat",
  "m2_L1_",
  "m2_L1.isat",
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
    if(!grepl("isat",j)){indv_files <- indv_files[!grepl("isat",indv_files)]}


    massive_overall <- tibble()
    for(k in seq_along(indv_files)){
      print(k)
      load(indv_files[k])

      done %>%
        filter(year > 2089) %>%

        mutate(diff = gdp_cap_hundred_climate / gdp_cap_hundred) %>%
        drop_na %>%

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

library(quantreg)

rm(list = ls())
select <- dplyr::select

for(type in c("sq")){
  files  <- list.files(here("data-raw","projections","projfiles"),pattern = "massive_EOC", full.names = T)
  #files <- grep("BMS",files,value=T,ignore.case = FALSE)
  #files <- grep("revision|altrestr1",files,value=T,ignore.case = FALSE)
  #files <- grep("Mueller",files,value=T,ignore.case = FALSE)

  overall_df <- tibble()
  for(i in 1:length(files)){
    print(gsub("_massive_EOC", "",gsub(".RData", "", gsub(paste0(here("data-raw","projections","projfiles"),"/"),"",files[i]), fixed = T)))

    load(files[i])

    tibble(name = gsub("_EOC", "",
                       gsub(".RData", "", gsub(paste0(here("data-raw","projections","projfiles"),"/"),"",files[i]), fixed = T))) %>%
      mutate(name = case_when(name=="Mueller_m2_massive"~"Mueller_Contemp_Base",
                              name=="Mueller_m2.isat_massive"~"Mueller_Contemp_IIS",
                              name=="Mueller_m2_L1_massive"~"Mueller_Lagged_Base",
                              name=="Mueller_m2_L1.isat_massive"~"Mueller_Lagged_IIS",
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


    rm(vlow,low,midh,midl,med,high,vhigh)
  }



  overall_df %>% arrange(baseline,model,specification,scenario,final_temp) -> overall_df


  write_csv(overall_df,here("data-raw","projections","projfiles",paste0("all_models_quantiles_",type,".csv")))
}



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
  facet_wrap(~model)+
  coord_cartesian(ylim = c(-1,1))+
  labs(x = "Temperature Anomaly", y = "% Change to Baseline") +
  ggsave(here("data-raw/projections/out/projections.pdf"), height = 6, width = 8)

