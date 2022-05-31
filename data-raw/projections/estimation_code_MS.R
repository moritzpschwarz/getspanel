library(tidyverse)
library(vroom)
library(gets)
library(here)
library(broom)

rm(list = ls())

here <- here::here
select <- dplyr::select

execute_isat <- TRUE


dat <- vroom(file = here("data-raw/projections/damage_curve_country_dataset_timetrends_updated02-19.csv"))

for(target_pval in c(0.01)){

  # No Adaptation -----------------------------------------------------------

  # Estimate M1 -------------------------------------------------------------

  dat %>%
    select(iso, year, diff.ln_gdp_cap, temp, temp_2, prcp, prcp_2 , starts_with(c("year_","time_", "iso_"))) %>%
    select(-iso,-year) %>%
    lm(diff.ln_gdp_cap~.-1,data = .) -> m1


  # Process M1 --------------------------------------------------------------


  m1 %>%
    tidy %>%
    filter(is.na(estimate)) %>%
    pull(term) -> m1_drop

  # Estimate M2 -------------------------------------------------------------

  dat %>%
    select(iso, year, diff.ln_gdp_cap, temp, temp_2, prcp, prcp_2 , starts_with(c("year_","time_", "iso_"))) %>%
    select(-all_of(m1_drop)) %>%
    drop_na -> m2_data

  write_csv(m2_data, here("data-raw/projections/m2_data.csv"))

  lm(diff.ln_gdp_cap~.-1,data = m2_data %>% select(-c(iso,year))) -> m2


  # save m2 ---------------------------------------------------------------

  save(m2, file = here("data-raw/projections/m2.RData"))


  # Isat --------------------------------------------------------------------

  if(execute_isat){
    m2.isat <- isat(
      y = m2_data %>% pull(diff.ln_gdp_cap),
      mxreg = m2_data %>% select(-c(diff.ln_gdp_cap,iso,year)) %>% as.matrix,
      mc = FALSE,
      iis = TRUE,
      t.pval = target_pval,
      sis = FALSE,
      max.block.size = 2,
      parallel.options = parallel::detectCores()-2,
      print.searchinfo = TRUE
    )

    save(m2.isat, file = here("data-raw","projections",paste0("m2.isat.",target_pval,".RData")))



  } else {
    load(file = here("data-raw","projections",paste0("m2.isat.",target_pval,"RData")))
  }


  # Adaptation --------------------------------------------------------------

  # Estimate AM1 -------------------------------------------------------------

  dat %>%
    select(iso, year, ln_gdp_cap, diff.ln_gdp_cap, temp, temp_2, prcp, prcp_2, starts_with(c("year_","time_", "iso_"))) %>%
    mutate(across(.cols = c(temp,temp_2, prcp, prcp_2), .fns = ~ . * cur_data() %>% pull(ln_gdp_cap), .names = "{.col}_int")) %>%
    relocate(ends_with("int"),.after = prcp_2) %>%
    select(-iso,-year, -ln_gdp_cap) %>%
    lm(diff.ln_gdp_cap~.-1,data = .) -> am1

  dat %>%
    select(iso, year, L1.ln_gdp_cap, diff.ln_gdp_cap, temp, temp_2, prcp, prcp_2, starts_with(c("year_","time_", "iso_"))) %>%
    mutate(across(.cols = c(temp,temp_2, prcp, prcp_2), .fns = ~ . * cur_data() %>% pull(L1.ln_gdp_cap), .names = "{.col}_int")) %>%
    relocate(ends_with("int"),.after = prcp_2) %>%
    select(-iso,-year, -L1.ln_gdp_cap) %>%
    lm(diff.ln_gdp_cap~.-1,data = .) -> am1_L1


  # Process AM1 --------------------------------------------------------------

  am1 %>%
    tidy %>%
    filter(is.na(estimate)) %>%
    pull(term) -> am1_drop

  am1_L1 %>%
    tidy %>%
    filter(is.na(estimate)) %>%
    pull(term) -> am1_L1_drop



  # Estimate M2 -------------------------------------------------------------

  dat %>%
    select(iso, year, ln_gdp_cap, diff.ln_gdp_cap, temp, temp_2, prcp, prcp_2 , starts_with(c("year_","time_", "iso_"))) %>%
    mutate(across(.cols = c(temp,temp_2, prcp, prcp_2), .fns = ~ . * cur_data() %>% pull(ln_gdp_cap), .names = "{.col}_int")) %>%
    select(-all_of(am1_drop)) %>%
    drop_na -> am2_data

  write_csv(am2_data, here("data-raw/projections/am2_data.csv"))

  lm(diff.ln_gdp_cap~.-1,data = am2_data %>% select(-c(iso,year,ln_gdp_cap))) -> am2


  dat %>%
    select(iso, year, L1.ln_gdp_cap, diff.ln_gdp_cap, temp, temp_2, prcp, prcp_2, starts_with(c("year_","time_", "iso_"))) %>%
    mutate(across(.cols = c(temp,temp_2, prcp, prcp_2), .fns = ~ . * cur_data() %>% pull(L1.ln_gdp_cap), .names = "{.col}_int")) %>%
    select(-all_of(am1_L1_drop)) %>%
    drop_na -> am2_L1_data

  write_csv(am2_L1_data, here("data-raw/projections/am2_L1_data.csv"))

  lm(diff.ln_gdp_cap~.-1,data = am2_L1_data %>% select(-c(iso,year,L1.ln_gdp_cap))) -> am2_L1

  # save am2 ---------------------------------------------------------------

  save(am2, file = here("data-raw/projections/am2.RData"))
  save(am2_L1, file = here("data-raw/projections/am2_L1.RData"))



  # Isat --------------------------------------------------------------------

  if(execute_isat){
    am2.isat <- isat(
      y = am2_data %>% pull(diff.ln_gdp_cap),
      mxreg = am2_data %>% select(-c(diff.ln_gdp_cap,iso,year)) %>% as.matrix,
      mc = FALSE,
      iis = TRUE,
      t.pval = target_pval,
      sis = FALSE,
      max.block.size = 2,
      parallel.options = parallel::detectCores()-2,
      print.searchinfo = TRUE
    )

    save(am2.isat, file = here("data-raw","projections",paste0("am2.isat.",target_pval,"RData")))


    am2.isat_L1 <- isat(
      y = am2_L1_data %>% pull(diff.ln_gdp_cap),
      mxreg = am2_L1_data %>% select(-c(diff.ln_gdp_cap,iso,year)) %>% as.matrix,
      mc = FALSE,
      iis = TRUE,
      t.pval = target_pval,
      sis = FALSE,
      max.block.size = 2,
      parallel.options = parallel::detectCores()-2,
      print.searchinfo = TRUE
    )
    save(am2.isat_L1, file = here("data-raw","projections",paste0("am2.isat_L1.",target_pval,"RData")))

  } else {
    load(file = here("data-raw","projections",paste0("am2.isat.",target_pval,"RData")))
    load(file = here("data-raw","projections",paste0("am2.isat_L1.",target_pval,"RData")))
  }

}

