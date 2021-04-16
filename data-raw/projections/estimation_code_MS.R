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
  select(-iso,-year,-all_of(m1_drop)) %>%
  drop_na -> m2_data
lm(diff.ln_gdp_cap~.-1,data = m2_data) -> m2


# save m2 ---------------------------------------------------------------

#save(m2, file = here("data-raw/projections/m2.RData"))


# Isat --------------------------------------------------------------------

if(execute_isat){
  m2.isat <- isat(
    y = m2_data %>% pull(diff.ln_gdp_cap),
    mxreg = m2_data %>% select(-diff.ln_gdp_cap) %>% as.matrix,
    mc = FALSE,
    iis = TRUE,
    t.pval = 0.01,
    sis = FALSE,
    max.block.size = 2,
    parallel.options = detectCores()-1,
    print.searchinfo = TRUE
  )

  save(m2.isat, file = here("data-raw","projections","m2.isat.RData"))



} else {
  load(file = here("data-raw","projections","m2.isat.RData"))
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
  select(-iso,-year,-all_of(am1_drop),-ln_gdp_cap) %>%
  drop_na -> am2_data
lm(diff.ln_gdp_cap~.-1,data = am2_data) -> am2


dat %>%
  select(iso, year, L1.ln_gdp_cap, diff.ln_gdp_cap, temp, temp_2, prcp, prcp_2, starts_with(c("year_","time_", "iso_"))) %>%
  mutate(across(.cols = c(temp,temp_2, prcp, prcp_2), .fns = ~ . * cur_data() %>% pull(L1.ln_gdp_cap), .names = "{.col}_int")) %>%
  select(-iso,-year,-all_of(am1_L1_drop),-L1.ln_gdp_cap) %>%
  drop_na -> am2_L1_data
lm(diff.ln_gdp_cap~.-1,data = am2_L1_data) -> am2_L1

# save am2 ---------------------------------------------------------------

save(am2, file = here("data-raw/projections/am2.RData"))
save(am2_L1, file = here("data-raw/projections/am2_L1.RData"))



# Isat --------------------------------------------------------------------

if(execute_isat){
  am2.isat <- isat(
    y = am2_data %>% pull(diff.ln_gdp_cap),
    mxreg = am2_data %>% select(-diff.ln_gdp_cap) %>% as.matrix,
    mc = FALSE,
    iis = TRUE,
    t.pval = 0.01,
    sis = FALSE,
    max.block.size = 2,
    parallel.options = detectCores()-1,
    print.searchinfo = TRUE
  )

  save(am2.isat, file = here("data-raw","projections","am2.isat.RData"))


  am2.isat_L1 <- isat(
    y = am2_L1_data %>% pull(diff.ln_gdp_cap),
    mxreg = am2_L1_data %>% select(-diff.ln_gdp_cap) %>% as.matrix,
    mc = FALSE,
    iis = TRUE,
    t.pval = 0.01,
    sis = FALSE,
    max.block.size = 2,
    parallel.options = detectCores()-1,
    print.searchinfo = TRUE
  )
  save(am2.isat_L1, file = here("data-raw","projections","am2.isat_L1.RData"))

} else {
  load(file = here("data-raw","projections","am2.isat.RData"))
  load(file = here("data-raw","projections","am2.isat_L1.RData"))
}

