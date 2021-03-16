library(tidyverse)
library(vroom)
library(gets)
library(here)
library(broom)

rm(list = ls())

here <- here::here
select <- dplyr::select

execute_isat <- FALSE


dat <- vroom(file = here("data-raw/projections/damage_curve_country_dataset_timetrends_updated02-19.csv"))


# Estimate M1 -------------------------------------------------------------

dat %>%
  select(iso, year, diff.ln_gdp_cap, temp, temp_2, prcp, prcp_2 , starts_with(c("year_","time_", "iso_"))) %>%
  select(-iso,-year) %>%
  lm(diff.ln_gdp_cap~.-1,data = .) -> m1

dat %>%
  select(iso, year, diff.ln_gdp_cap, contains(c("temp","prcp"), ignore.case = FALSE), -contains("diff"),diff.ln_gdp_cap, starts_with(c("year_","time_", "iso_"))) %>%
  select(-iso,-year) %>%
  lm(diff.ln_gdp_cap~.-1,data = .) -> m1_L1


# Process M1 --------------------------------------------------------------


m1 %>%
  tidy %>%
  filter(is.na(estimate)) %>%
  pull(term) -> m1_drop

m1_L1 %>%
  tidy %>%
  filter(is.na(estimate)) %>%
  pull(term) -> m1_L1_drop



# Estimate M2 -------------------------------------------------------------

dat %>%
  select(iso, year, diff.ln_gdp_cap, temp, temp_2, prcp, prcp_2 , starts_with(c("year_","time_", "iso_"))) %>%
  select(-iso,-year,-all_of(m1_drop)) %>%
  drop_na -> m2_data
lm(diff.ln_gdp_cap~.-1,data = m2_data) -> m2


dat %>%
  select(iso, year, diff.ln_gdp_cap, contains(c("temp","prcp"), ignore.case = FALSE), -contains("diff"),diff.ln_gdp_cap, starts_with(c("year_","time_", "iso_"))) %>%
  select(-iso,-year,-all_of(m1_L1_drop)) %>%
  drop_na -> m2_L1_data
lm(diff.ln_gdp_cap~.-1,data = m2_L1_data) -> m2_L1




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



  m2.isat_L1 <- isat(
    y = m2_L1_data %>% pull(diff.ln_gdp_cap),
    mxreg = m2_L1_data %>% select(-diff.ln_gdp_cap) %>% as.matrix,
    mc = FALSE,
    iis = TRUE,
    t.pval = 0.01,
    sis = FALSE,
    max.block.size = 2,
    parallel.options = detectCores()-1,
    print.searchinfo = TRUE
  )
  save(m2.isat_L1, file = here("data-raw","projections","m2.isat_L1.RData"))

} else {
  load(file = here("data-raw","projections","m2.isat.RData"))
  load(file = here("data-raw","projections","m2.isat_L1.RData"))
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
  select(iso, year, ln_gdp_cap, diff.ln_gdp_cap,temp,temp_2, L1.temp, L1.temp_2, prcp, prcp_2,L1.prcp, L1.prcp_2, starts_with(c("year_","time_", "iso_"))) %>%
  mutate(across(.cols = c(temp,temp_2, L1.temp, L1.temp_2, prcp, prcp_2,L1.prcp, L1.prcp_2), .fns = ~ . * cur_data() %>% pull(ln_gdp_cap), .names = "{.col}_int")) %>%
  select(-iso,-year, -ln_gdp_cap) %>%
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
  select(iso, year, diff.ln_gdp_cap, ln_gdp_cap, temp,temp_2, L1.temp, L1.temp_2, prcp, prcp_2,L1.prcp, L1.prcp_2, starts_with(c("year_","time_", "iso_"))) %>%
  mutate(across(.cols = c(temp,temp_2, L1.temp, L1.temp_2, prcp, prcp_2,L1.prcp, L1.prcp_2), .fns = ~ . * cur_data() %>% pull(ln_gdp_cap), .names = "{.col}_int")) %>%
  select(-iso,-year,-all_of(am1_L1_drop),-ln_gdp_cap) %>%
  drop_na -> am2_L1_data
lm(diff.ln_gdp_cap~.-1,data = am2_L1_data) -> am2_L1




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





drop <- vector()
wdi <- c("gdp_cap_pc_growth","ln_gdp_cap","L1.ln_gdp_cap","agricult_growth","industry_growth","service_growth")
drop <- append(drop,wdi)
drop <- append(drop,c("time","time_2"))
if(!use_diffs){
  diffs <- country_dataset %>% select(starts_with("diff.")) %>% names %>% .[.!="diff.ln_gdp_cap"] #
  drop <- append(drop,diffs)
}
if(!include_penn){
  penn <- c("rgdpe","pop","csh_x","csh_m","pwt_gdp_pc","ln_pwt_gdp_pc","L1.rgdpe","L1.pop","L1.csh_x","L1.csh_m","L1.pwt_gdp_pc","L1.ln_pwt_gdp_pc",
            "hc","csh_g","pl_i","trade","L1.hc","L1.csh_g","L1.pl_i","L1.trade") #only needed if using system-based modelling
  drop <- append(drop,penn)
}
if(only_BHM){
  climdex <- names(country_dataset[which(names(country_dataset)=="CDD"):which(names(country_dataset)=="L1.WSDI_2")])
  drop <- append(drop,climdex)
}
if(!use_lags){
  lags <- country_dataset %>% select(starts_with("L1.")) %>% names %>% .[.!="L1.diff.ln_gdp_cap"]
  drop <- append(drop,lags)
}
if(no_BHM){
  drop <- append(drop,c("temp","temp_2","prcp","prcp_2","L1.temp","L1.temp_2","L1.prcp","L1.prcp_2"))
}

# Complete Cases ----------------------------------------------------------
cleaned <- country_dataset %>% select(-drop) #drops all unneeded variables
#complete cases
cleaned_comp <- cleaned %>% filter(complete.cases(.)) #retains only complete cases

### drop year but save it separately
year.x <- cleaned_comp %>% select(year)
cleaned_comp <- cleaned_comp %>% select(-year)

### drop gdp_cap but save it separately
gdp_cap.x <- cleaned_comp %>% select(gdp_cap)
cleaned_comp <- cleaned_comp %>% select(-gdp_cap)


regr <- lm(diff.ln_gdp_cap ~ .-1,data=cleaned_comp[,-c(which(names(cleaned_comp)=="iso"))]) #regresses growth over everything

#complete coefficients
drop <- names(coefficients(regr))[which(is.na(coefficients(regr)))] #checks which coefficients are na in the above regression
cleaned_comp_drop <- cleaned_comp[,!names(cleaned_comp) %in% drop] #drops the coefficients which were na
dep_var <- cleaned_comp_drop$diff.ln_gdp_cap #saves the growth variable

###drop iso
iso.x <- cleaned_comp_drop$iso
cleaned_comp_drop <- cleaned_comp_drop[,-c(which(names(cleaned_comp_drop)=="iso"))]

dep_var_name <- c("diff.ln_gdp_cap")
selection_matrix_std <- as.matrix(cleaned_comp_drop[,!names(cleaned_comp_drop) %in% dep_var_name]) # drops the growth variable and saves the remaining variables and cases as selection matrix
