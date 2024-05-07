library(conflicted)
library(tidyverse)
library(glmnet)
conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::first)

load("Saving Overall intermediate.RData")


gets_obj <- overall %>%
  filter(engine == "gets", country_sample == "AC6", grepl("buildings",source)) %>%
  pull(is) %>% first

gets_coef <- coef(gets_obj$isatpanel.result)[get_indicators(gets_obj)$fesis$name]
gets_coef_neg <- gets_coef[gets_coef < 0]
no_neg_breaks <- length(gets_coef_neg)


lasso_obj <- overall %>%
  filter(engine == "lasso", country_sample == "AC6", grepl("buildings",source), scale_by == "country", !iis, lambda == "min") %>%
  pull(is) %>% first

plot(lasso_obj$lasso_output$firststage)


threshold_lasso <- function(lasso_obj, target_neg_breaks, target_overall_breaks = NULL, plot = TRUE){

  if(!is.null(target_neg_breaks) & !is.null(target_overall_breaks)){stop("Can only either use neg_breaks or overall_breaks. One of them must be NULL, the other must be numeric.")}

  lass_coef_collection <- tibble()
  if(!is.null(lasso_obj$lasso_output$secondstage)){
    pot_lambdas <- lasso_obj$lasso_output$firststage$lambda
    glmnet_obj <- lasso_obj$lasso_output$firststage
  } else {
    pot_lambdas <- lasso_obj$lasso_output$secondstage$lambda
    glmnet_obj <- lasso_obj$lasso_output$secondstage
  }

  for(i in pot_lambdas){
    # i = pot_lambdas[1]
    orig_lass_coef <- coef(glmnet_obj, s = i)
    lass_coef <- as.numeric(orig_lass_coef)
    lass_vars <- length(lass_coef[lass_coef != 0])
    neg_breaks <- tibble(name = row.names(orig_lass_coef),
                         coef = as.numeric(orig_lass_coef)) %>%
      filter(!name %in% c("Intercept",names(lasso_obj$estimateddata))) %>%
      filter(coef < 0) %>%
      nrow

    tibble(lambda = i,
           lass_vars = lass_vars,
           neg_breaks = neg_breaks) %>%
      bind_rows(lass_coef_collection, .) -> lass_coef_collection
  }


  lass_coef_collection %>%
    mutate(diff_to_gets = abs(neg_breaks - target_neg_breaks)) %>%
    filter(diff_to_gets == min(diff_to_gets)) %>%
    filter(lambda == min(lambda)) %>%
    pull(lambda) -> lambda_to_use


  coef_to_use <- coef(glmnet_obj, s = lambda_to_use)
  lasso_ret_names <- row.names(coef_to_use)[as.numeric(coef_to_use) != 0]

  lasso_obj$estimateddata %>%
    bind_cols(lasso_obj$indicator_matrix$fesis) %>%
    as_tibble() %>%
    select(all_of(lasso_ret_names)) -> dat_for_new_estimate

  new_lasso_obj <- lasso_obj
  new_lasso_obj$isatpanel.result <- suppressWarnings(arx(y = lasso_obj$inputdata$y, mxreg = dat_for_new_estimate))
  new_lasso_obj$isatpanel.result$aux$mX <- dat_for_new_estimate[,colnames(dat_for_new_estimate) %in% new_lasso_obj$isatpanel.result$aux$mXnames]



  if(plot){
    lass_coef_collection %>%
      pivot_longer(-lambda) %>%
      ggplot(aes(x = lambda, y = value, color = name)) +
      geom_hline(aes(yintercept = 0)) +
      geom_vline(aes(xintercept = lambda_to_use)) +
      geom_line() +
      theme_minimal() -> p1

    #plot(new_lasso_obj) -> p2
    plot_grid(new_lasso_obj) -> p3

    print(cowplot::plot_grid(p1,
                             #p2,
                             p3, ncol = 1))

  }



  out <- list()
  out$lasso_obj <- lasso_obj
  out$new_lasso_obj <- new_lasso_obj
  out$lambda <- lambda_to_use
}





# Run model again ---------------------------------------------------------

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

##run models for developing economies
rel_forms_trends <- tot_forms_trends[1:4]

f = rel_forms_trends[1]
scale_by = "country"
adpt = FALSE
lambda = lambda_to_use
iis = FALSE


print(paste0("Scale by:",scale_by," Adaptive:",adpt, " Sector:", f, " Lambda:",lambda, " IIS:",iis))


set.seed(123)
dat <- df_restr_trends %>% filter(country %in% samples[["AC6"]])
if(scale_by == "NULL"){
  dat <- mutate(dat,across(c(where(is.numeric),-year), ~as.numeric(scale(.))))
} else {
  dat <- mutate(dat,across(c(where(is.numeric),-year), ~as.numeric(scale(.))), .by = country)
}


# dat %>%
#   pivot_longer(-c(country,year)) %>%
#   ggplot(aes(x = year, y = value, color = name)) +
#   geom_line() +
#   facet_wrap(~country)

is <- isatpanel(
  data = dat,
  formula = as.formula(f),
  index = c("country", "year"),
  effect = "twoways",
  iis = iis,
  fesis = TRUE,
  ar = 0,
  engine = "lasso",
  lasso_opts = list(adaptive = adpt, foldid = 0.25, s = "min", scale_by = "id")
)
