library(here)
library(tidyverse)
library(gets)

rm(list=ls())

load(here("data-raw/projections/m2.RData"))
load(here("data-raw/projections/m2.isat.RData"))
load(here("data-raw/projections/am2.RData"))
load(here("data-raw/projections/am2.isat.RData"))
load(here("data-raw/projections/am2_L1.RData"))
load(here("data-raw/projections/am2.isat_L1.RData"))


library(plm)

dat <- vroom::vroom(file = here("data-raw/projections/damage_curve_country_dataset_timetrends_updated02-19.csv"))
m2_data <- vroom::vroom(file = here("data-raw/projections/m2_data.csv"))
am2_data <- vroom::vroom(file = here("data-raw/projections/am2_data.csv"))
am2_L1_data <- vroom::vroom(file = here("data-raw/projections/am2_L1_data.csv"))


# Serial Correlation Testing

plm_formula <- as.formula(paste0("diff.ln_gdp_cap ~ ",paste0(m2_data %>% select(-c(iso,year,diff.ln_gdp_cap)) %>% names,collapse = " + ")))
m2.plm <- plm(plm_formula, effect = "twoways",model = "within", index = c("iso","year"), data = m2_data)
test_m2 <- plm::pbgtest(m2.plm)

m2.isat_data <- cbind(m2_data, iim(m2_data$iso,which.ones = isatdates(m2.isat)$iis$index))
plm_formula <- as.formula(paste0("diff.ln_gdp_cap ~ ",paste0(m2.isat_data %>% select(-c(iso,year,diff.ln_gdp_cap)) %>% names,collapse = " + ")))
m2.isat.plm <- plm(plm_formula, effect = "twoways",model = "within", index = c("iso","year"), data = m2.isat_data)
test_m2.isat <- plm::pbgtest(m2.isat.plm)


plm_formula <- as.formula(paste0("diff.ln_gdp_cap ~ ",paste0(am2_data %>% select(-c(iso,year,diff.ln_gdp_cap)) %>% names,collapse = " + ")))
am2.plm <- plm(plm_formula, effect = "twoways",model = "within", index = c("iso","year"), data = am2_data)
test_am2 <- plm::pbgtest(am2.plm)

am2.isat_data <- cbind(am2_data, iim(am2_data$iso,which.ones = isatdates(am2.isat)$iis$index))
plm_formula <- as.formula(paste0("diff.ln_gdp_cap ~ ",paste0(am2.isat_data %>% select(-c(iso,year,diff.ln_gdp_cap)) %>% names,collapse = " + ")))
am2.isat.plm <- plm(plm_formula, effect = "twoways",model = "within", index = c("iso","year"), data = am2.isat_data)
test_am2.isat <- plm::pbgtest(am2.isat.plm)


plm_formula <- as.formula(paste0("diff.ln_gdp_cap ~ ",paste0(am2_L1_data %>% select(-c(iso,year,diff.ln_gdp_cap)) %>% names,collapse = " + ")))
m2.plm <- plm(plm_formula, effect = "twoways",model = "within", index = c("iso","year"), data = am2_L1_data)
test_am2_L1 <- plm::pbgtest(m2.plm)

am2.isat_L1_data <- cbind(am2_L1_data, iim(am2_L1_data$iso,which.ones = isatdates(m2.isat)$iis$index))
plm_formula <- as.formula(paste0("diff.ln_gdp_cap ~ ",paste0(am2_L1_data %>% select(-c(iso,year,diff.ln_gdp_cap)) %>% names,collapse = " + ")))
m2.isat.plm <- plm(plm_formula, effect = "twoways",model = "within", index = c("iso","year"), data = am2.isat_L1_data)
test_am2.isat_L1 <- plm::pbgtest(m2.isat.plm)


test_list <- list(test_m2, test_m2.isat, test_am2, test_am2.isat, test_am2_L1, test_am2.isat_L1)
pvals <- c()
for(i in 1:length(test_list)){
  pvals <- c(pvals,test_list[[i]]$p.value)
}

tests <- tibble(model = c("Base", "Base", "Adapat", "Adapt", "L1.Adapt", "L1.Adapt"),
                bgtest_pval = pvals)


# Heteroskedasticity

library(lmtest)

bptest <- c(bptest(m2)$p.value,
            bptest(m2.isat %>% as.lm)$p.value,
            bptest(am2)$p.value,
            bptest(am2.isat %>% as.lm)$p.value,
            bptest(am2_L1)$p.value,
            bptest(am2.isat_L1 %>% as.lm)$p.value)


# Normality of residuals

normality <- c(diagnostics(x = list(residuals = m2$residuals), ar.LjungB = NULL, arch.LjungB = NULL, normality.JarqueB = 0.05)[3],
               diagnostics(x = list(residuals = m2.isat$residuals), ar.LjungB = NULL, arch.LjungB = NULL, normality.JarqueB = 0.05)[3],
               diagnostics(x = list(residuals = am2$residuals), ar.LjungB = NULL, arch.LjungB = NULL, normality.JarqueB = 0.05)[3],
               diagnostics(x = list(residuals = am2.isat$residuals), ar.LjungB = NULL, arch.LjungB = NULL, normality.JarqueB = 0.05)[3],
               diagnostics(x = list(residuals = am2_L1$residuals), ar.LjungB = NULL, arch.LjungB = NULL, normality.JarqueB = 0.05)[3],
               diagnostics(x = list(residuals = am2.isat_L1$residuals), ar.LjungB = NULL, arch.LjungB = NULL, normality.JarqueB = 0.05)[3])

tests %>%
  mutate(bptest_pval = bptest,
         jarque_pval = normality) -> test_outcomes
