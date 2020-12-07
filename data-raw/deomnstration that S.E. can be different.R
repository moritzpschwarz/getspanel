library(here)
library(tidyverse)
library(lfe)
library(fixest)

load(here("saving_getsfun.RData"))

data <- as_tibble(df[[1]])

data %>%
  bind_cols(.,time = df[[2]]$time) %>%
  bind_cols(.,id = df[[2]]$id) %>%
  relocate(id,time) %>%
  mutate(id = factor(id),
         time = factor(time)) -> dt



lmmodel <- lm(y~.,data = dt)

parsed_formula <- as.formula(paste0("y ~ ",paste0(colnames(dt %>% select(-id,-time,-y)),collapse = " + "), "| time + id | 0 | 0 "))
nocluster <- felm(formula = parsed_formula,data = dt)


parsed_formula <- as.formula(paste0("y ~ ",paste0(colnames(dt %>% select(-id,-time,-y)),collapse = " + "), "| time + id | 0 | time "))
withcluster <- felm(formula = parsed_formula,data = dt)
summary(withcluster)

parsed_formula <- as.formula(paste0("y ~ ",paste0(colnames(dt %>% select(-id,-time,-y)),collapse = " + "), "| id + time"))
fixestcluster <- feols(fml = parsed_formula,data = dt)

summary(fixestcluster,se = "standard")
summary(fixestcluster,se = "twoway")
summary(fixestcluster,se = "cluster",cluster = "id")
summary(fixestcluster,se = "cluster",cluster = "time")

summary(nocluster)
summary(nocluster,se = "cluster", cluster="id")
summary(withcluster)


summary(lmmodel,se = "white")
vcov(lmmodel, cluster = df[[2]]$id)

lmtest::coeftest(lmmodel,sandwich::vcovCL(lmmodel,cluster = df[[2]]$id,type = "HC1"))
