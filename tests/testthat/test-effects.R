# devtools::install_github(repo = "moritzpschwarz/getspanel")
library(getspanel)
library(tidyverse) # needed for the plots
library(fixest)
library(lfe)

data("pandata_simulated")


# Case 1: No FE

lm(y~temp,is2$estimateddata) %>% summary

feols(y~temp, is2$estimateddata)

isatpanel(data = pandata_simulated,
          formula = gdp ~ temp,
          index = c("country","year"),
          effect = "none",
          csis = TRUE)

isatpanel(data = pandata_simulated,
          formula = gdp ~ temp,
          index = c("country","year"),
          effect = "none",
          engine = "fixest",
          csis = TRUE)

isatpanel(data = pandata_simulated,
          formula = gdp ~ temp,
          index = c("country","year"),
          effect = "none",
          engine = "felm",
          csis = TRUE)

# Case 2: Indv FE

lm(y~temp + as.factor(id),is2$estimateddata) %>% summary
lm(y~temp+ as.factor(id)-1,is2$estimateddata) %>% summary

feols(y~temp|id, is2$estimateddata)
feols(y~temp-1|id, is2$estimateddata)

isatpanel(data = pandata_simulated,
          formula = gdp ~ temp,
          index = c("country","year"),
          effect = "individual",
          csis = TRUE)

isatpanel(data = pandata_simulated,
          formula = gdp ~ temp,
          index = c("country","year"),
          effect = "individual",
          engine = "fixest",
          csis = TRUE)

isatpanel(data = pandata_simulated,
          formula = gdp ~ temp,
          index = c("country","year"),
          effect = "individual",
          engine = "felm",
          csis = TRUE)

# Case 3: Time FE

lm(y~temp + as.factor(time),is2$estimateddata) %>% summary
lm(y~temp+ as.factor(time)-1,is2$estimateddata) %>% summary

feols(y~temp|time, is2$estimateddata)
feols(y~temp-1|time, is2$estimateddata)

isatpanel(data = pandata_simulated,
          formula = gdp ~ temp,
          index = c("country","year"),
          effect = "time",
          csis = TRUE)

isatpanel(data = pandata_simulated,
          formula = gdp ~ temp,
          index = c("country","year"),
          effect = "time",
          engine = "fixest",
          csis = TRUE)

isatpanel(data = pandata_simulated,
          formula = gdp ~ temp,
          index = c("country","year"),
          effect = "time",
          engine = "felm",
          csis = TRUE)

# Case 4: Two-way FE

lm(y~temp + as.factor(time) + as.factor(id),is2$estimateddata) %>% summary
lm(y~temp+ as.factor(time) + as.factor(id)-1,is2$estimateddata) %>% summary

feols(y~temp|time + id, is2$estimateddata)
feols(y~temp-1|time + id, is2$estimateddata)

isatpanel(data = pandata_simulated,
          formula = gdp ~ temp,
          index = c("country","year"),
          effect = "twoways",
          csis = TRUE)

isatpanel(data = pandata_simulated,
          formula = gdp ~ temp,
          index = c("country","year"),
          effect = "twoways",
          engine = "fixest",
          csis = TRUE)

isatpanel(data = pandata_simulated,
          formula = gdp ~ temp,
          index = c("country","year"),
          effect = "twoways",
          engine = "felm",
          csis = TRUE)




