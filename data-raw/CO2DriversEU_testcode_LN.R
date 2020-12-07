
rm(list = ls())
library(gets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(data.table)

devtools::load_all(".")

#source("isatpanel_v9.R")


set.seed(123)


data <- read.csv("data-raw/CO2DriversEU_dataset_v1.csv")
data$lgdp_sq <- data$lgdp^2


data <- as.data.table(data)
data$transport.emissions_pc <- data$transport.emissions/data$pop
data$ltransport.emissions_pc <- log(data$transport.emissions_pc)
data$growthtransport.emissions_pc <- log(data$transport.emissions_pc/exp(data$ltransport.emissions_pc))

EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg",
          "Netherlands", "Greece", "Portugal", "Sweden")
EU31 <- c(EU15, "Croatia", "Bulgaria", "Cyprus","Czech Republic", "Estonia",
          "Hungary", "Lithuania", "Latvia", "Malta", "Poland", "Romania",
          "Slovak Republic", "Slovenia", "Switzerland", "Iceland",
          "Norway")


###### Analysis:


# Specify parameters:
syear <- 1995
runit <- "Austria"


for(sample in list(EU15)){
  dat <- filter(data, country %in% sample, year>=syear)

  for(dv in list(dat$ltransport.emissions_pc)){

    # Specify control variables:
    controls <- data.frame(dat %>% select(lgdp,lgdp_sq))
    # Break analysis:
    is2 <- isatpanel(
      y=dv,
      id=dat$country,
      time=dat$year,
      mxreg=controls,
      mxbreak=c(dat$const),
      break.method="both",
      effect="twoways",
      engine = "fixest",
      iis=TRUE,
      t.pval=0.01)

    # Output analysis results:
    print(is2)

  }
}

library(plm)
library(tidyverse)
library(lmtest)
library(sandwich)

data %>%
  select(country,year) %>%
  filter(country %in% EU15) %>%
  filter(year>=1995) %>%
  mutate(country = as.factor(country),
         year = as.factor(year)) -> input_df
input_df %>%
  bind_cols(.,y = is2$aux$y) %>%
  bind_cols(.,is2$aux$mX %>% as_tibble) -> new_data




new_data %>%
  lm(y~.-1,data = .) %>% summary

formula_felm_uncl <- paste("y ~ ",paste0(new_data %>% select(-country,-year,-y) %>% names,collapse = " + "),"| country + year | 0 | 0 ") %>% as.formula()
lfe::felm(formula = formula_felm_uncl,new_data) %>%
  summary


formula_felm_clu <- paste("y ~ ",paste0(new_data %>% select(-country,-year,-y) %>% names,collapse = " + "),"| country + year | 0 | country ") %>% as.formula()
lfe::felm(formula = formula_felm_clu,new_data) %>%
  summary

# Fixest Package (same as used in isat)
formula_fixest_clu <- paste("y ~ ",paste0(new_data %>% select(-country,-year,-y) %>% names,collapse = " + "),"| country + year") %>% as.formula()
fixest::feols(fml = formula_fixest_clu,new_data) %>%
  summary







# Plotting


plot(is2)



new_data %>%
  select(starts_with("iis"))

cbind(new_data,fitted = is2$fit) %>%
  ggplot(aes(y=fitted,x=year,group=country)) +
  geom_line() +
  facet_wrap(~country) +
  theme(legend.position = "none")














data %>%
  select(country,year) %>%
  filter(country %in% EU15) %>%
  filter(year>=1995) %>%
  mutate(country = as.factor(country),
         year = as.factor(year)) %>%
  bind_cols(.,y = is2$aux$y) %>%
  bind_cols(.,is2$aux$mX %>% as_tibble) %>%
  lm(y~.-1,data = .) %>% summary




prepare_robust <- function(isat_object,
                           groupdummyprefix = "id",
                           timedummyprefix = "time",
                           grouplabel = "country",
                           timelabel = "year",
                           firstgroup = "Austria",
                           firsttime = 1995) {

  isat_object %>%
    as.lm %>%
    with(model) %>%
    mutate(newgroup = ifelse(rowSums(cur_data() %>% select(starts_with("id")))==1,0,1),
           newtime = ifelse(rowSums(cur_data() %>% select(starts_with("time")))==1,0,1)) %>%

    rename(!!paste0(groupdummyprefix,firstgroup):=newgroup,
           !!paste0(timedummyprefix,firsttime):=newtime) %>%


    pivot_longer(cols = starts_with(groupdummyprefix),names_to = grouplabel,names_prefix = groupdummyprefix) %>%
    filter(value == 1) %>%
    select(-value) %>%

    pivot_longer(cols = starts_with(timedummyprefix),names_to = timelabel,names_prefix = timedummyprefix) %>%
    filter(value == 1) %>%
    select(-value)  %>%

    mutate(across(.cols = c(all_of(grouplabel),all_of(timelabel)),factor)) %>%
    relocate(all_of(c(grouplabel,timelabel))) -> output

  return(output)

}

df <- prepare_robust(is2)

df %>%
  pdata.frame(index = c("country","year")) -> pdata

formula <- as.formula(paste0("y ~ ",names(pdata[,!names(pdata) %in% c("y","year","country")]) %>% paste0(collapse = " + ")))
plm(formula = formula,data = pdata, effect = "twoways",model = "within") -> plm_object
summary(plm_object)


# HC0 is White SE
coeftest(plm_object, vcov=vcovHC(plm_object,type="HC0"))
coeftest(plm_object, vcov=vcovHC(plm_object,type="HC0",cluster="group"))
coeftest(plm_object, vcov=vcovHC(plm_object,type="HC0",cluster="time")) # doesn't make much sense, but for completeness

# HC3 Long + Ervin
coeftest(plm_object, vcov=vcovHC(plm_object,type="HC3"))
coeftest(plm_object, vcov=vcovHC(plm_object,type="HC3",cluster="group"))
coeftest(plm_object, vcov=vcovHC(plm_object,type="HC3",cluster="time")) # doesn't make much sense, but for completeness


# Heteroskedasticity- and autocorrelation-consistent (HAC)
coeftest(plm_object, vcovHC(plm_object, method = "arellano"))

lm_mod <- lm(y~.,pdata)
coeftest(lm_mod, vcov=vcovHAC(lm_mod, cluster="individual"))
coeftest(lm_mod, vcov=vcovHAC(lm_mod,prewhite = FALSE,cluster = "individual"))
coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1,cluster = "individual"))
coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1,cluster = "time"))

coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 3,cluster = "individual"))
coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 3,cluster = "time"))

coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1:3,cluster = "individual"))
coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1:3,cluster = "time"))


coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1:3,cluster = c("individual","time")))



