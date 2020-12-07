

# First run your isatpanel and then use the object below

library(plm)
library(tidyverse)
library(lmtest)
library(sandwich)

# This just defines a function that allows you to recreate the original dataset
# By this, I mean that it gives you back the "country" and "id" columns from the dummy variables of the isat object
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

# This is calibrated to Austria being the first country in the sample and the first country being 1995
# If this is not the case, then please adjust the arguments of the function
df <- prepare_robust(is1)

# Then we create a pdata.frame with the plm package
df %>%
  pdata.frame(index = c("country","year")) -> pdata

# Let's look at the plm output
formula <- as.formula(paste0("y ~ ",names(pdata[,!names(pdata) %in% c("y","year","country")]) %>% paste0(collapse = " + ")))
plm(formula = formula,data = pdata, effect = "twoways",model = "within") -> plm_object
summary(plm_object)
# This should be exactly the same output as the one from isatpanel


# Now let's do some SE testing!!

# The two first tests control for heterogeneity

# HC0 is White SE
coeftest(plm_object, vcov=vcovHC(plm_object,type="HC0"))
coeftest(plm_object, vcov=vcovHC(plm_object,type="HC0",cluster="group"))
coeftest(plm_object, vcov=vcovHC(plm_object,type="HC0",cluster="time")) # doesn't make much sense, but for completeness

# HC3 Long + Ervin
coeftest(plm_object, vcov=vcovHC(plm_object,type="HC3"))
coeftest(plm_object, vcov=vcovHC(plm_object,type="HC3",cluster="group"))
coeftest(plm_object, vcov=vcovHC(plm_object,type="HC3",cluster="time")) # doesn't make much sense, but for completeness



# The following also for autocorrelation
# Heteroskedasticity- and autocorrelation-consistent (HAC)
coeftest(plm_object, vcovHC(plm_object, method = "arellano"))

lm_mod <- lm(y~.,pdata)
coeftest(lm_mod, vcov=vcovHAC(lm_mod, cluster="individual"))

# All of the below are unlikely to affect the S.E. significantly, but for completeness
coeftest(lm_mod, vcov=vcovHAC(lm_mod,prewhite = FALSE,cluster = "individual"))
coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1,cluster = "individual"))
coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1,cluster = "time"))

coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 3,cluster = "individual"))
coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 3,cluster = "time"))

coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1:3,cluster = "individual"))
coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1:3,cluster = "time"))


coeftest(lm_mod, vcov=vcovHAC(lm_mod,lag = 1:3,cluster = c("individual","time")))
