library(tidyverse)
library(here)
load(here::here("data-raw/simulations/rr2305/spec_list.RData"))


files <- list.files(here::here("data-raw/simulations/rr2305/"),recursive = FALSE, pattern = "null")
files <- grep("norm", files, value = TRUE)
files <- grep("^FALSE", files, value = TRUE)
files <- grep("10000", files, value = TRUE)

overall <- tibble()
for(i in 1:5){
  print(i)
  load(here::here("data-raw/simulations/rr2305/",files[i]))

  res %>%
    mutate(teststat = map(dist, function(x){x$statistic}),
           coef.diff = map(dist, function(x){x$coef.diff}),
           var.diff = map(dist, function(x){x$var.diff}),
           iis.coef = map(dist, function(x){coef(x$iis)}),
           ols.coef = map(dist, function(x){coef(x$ols)}),
           rel.df = map(dist, function(x){x$df})) %>%
    select(-dist) %>%
    bind_rows(overall, .) -> overall
}

overall %>%
  left_join(specs) -> dat

dat$sample <- rep(c(100, 200, 300, 400, 500), each = 10000)

dat_short <- janitor::remove_empty(dat, which = "cols")



library(tidyverse)

dat_short %>%
  select(sample, teststat) %>%
  unnest(teststat) %>%
  tidyr::pivot_longer(cols = -sample) %>%
  mutate(sample = as.character(sample)) %>%
  ggplot() +
  aes(x = value, color = sample) +
  #geom_histogram(bins = 1000) +
  geom_density(linewidth = 1) +
  scale_color_viridis_d(name = "Sample") +
  labs(y = "Density", x = "Test Statistic", title = "Test Statistic Distribution under the Null",
       subtitle = "10,000 Replications. 5 Regressors. Normal Distribution. AR = 0.\nChi-sq with df = 6.") +
  theme_minimal() +
  stat_function(fun = dchisq, args = list(df = 6), linewidth = 0.5, linetype = 2, color = "black", inherit.aes = FALSE,
                aes(x = .data$value)) +
  geom_hline(aes(yintercept = 0)) -> p

ggsave(plot = p,here("data-raw/figures/rr2305/Chisq_teststat.png"), dpi = 1000, width = 7, height = 5, bg ="white")

dat_short %>%
  select(sample, coef.diff) %>%
  mutate(coef.diff = map(coef.diff, function(x){
    n <- names(x)
    tibble(var = n,
           coef = x)})) %>%
  unnest(coef.diff) %>%
  mutate(sample = as.character(sample)) -> plot_df

plot_df %>%
  ggplot() +
  aes(x = coef, color = sample) +
  #geom_histogram(bins = 1000) +
  geom_density(linewidth = 1) +
  scale_color_viridis_d(name = "Sample") +
  facet_wrap(~var, scales = "free") +
  labs(y = "Density", x = "Coefficient Difference",title = "Coefficient Difference Distribution under the Null",
       subtitle = "10,000 Replications. 5 Regressors. Normal Distribution. AR = 0.\nChi-sq with df = 6.") +
  theme_minimal() +
  geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) -> p
ggsave(plot = p,here("data-raw/figures/rr2305/Difference.png"), dpi = 1000, width = 7, height = 5, bg ="white")



dat_short %>%
  select(sample, ols.coef) %>%
  mutate(ols.coef = map(ols.coef, function(x){
    tibble(var = c("mconst", "x1", "x2", "x3", "x4", "x5"),
           coef = x)})) %>%
  unnest(ols.coef) %>%
  mutate(sample = as.character(sample)) -> plot_df

plot_df %>%
  ggplot() +
  aes(x = coef, color = sample) +
  #geom_histogram(bins = 1000) +
  geom_density(linewidth = 1) +
  scale_color_viridis_d(name = "Sample") +
  facet_wrap(~var, scales = "free") +
  labs(y = "Density", x = "OLS Coefficient",title = "OLS Coefficient Distribution under the Null",
       subtitle = "10,000 Replications. 5 Regressors. Normal Distribution. AR = 0.\nChi-sq with df = 6.") +
  theme_minimal() +
  geom_vline(data = tibble(var = c("mconst", "x1", "x2", "x3", "x4", "x5"),
                           intercept = c(0, rep(0.5,5))),
             aes(xintercept = intercept)) +
  geom_hline(aes(yintercept = 0)) -> p
ggsave(plot = p,here("data-raw/figures/rr2305/OLS.png"), dpi = 1000, width = 7, height = 5, bg ="white")



dat_short %>%
  select(sample, iis.coef) %>%
  mutate(iis.coef = map(iis.coef, function(x){
    tibble(var = c("mconst", "x1", "x2", "x3", "x4", "x5"),
           coef = x[1:6])})) %>%
  unnest(iis.coef) %>%
  mutate(sample = as.character(sample)) -> plot_df

plot_df %>%
  ggplot() +
  aes(x = coef, color = sample) +
  #geom_histogram(bins = 1000) +
  geom_density(linewidth = 1) +
  scale_color_viridis_d(name = "Sample") +
  facet_wrap(~var, scales = "free") +
  labs(y = "Density", x = "IIS Coefficient",title = "IIS Coefficient Distribution under the Null",
       subtitle = "10,000 Replications. 5 Regressors. Normal Distribution. AR = 0.\nChi-sq with df = 6.") +
  theme_minimal() +
  geom_vline(data = tibble(var = c("mconst", "x1", "x2", "x3", "x4", "x5"),
                           intercept = c(0, rep(0.5,5))),
             aes(xintercept = intercept)) +
  geom_hline(aes(yintercept = 0)) -> p
ggsave(plot = p,here("data-raw/figures/rr2305/IIS.png"), dpi = 1000, width = 7, height = 5, bg ="white")



dat_short %>%
  dplyr::select(is.dist1.p, is.prop.test.p, is.avg.dist.pct, is.euclid, sample) %>%
  tidyr::pivot_longer(cols = -sample) %>%
  mutate(sample = as.character(sample)) %>%
  ggplot() +
  aes(x = value, color = sample, group = sample) +
  geom_histogram(bins = 1000) +
  facet_wrap(~name, scales = "free")


dat_short %>%
  dplyr::select(is.euclid, sample) %>%
  tidyr::pivot_longer(cols = -sample) %>%
  mutate(sample = as.character(sample)) %>%
  ggplot() +
  aes(x = value, color = sample, group = sample) +
  stat_function(fun = dchisq, args = list(x = c(0.1, .8, .9, 10), df = 1)) +
  geom_histogram(bins = 1000) +
  facet_wrap(~sample, scales = "free") +
  stat_function(fun = dchisq, args = list(x = 1, df = 1))
#stat_function(fun = dchisq, args = list(mean = mean(~value, na.rm = TRUE), sd = sd(~value, na.rm = TRUE)))





ggplot(data.frame(value = seq(0,10,0.2))) +
  aes(x = value) +
  stat_function(fun = dchisq, args = list(x = c(1, 10), df = 10))
