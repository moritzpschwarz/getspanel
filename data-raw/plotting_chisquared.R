library(conflicted)
library(tidyverse)
library(here)
library(data.table)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("first", "dplyr")

load(here::here("data-raw/simulations/rr2305/spec_list_new.RData"))


# data(Nile)
# a <- isat(as.numeric(Nile), ar = 1, sis=FALSE, iis=TRUE, plot=TRUE, t.pval=0.05)
#
# b <- distorttest(a)

files <- list.files(here::here("data-raw/simulations/rr2305/"),recursive = FALSE, pattern = "null")
files <- grep("norm", files, value = TRUE)
files <- grep("^FALSE", files, value = TRUE)
files <- grep("10000", files, value = TRUE)

overall <- data.table::data.table()
for(i in 1:length(files)){
  print(i)
  load(here::here("data-raw/simulations/rr2305/",files[i]))

  res %>%
    mutate(teststat = map(dist, function(x){x$statistic}),
           coef.diff = map(dist, function(x){x$coef.diff}),
           var.diff = map(dist, function(x){x$var.diff}),
           iis.coef = map(dist, function(x){cc <- coef(x$iis); names(cc) <- row.names(x$iis$mean.results); return(cc)}),
           ols.coef = map(dist, function(x){cc <- coef(x$ols); names(cc) <- row.names(x$ols$mean.results); return(cc)}),
           rel.df = map(dist, function(x){x$df}),
           var.diff = map(dist, function(x){diag(x$var.diff)})) %>%
    select(-dist) %>%
    mutate(file = files[i]) %>%
    bind_rows(overall, .) -> overall
  rm(res)
}
#
# save(overall, file = here("data-raw/simulations/full_simulation_selection_for_chisq_plotting.RData"))

#load(file = here("data-raw/simulations/full_simulation_selection_for_chisq_plotting.RData"))

overall %>%
  left_join(specs) %>%
  janitor::remove_empty(which = "cols") -> dat_short



# Test stat plot ----------------------------------------------------------

dat_short %>%
  select(sample, teststat, ar, nreg, rel.df) %>%
  filter(nreg == 5, ar == 0) %>%
  unnest(teststat) %>%
  unnest(rel.df) %>%
  tidyr::pivot_longer(cols = -c(sample, ar, nreg, rel.df)) %>%
  mutate(sample = as.character(sample)) -> plot_df



# # plot 1
# plot_df %>%
#   filter(nreg == 1) %>%
#   ggplot() +
#   aes(x = value, color = sample) +
#   #geom_density(linewidth = 1) +
#   scale_color_viridis_d(name = "Sample") +
#   labs(y = "Density", x = "Test Statistic",
#        title = "Test Statistic Distribution under the Null",
#        subtitle = "10,000 Replications. 1 Regressors. Normal Distribution. AR = 0.\nDashed is Chi-sq with df = 2.") +
#   theme_minimal() +
#   geom_hline(aes(yintercept = 0)) +
#   stat_function(fun = dchisq, args = list(df = 2), #6),
#                 linewidth = 0.5, linetype = 2, color = "black", inherit.aes = FALSE,
#                 aes(x = .data$value))-> p1
#
# plot_df %>%
#   filter(nreg == 5, ar == 0) %>%
#   ggplot() +
#   aes(x = value, color = sample) +
#   geom_density(linewidth = 1) +
#   scale_color_viridis_d(name = "Sample") +
#   labs(y = "Density", x = "Test Statistic",
#        title = "Test Statistic Distribution under the Null",
#        subtitle = "10,000 Replications. 5 Regressors. Normal Distribution. AR = 0.\nDashed is Chi-sq with df = 6.") +
#   theme_minimal() +
#   stat_function(fun = dchisq, args = list(df = 6),
#                 linewidth = 0.5, linetype = 2, color = "black", inherit.aes = FALSE,
#                 aes(x = .data$value)) +
#   geom_hline(aes(yintercept = 0))  -> p2
#
#
# plot_df %>%
#   filter(nreg == 5, ar == 0.5) %>%
#   ggplot() +
#   aes(x = value, color = sample) +
#   geom_density(linewidth = 1) +
#   scale_color_viridis_d(name = "Sample") +
#   labs(y = "Density", x = "Test Statistic",
#        title = "Test Statistic Distribution under the Null",
#        subtitle = "10,000 Replications. 5 Regressors. Normal Distribution. AR = 0.5.\nDashed is Chi-sq with df = 6.") +
#   theme_minimal() +
#   stat_function(fun = dchisq, args = list(df = 6),
#                 linewidth = 0.5, linetype = 2, color = "black", inherit.aes = FALSE,
#                 aes(x = .data$value)) +
#   geom_hline(aes(yintercept = 0)) -> p3
#
# plot_df %>%
#   filter(nreg == 5, ar == 0.9) %>%
#   ggplot() +
#   aes(x = value, color = sample) +
#   geom_density(linewidth = 1) +
#   scale_color_viridis_d(name = "Sample") +
#   labs(y = "Density", x = "Test Statistic",
#        title = "Test Statistic Distribution under the Null",
#        subtitle = "10,000 Replications. 5 Regressors. Normal Distribution. AR = 0.9.\nDashed is Chi-sq with df = 6.") +
#   theme_minimal() +
#   stat_function(fun = dchisq, args = list(df = 6),
#                 linewidth = 0.5, linetype = 2, color = "black", inherit.aes = FALSE,
#                 aes(x = .data$value)) +
#   geom_hline(aes(yintercept = 0)) -> p4
#
# library(cowplot)
# plot_grid(p1, p2, p3, p4)


plot_df %>%
  ggplot() +
  aes(x = value, color = sample) +
  #geom_histogram(bins = 1000) +
  geom_density(linewidth = 1) +
  scale_color_viridis_d(name = "Sample") +
  labs(y = "Density", x = "Test Statistic", title = "Test Statistic Distribution under the Null",
       subtitle = bquote('10,000 Replications. 5 Regressors. Normal Distribution. '*rho * {phantom() == phantom()} * 0*'. Dashed line is '*chi[df * {phantom() == phantom()} * 6]^{2}*".")) +
  theme_minimal() +
  stat_function(fun = dchisq, args = list(df = 6), #6),
                linewidth = 0.5, linetype = 2, color = "black", inherit.aes = FALSE,
                aes(x = .data$value)) +
  geom_hline(aes(yintercept = 0)) -> p

ggsave(plot = p,here("data-raw/figures/rr2305/Chisq_teststat.png"), dpi = 1000, width = 7, height = 5, bg ="white")
ggsave(plot = p,here("data-raw/figures/rr2305/Chisq_teststat.pdf"), width = 7, height = 5, bg ="white")






# Plotting the IIS Estimator ------------------------------

dat_short %>%
  select(sample, nreg, iis.coef, ar) %>%
  mutate(iis.coef = map(iis.coef, function(x){

    x_short <- x[!grepl("iis[0-9]+",names(x))]
    x_short_names <- names(x_short)

    tibble(var = x_short_names,
           coef = x_short)

  })) %>% # +1 for mconst
  unnest(iis.coef) %>%
  mutate(sample = as.character(sample)) -> plot_df



plot_df %>%
  filter(nreg > 1) %>%
  mutate(var = case_when(ar != 0 & var =="ar1" ~ paste0(var,"_",as.character(ar)),
                         TRUE ~ var)) %>%
  left_join(tibble(true_param = c(0,0.5, 0.75, 0.9, rep(0.5,11)),
                   var = c("mconst", "ar1_0.5", "ar1_0.75", "ar1_0.9",
                           "mxreg", "x1", "x2", "x3", "x4", "x5",
                           "mxreg1", "mxreg2", "mxreg3", "mxreg4", "mxreg5"))) %>%

  mutate(coef = as.numeric(sample)^0.5 * (coef - true_param),
         var = gsub("mxreg","x",var),
         var = gsub("ar1_0.[0-9]+","ar1",var)) -> plot_df_ready


t.pval = 0.05
alpha <-   t.pval
c <- abs(qnorm(alpha/2))
#c <- 1.96
fc <- dnorm(c)
gamma_c <- 2 * (1 - pnorm(c))
psi_c <- 1 - gamma_c
tau_2c <- psi_c - 2*c*fc

numerator <- 4*(c^2)*(fc^2) + 4*tau_2c*c*fc + tau_2c
denominator <- psi_c^2
ratio <- numerator/denominator

# plot_df_ready %>%
#   ggplot() +
#   aes(x = coef, color = sample) +
#   #geom_histogram(bins = 1000) +
#   geom_density(linewidth = 1) +
#   scale_color_viridis_d(name = "Sample") +
#   facet_grid(ar~var, scales = "free") +
#   stat_function(fun = dnorm, args = list(mean = 0, sd = sqrt(ratio)),
#                 linewidth = 0.5, linetype = 2, color = "black") +
#   labs(y = "Density", x = bquote(n^{1 / 2}*(widehat(beta)[c]^{(1)} - beta)),title = "IIS Estimator under the Null",
#        subtitle = "10,000 Replications. 5 Regressors. Normal Distribution. AR = 0.\nDashed is Normal Distribution with appropriate Variance.") +
#   theme_minimal() +
#   geom_hline(aes(yintercept = 0)) -> p
# ggsave(plot = p,here("data-raw/figures/rr2305/IIS_wAR.png"), dpi = 1000, width = 7, height = 5, bg ="white")

plot_df_ready %>%

  filter(var != "ar1") %>%
  mutate(ar_lab = factor(ar, labels = c(paste0("rho * {phantom() == phantom()} * ",c(0, 0.5, 0.9)))),
         var = factor(var, labels = c("Intercept", "x[1]", "x[2]", "x[3]", "x[4]", "x[5]"))) %>%
  ggplot() +
  aes(x = coef, color = sample) +
  #geom_histogram(bins = 1000) +
  geom_density(linewidth = 1) +
  scale_color_viridis_d(name = "Sample") +
  facet_grid(ar_lab~var, scales = "free",
             #labeller = labeller(.rows = function(x){latex2exp::TeX(paste0("$\\rho = ",x,"$"))})) +
             labeller = label_parsed) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = sqrt(ratio)),
                linewidth = 0.5, linetype = 2, color = "black") +
  labs(y = "Density", x = bquote(n^{1 / 2}*(widehat(beta)[c]^{(1)} - beta)),title = "IIS Estimator under the Null",
       subtitle = "10,000 Replications. 5 Regressors. Normal Distribution.\nDashed is Normal Distribution with corresponding Variance.") +
  theme_minimal() +
  geom_hline(aes(yintercept = 0)) -> p
ggsave(plot = p,here("data-raw/figures/rr2305/IIS_woAR.png"), dpi = 1000, width = 7, height = 5, bg ="white")
ggsave(plot = p,here("data-raw/figures/rr2305/IIS_woAR.pdf"), width = 7, height = 5, bg ="white")





# plots only used in development - not used -------------------------------
# Coef Diff --

# dat_short %>%
#   select(sample, coef.diff, ar, nreg, rel.df, var.diff) %>%
#   mutate(coef.diff = map2(coef.diff, var.diff, function(x, y){
#     nam <- names(x)
#     tibble(xvarname = nam,
#            coef.diff = x,
#            var = sqrt(y))}),
#     var.diff = NULL) %>%
#   unnest(coef.diff) %>%
#   unnest(rel.df) %>%
#   mutate(sample = as.character(sample)) -> plot_df


dat_short %>%
  select(sample, iis.coef) %>%
  mutate(iis.coef = map(iis.coef, function(x){
    nam <- names(x)
    tibble(xvarname = nam,
           iis.coef = "mxreg",
           iis.coef.diff = 0.5-x["mxreg"])})) %>%
  unnest(iis.coef) %>%
  mutate(sample = as.character(sample)) -> plot_df

plot_df %>%
  select(-xvarname) %>%
  distinct %>%







  dat_short %>%
  select(sample, coef.diff, ar, nreg, rel.df) %>%
  mutate(coef.diff = map(coef.diff, function(x){
    nam <- names(x)
    tibble(xvarname = nam,
           coef.diff = x)})) %>%
  unnest(coef.diff) %>%
  unnest(rel.df) %>%
  mutate(sample = as.character(sample)) -> plot_df


plot_df %>%
  mutate(xvarname = gsub("mxreg","x",xvarname)) %>%
  #slice(1:1000) %>%
  ggplot() +
  aes(x = coef.diff, after_stat(scaled), color = sample) +
  #geom_histogram(bins = 1000) +
  geom_density(linewidth = 1) +
  stat_function(fun = dnorm, args = list(sd = sqrt(0.03)), #6),
                linewidth = 0.5, linetype = 2, color = "black", inherit.aes = FALSE,
                aes(x = .data$coef.diff, after_stat(scaled))) +

  scale_color_viridis_d(name = "Sample") +
  facet_grid(ar~xvarname, scales = "free") +
  labs(y = "Density", x = "Coefficient Difference",title = "Coefficient Difference Distribution under the Null",
       subtitle = "10,000 Replications. 5 Regressors. Normal Distribution. AR = 0.\nChi-sq with df = 6.") +
  theme_minimal() +
  geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) -> p
ggsave(plot = p,here("data-raw/figures/rr2305/Difference.png"), dpi = 1000, width = 7, height = 5, bg ="white")








plot_df %>%
  mutate(xvarname = gsub("mxreg","x",xvarname)) %>%
  #slice(1:1000) %>%
  ggplot() +
  aes(x = var, color = sample) +
  #geom_histogram(bins = 1000) +
  geom_density(linewidth = 1) +
  facet_grid(ar~xvarname, scales = "free")





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
  select(sample, nreg, iis.coef) %>%
  mutate(iis.coef = map2(iis.coef, nreg, function(x,y){
    if(y == 5){
      var =  c("mconst", "x1", "x2", "x3", "x4", "x5")
      coef = x[1:(y+1)] - c(0,rep(0.5,5))
    }
    if(y == 1){
      var =  c("mconst", "mxreg")
      coef = x[1:(y+1)] - c(0,rep(0.5,5))}

    tibble(var = var,
           coef = coef)})) %>% # +1 for mconst
  unnest(iis.coef) %>%
  mutate(sample = as.character(sample)) -> plot_df


plot_df %>%
  filter(var == "mxreg") %>%
  mutate(true_coef = ifelse(var == "mconst", 0, 0.5)) %>%
  ggplot() +
  geom_histogram(aes(coef, y = after_stat(count)), fill='darkred') +
  stat_function(fun=dnorm, args= list(mean=0.5, sd = .1))

plot_df %>%
  filter(var == "mxreg") %>%
  mutate(true_coef = ifelse(var == "mconst", 0, 0.5)) %>%
  ggplot() +

  #geom_histogram(bins = 1000, fill = "red") +
  geom_density(aes(x = coef, y = after_stat(density), color = sample), linewidth = 1) +
  scale_color_viridis_d(name = "Sample") +
  stat_function(fun = dnorm, args = list(mean = 0.5, sd = .05), inherit.aes = FALSE,
                aes(x = .data$coef))








stat_function(fun = dnorm, args = list(mean = 0.5, sd = sqrt(0.1)), #6),
              linewidth = 0.5, linetype = 2, color = "black", inherit.aes = FALSE,
              aes(x = .data$coef, after_stat(scaled)))
labs(y = "Density", x = "IIS Coefficient",title = "IIS Coefficient Distribution under the Null",
     subtitle = "10,000 Replications. 5 Regressors. Normal Distribution. AR = 0.\nChi-sq with df = 6.") +
  theme_minimal() +
  geom_vline(data = tibble(var = c("mconst", "x1", "x2", "x3", "x4", "x5"),
                           intercept = c(0, rep(0.5,5))),
             aes(xintercept = intercept)) +
  geom_hline(aes(yintercept = 0)) -> p


####
####
####
###




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
