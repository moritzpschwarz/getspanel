library(conflicted)
library(tidyverse)
library(glmnet)
library(gets)
devtools::load_all()
conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::first)

setwd("~/GitHub/getspanel/Zenodo_code_oecd_project/Zenodo_code_oecd_project/")
load("20240515 Saving Overall LASSO FINAL.RData")


# Analyse gets models -----------------------------------------------------

overall %>%
  filter(engine == "gets") %>%
  mutate(neg_breaks = map(is, function(x){
    gets_coef <- coef(x$isatpanel.result)[get_indicators(x)$fesis$name]
    gets_coef_neg <- gets_coef[gets_coef < 0] # get only negative breaks
    length(gets_coef_neg)
  }),
  min_detectable_effect = map(is, function(x){
    gets_coef <- coef(x$isatpanel.result)[get_indicators(x)$fesis$name]
    gets_coef_neg <- gets_coef[gets_coef < 0] # get only negative breaks
    max(gets_coef_neg) # get the one closest to 0
  }),
  min_detectable_effect_abs = map(is, function(x){
    gets_coef <- coef(x$isatpanel.result)[get_indicators(x)$fesis$name]
    min(abs(gets_coef))
  })) %>%

  unnest(neg_breaks) %>%
  select(source, country_sample, neg_breaks, min_detectable_effect, min_detectable_effect_abs) -> neg_breaks_gets

overall %>%
  filter(engine == "lasso") %>%
  full_join(neg_breaks_gets, by = join_by(source, country_sample)) %>%
  mutate(new_lass_model = pmap(.l = list(is, min_detectable_effect_abs, row_number()), function(is, min_detectable_effect_abs , i){
    print(i)
    result <- threshold.coef_lasso(lasso_ob = is,
                                   absolute = TRUE,
                                   scale_min_coef = 0.5,
                                   min_coef = min_detectable_effect_abs)
    result$new_lasso_obj
  })) -> overall_threshold.coef_absolute_0.5_lasso


# plotting ---------
plot_outcome_threshold <- function(threshold_data_output, original_model_list, file_name_suffix = "_threshold_lasso.png",
                                   threshold, coef_direction = NULL, coef_scaled = 1){

  threshold_data_output %>%
    full_join(original_model_list %>%
                filter(engine == "gets") %>% select(source, country_sample, gets_obj = is)) %>%
    mutate(plt = pmap(.l = list(gets_obj, is, new_lass_model, adaptive, iis, lambda, source, row_number(), country_sample),
                      function(g,l,l_new, adaptive, iis, lambda, source, i, country_sample){
                        print(i)

                        sector <- case_when(grepl("buildings", source) ~ "Buildings",
                                            grepl("electricity", source) ~ "Electricity",
                                            grepl("industry", source) ~ "Industry",
                                            grepl("transport", source) ~ "Transport")

                        adaptive <- case_when(adaptive ~ "Adaptive LASSO",
                                              TRUE ~ "LASSO")
                        iis <- case_when(iis ~ "Using IIS",
                                         TRUE ~ "Not Using IIS")

                        gets_coef <- coef(g$isatpanel.result)[get_indicators(g)$fesis$name]
                        l_new_coef <- coef(l_new$isatpanel.result)[get_indicators(l_new)$fesis$name]
                        l_coef <- coef(l$isatpanel.result)[get_indicators(l)$fesis$name]

                        gets_coef_excl <- paste0(names(gets_coef[gets_coef > 0]), collapse = "|")
                        l_new_coef_excl <- paste0("^iis|", names(l_new_coef[l_new_coef > 0]), collapse = "|")
                        l_coef_excl <- paste0("^iis|",paste0(names(l_coef[l_coef > 0]), collapse = "|"))

                        gets_coef_excl <- if(gets_coef_excl == ""){NULL} else {gets_coef_excl}
                        l_new_coef_excl <- if(l_new_coef_excl == ""){NULL} else {l_new_coef_excl}
                        l_coef_excl <- if(l_coef_excl == ""){NULL} else {l_coef_excl}


                        threshold_subtitle <- case_when(
                          threshold == "breaks" ~ "Lambda chosen based on Number of Negative Breaks in gets",
                          threshold == "coef" & coef_direction == "min" & coef_scaled == 1 ~ "Lambda chosen based on minimum detectable effect size of Negative Breaks in gets",
                          threshold == "coef" & coef_direction == "absolute" & coef_scaled == 1 ~ "Lambda chosen based on minimum detectable effect size of all Breaks in gets",
                          threshold == "coef" & coef_direction == "min" & coef_scaled != 1 ~ paste0("Lambda chosen based on minimum detectable effect size multiplied by ",coef_scaled," of Negative Breaks in gets"),
                          threshold == "coef" & coef_direction == "absolute" & coef_scaled != 1 ~ paste0("Lambda chosen based on minimum detectable effect size multiplied by ",coef_scaled," of all Breaks in gets")
                        )

                        if(identical(names(gets_coef[gets_coef > 0]), names(gets_coef))){a <- ggplot()}
                        if(identical(names(l_new_coef[l_new_coef > 0]), names(l_new_coef))){b <- ggplot()}
                        if(identical(names(l_coef[l_coef > 0]), names(l_coef))){c <- ggplot()}

                        if(!identical(names(gets_coef[gets_coef > 0]), names(gets_coef))){a <- plot_grid(g, regex_exclude_indicators = gets_coef_excl) + labs(title = paste0(sector, "\ngets"))}
                        if(!identical(names(l_new_coef[l_new_coef > 0]), names(l_new_coef))){b <- plot_grid(l_new, regex_exclude_indicators = l_new_coef_excl) + labs(title = "Threshold LASSO", subtitle = threshold_subtitle)}
                        if(!identical(names(l_coef[l_coef > 0]), names(l_coef))){c <- plot_grid(l, regex_exclude_indicators = l_coef_excl) + labs(title = paste0(adaptive,", ", iis), subtitle = paste0("Lambda Selection: ", lambda), caption = "In all plots only negative breaks shown.")}

                        p <- cowplot::plot_grid(
                          a,b,c,
                          ncol = 1)
                        #ggsave(p,file = paste0(i,"_",sector,"_",country_sample,file_name_suffix), width = 8, height = 9)
                      }))
}

plot_outcome_threshold(overall_threshold.coef_absolute_0.5_lasso %>% filter(country_sample == "AC1", grepl("buildings",source)), overall, file_name_suffix = "_threshold.coef_absolute_0.5_lasso.png", threshold = "coef",coef_direction = "absolute", coef_scaled = 0.5)



# Finishing up the analysis for inclusion in the paper ---------------------------------------------------------

overall_threshold.coef_absolute_0.5_lasso %>%
  filter(lambda == "min", iis) %>%
  unnest(min_detectable_effect_abs) %>%
  full_join(overall %>%
              filter(engine == "gets") %>%
              select(source, country_sample, gets_obj = is)) %>%
  select(-c(p_val, year_range, b_size, ar, engine, neg_breaks, min_detectable_effect)) %>%
  select(source, country_sample, iis, adaptive, scale_by, lambda, min_detectable_effect_abs,
         gets_model = gets_obj,
         lasso_model = is,
         threshold_lasso_model = new_lass_model) -> final_lasso_modellist

save(final_lasso_modellist, file = "20240515 Final Lasso Models.RData")
