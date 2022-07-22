
# library(remotes)
# remotes::install_github('vincentarelbundock/modelsummary')

devtools::load_all()


library(tidyverse)
library(here)
library(modelsummary)
library(gets)
library(kableExtra)



rm(list=ls())
select <- dplyr::select


# CHECK MODELSUMMARY VERSION ----
#'
#' This does not work with the most recent modelsummary version.
#' We need version 0.9.6
#'
#'packageurl <- "https://cran.r-project.org/src/contrib/Archive/modelsummary/modelsummary_0.8.1.tar.gz"
#'install.packages(packageurl, repos=NULL, type="source")

if(packageVersion("modelsummary") != "0.9.6"){stop("STOP!! Modelsummary Version must be 0.9.6 for this to work.")}

#'
#'
#'



for(sig_level in c(0.01, 0.05, 0.001)){
  print(sig_level)
  file_list <- list.files(here("data-raw/projections"),pattern = ".RData", full.names = TRUE)
  file_list <- c(here("data-raw/projections/m2.RData"),
                 here("data-raw/projections/am2.RData"),
                 here("data-raw/projections/am2_L1.RData"),
                 file_list[grepl(sig_level,file_list)])
  if(exists("m2")){rm(m2,am2,am2_L1,am2.isat,m2.isat, am2.isat_L1)}
  lapply(file_list, load, envir=.GlobalEnv)


  # Main Table ----


  base_vars <- grep("temp|prcp",names(coef(m2.isat)), value = TRUE)
  diffs <- vector()
  stats <- vector()
  pvals <- vector()
  for(i in base_vars){
    #diffs <- append(diffs,distorttest(m2.isat, coef = i)$coef.diff)
    running_test <- distorttest(m2.isat, coef = i)
    stats <- append(stats,running_test$statistic)
    pvals <- append(pvals,running_test$p.value)
  }

  ti1 <- tibble(
    term = base_vars,
    estimate = stats,
    std.error = pvals,
    p.value = rep(1,4)
  )

  adapt_vars <- grep("temp|prcp",names(coef(am2.isat)), value = TRUE)
  diffs <- vector()
  stats <- vector()
  pvals <- vector()
  for(i in adapt_vars){
    running_test <- distorttest(am2.isat, coef = i)
    #diffs <- append(diffs,distorttest(am2.isat, coef = i)$coef.diff)
    stats <- append(stats,running_test$statistic)
    pvals <- append(pvals,running_test$p.value)
  }

  ti2 <- tibble(
    term = grep("temp|prcp",names(coef(am2.isat)), value = TRUE),
    estimate = stats,
    std.error = pvals,
    p.value = rep(1,8)
  )



  # gl <- data.frame(
  #   stat1 = "blah",
  #   stat2 = "blah blah")

  # a <- distorttest(m2.isat)
  # b <- distorttest(am2.isat)


  overall_test_base <- distorttest(m2.isat, coef = c("temp","temp_2"))
  overall_test_adapt <- distorttest(am2.isat, coef = c("temp","temp_2", "temp_int","temp_2_int"))


  gof <- c("sigma", "statistic", "p.value", "df",
           "logLik", "BIC", "deviance", "df.residual",
           "nobs", "outlier_teststat",
           "outlier_pval", "outlier_no")

  matrix("",ncol = length(gof)) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(gof) %>%
    mutate(outlier_teststat = overall_test_base$statistic,
           outlier_pval = overall_test_base$p.value,
           outlier_no = length(overall_test_base$iis$ISnames))-> gl1



  matrix("",ncol = length(gof)) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(gof) %>%
    mutate(outlier_teststat = overall_test_adapt$statistic,
           outlier_pval = overall_test_adapt$p.value,
           outlier_no = length(overall_test_adapt$iis$ISnames))-> gl2



  mod1 <- list(
    tidy = ti1,
    glance = gl1)
  class(mod1) <- "modelsummary_list"

  mod2 <- list(
    tidy = ti2,
    glance = gl2)
  class(mod2) <- "modelsummary_list"


  #modelsummary(list(mod1,mod2), coef_rename = name_vec, escape = FALSE, stars = TRUE)


  m2.isat %>% as.lm -> m2.isat_lm
  am2.isat %>% as.lm -> am2.isat_lm


  list(
    "Base"  = m2,
    "Base IIS"  = m2.isat_lm,
    "Base Outlier Distortion Test" = mod1,


    "Adaptation" = am2,
    "Adaptation IIS" = am2.isat_lm,
    "Adaptation Outlier Distortion Test" = mod2) -> modellist

  # The lag(GDP) goes into the appendix
  #"Adaptation Lag(GDP_pc)" = am2_L1,
  #"Adaptation Lag(GDP_pc) IIS" = am2.isat_L1


  names(coef(am2.isat)) %>%
    grep(x = .,pattern = "temp|prcp", value = TRUE) -> name_vec

  names(name_vec) <- name_vec
  name_vec <- gsub("temp","Temperature", name_vec)
  name_vec <- gsub("prcp","Precipitation", name_vec)
  name_vec <- gsub("_2","<sup>2</sup>", name_vec)
  name_vec <- gsub("_int"," x GDPpc", name_vec)

  name_vec_tex <- gsub("<sup>2</sup>","\\\\textsuperscript{2}",name_vec)
  name_vec_tex <- gsub("pc","\\\\textsubscript{pc}",name_vec_tex)



  gof_map_mod <- data.frame(raw = c("outlier_no", "outlier_teststat", "outlier_pval"),
                            clean = c("Num. Outliers",
                                      "Outlier Distortion test statistic for Temp. Variables",
                                      ""),
                            fmt = c(0,2,4))

  gof_map_mod <- rbind(gof_map_mod, gof_map[grepl("nobs|BIC|logLik",gof_map$raw),1:3])


  modelsummary(modellist,
               coef_rename = name_vec_tex,
               stars = c('*' = .05, '**' = .01, '***' = 0.001),
               fmt = 5,
               gof_map = gof_map_mod,
               coef_omit = "time|year|iso|iis",
               gof_omit = "AIC|F|R",
               escape = FALSE,
               add_rows = data.frame("Two Way Fixed Effects", "Yes","Yes","","Yes","Yes",""),
               output = "modelsummary_list",
               #output = here("data-raw/projections/out/models.tex"),
               notes = "For difference columns, test statistics are in brackets. Standard Errors in brackets of the other columns."
  ) -> intermediate_list


  options(scipen = 5)
  modify_brackets <- function(modelsummary_list){
    for(i in 1:length(modelsummary_list)){
      modelsummary_list[[i]]$tidy %>%
        {if(!i %in% c(3,6)){
          mutate(.,std.error = paste0("(",round(std.error,5),")"))
        } else{
          rowwise(.) %>%
            mutate(.,
                   estimate = as.character(round(estimate,2)),
                   std.error = paste0("[",format(round(std.error,3), nsmall = 3),"]"),
                   std.error = ifelse(std.error == "[0.000]","[$<$0.001]",std.error))
        }} -> modelsummary_list[[i]]$tidy
    }
    return(modelsummary_list)
  }


  intermediate_list_modified <- modify_brackets(intermediate_list)

  teststat_base <- intermediate_list_modified$`Base Outlier Distortion Test`$glance$outlier_teststat
  teststat_adap <- intermediate_list_modified$`Adaptation Outlier Distortion Test`$glance$outlier_teststat

  testpval_base <- intermediate_list_modified$`Base Outlier Distortion Test`$glance$outlier_pval
  testpval_adap <- intermediate_list_modified$`Base Outlier Distortion Test`$glance$outlier_pval

  ## Add Chi-sq sign to the line

  intermediate_list_modified$`Base Outlier Distortion Test`$glance$outlier_teststat <- paste0("$\\chi^2_{",overall_test_base$df,"}$ = ",round(teststat_base, 2))
  intermediate_list_modified$`Adaptation Outlier Distortion Test`$glance$outlier_teststat <- paste0("$\\chi^2_{",overall_test_adapt$df,"}$ = ",round(teststat_adap, 2))


  # Format outlier test stat p value
  intermediate_list_modified$`Base Outlier Distortion Test`$glance$outlier_pval <- paste0("[", ifelse(
    format(round(testpval_base, 3), nsmall = 3) == "0.000",
    "$<$0.001",
    format(round(testpval_base, 3), nsmall = 3)), "]")

  intermediate_list_modified$`Adaptation Outlier Distortion Test`$glance$outlier_pval <- paste0("[", ifelse(
    format(round(testpval_adap, 3), nsmall = 3) == "0.000",
    "$<$0.001",
    format(round(testpval_adap, 3), nsmall = 3)), "]")



  #Put both outlier test stat and p-value in the same cell
  intermediate_list_modified$`Base Outlier Distortion Test`$glance$outlier_teststat <-
    paste0(intermediate_list_modified$`Base Outlier Distortion Test`$glance$outlier_teststat,"\n",intermediate_list_modified$`Base Outlier Distortion Test`$glance$outlier_pval)

  intermediate_list_modified$`Adaptation Outlier Distortion Test`$glance$outlier_teststat <-
    paste0(intermediate_list_modified$`Adaptation Outlier Distortion Test`$glance$outlier_teststat,"\n",intermediate_list_modified$`Adaptation Outlier Distortion Test`$glance$outlier_pval)

  # Delete the pvalue information
  intermediate_list_modified$`Base Outlier Distortion Test`$glance$outlier_pval <- NULL
  intermediate_list_modified$`Adaptation Outlier Distortion Test`$glance$outlier_pval <- NULL



  options("modelsummary_format_numeric_latex" = "plain")
  modelsummary(intermediate_list_modified,
               statistic = "{std.error}",
               coef_rename = name_vec_tex,
               stars = c('*' = .05, '**' = .01, '***' = 0.001),
               fmt = 5,
               gof_map = gof_map_mod,
               coef_omit = "time|year|iso|iis",
               escape = FALSE,
               output = "latex",
               add_rows = data.frame("Fixed Effects", "Country \\& Year","Country \\& Year","",
                                     "Country \\& Year","Country \\& Year",""),
               #output = here("data-raw/projections/out/models.tex"),
               notes = c("(Standard Errors) and [p-values]",
                         "its outlier distortion test statistics is not zero.",
                         "Please note that the estimated coefficients on Precipitation$^{2}$ by OLS and IIS are very close but not exactly equal in the base model. Thus,")) %>%

    column_spec(column = c(4,7), border_left = TRUE, border_right = TRUE) -> tab

  # insert caption
  tab <- gsub("\\begin{table}",
              paste0("\\begin{table}[H]
\\caption{OLS and IIS Panel Regression Results together with their difference in coefficients and the resulting outlier distortion test statistic. Coefficients on control variables are omitted. IIS selection was carried out at $t = ",sig_level,"$.}
\\label{tab_app1_estimates",sig_level,"}"), tab, fixed = TRUE)

  # fixing the header
  tab <- gsub("\\begin{tabular}[t]{lcc|>{}c|cc|>{}c|}",
              "\\resizebox{\\textwidth}{!}{\\begin{tabular}[t]{|L{5cm}cc|>{}C{2.5cm}|cc|>{}C{2.5cm}|}",tab, fixed = TRUE)
  # fixing the footer
  tab <- gsub("\\end{tabular}",
              "\\end{tabular}}",tab, fixed = TRUE)

  cat(tab, file = here(paste0("data-raw/projections/out/tables/",sig_level,"_application_main_table.tex")))
  #cat(tab, file = here("data-raw/text/v7 edit/application_main_table.tex"))






  # Appendix Table (Lag(GDP)) ----

  adapt_vars <- grep("temp|prcp",names(coef(am2.isat_L1)), value = TRUE)
  diffs <- vector()
  stats <- vector()
  pvals <- vector()
  for(i in adapt_vars){
    running_test <- distorttest(am2.isat, coef = i)
    #diffs <- append(diffs,distorttest(am2.isat, coef = i)$coef.diff)
    stats <- append(stats,running_test$statistic)
    pvals <- append(pvals,running_test$p.value)
  }

  ti3 <- tibble(
    term = grep("temp|prcp",names(coef(am2.isat_L1)), value = TRUE),
    estimate = stats,
    std.error = pvals,
    p.value = rep(1,8)
  )



  overall_test_adapt_L1 <- distorttest(am2.isat_L1, coef = c("temp","temp_2", "temp_int","temp_2_int"))

  gof <- c("sigma", "statistic", "p.value", "df",
           "logLik", "BIC", "deviance", "df.residual",
           "nobs", "outlier_teststat",
           "outlier_pval", "outlier_no")

  matrix("",ncol = length(gof)) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(gof) %>%
    mutate(outlier_teststat = overall_test_adapt_L1$statistic,
           outlier_pval = overall_test_adapt_L1$p.value,
           outlier_no = length(overall_test_adapt_L1$iis$ISnames))-> gl3


  mod3 <- list(
    tidy = ti3,
    glance = gl3)
  class(mod3) <- "modelsummary_list"


  am2.isat_L1 %>% as.lm -> am2.isat_L1_lm


  list(
    "Lagged Adaptation" = am2_L1,
    "Lagged Adaptation IIS" = am2.isat_L1_lm,
    "Lagged Adaptation Outlier Distortion Test" = mod3) -> modellist

  # The lag(GDP) goes into the appendix
  #"Adaptation Lag(GDP_pc)" = am2_L1,
  #"Adaptation Lag(GDP_pc) IIS" = am2.isat_L1


  names(coef(am2.isat_L1)) %>%
    grep(x = .,pattern = "temp|prcp", value = TRUE) -> name_vec

  names(name_vec) <- name_vec
  name_vec <- gsub("temp","Temperature", name_vec)
  name_vec <- gsub("prcp","Precipitation", name_vec)
  name_vec <- gsub("_2","<sup>2</sup>", name_vec)
  name_vec <- gsub("_int"," x Lag(GDPpc)", name_vec)

  name_vec_tex <- gsub("<sup>2</sup>","\\\\textsuperscript{2}",name_vec)
  name_vec_tex <- gsub("pc","\\\\textsubscript{pc}",name_vec_tex)




  modelsummary(modellist,
               coef_rename = name_vec_tex,
               stars = c('*' = .05, '**' = .01, '***' = 0.001),
               fmt = 5,
               gof_map = gof_map_mod,
               coef_omit = "time|year|iso|iis",
               gof_omit = "AIC|F|R",
               escape = FALSE,
               add_rows = data.frame("Fixed Effects", "Country & Year","Country & Year",""),
               output = "modelsummary_list",
               #output = here("data-raw/projections/out/models_Appendix.tex"),
               notes = "For difference columns, test statistics are in brackets. Standard Errors in brackets of the other columns."
  ) -> intermediate_list



  intermediate_list_modified <- modify_brackets(intermediate_list)

  teststat_lagged <- intermediate_list_modified$`Lagged Adaptation Outlier Distortion Test`$glance$outlier_teststat
  testpval_lagged <- intermediate_list_modified$`Lagged Adaptation Outlier Distortion Test`$glance$outlier_pval

  ## Add Chi-sq sign to the line
  intermediate_list_modified$`Lagged Adaptation Outlier Distortion Test`$glance$outlier_teststat <- paste0("$\\chi^2_{",overall_test_adapt_L1$df,"}$ = ",round(teststat_lagged, 2))

  # Format outlier test stat p value
  intermediate_list_modified$`Lagged Adaptation Outlier Distortion Test`$glance$outlier_pval <- paste0("[", ifelse(
    format(round(testpval_lagged, 3), nsmall = 3) == "0.000",
    "$<$0.001",
    format(round(testpval_lagged, 3), nsmall = 3)), "]")


  #Put both outlier test stat and p-value in the same cell
  intermediate_list_modified$`Lagged Adaptation Outlier Distortion Test`$glance$outlier_teststat <-
    paste0(intermediate_list_modified$`Lagged Adaptation Outlier Distortion Test`$glance$outlier_teststat,"\n",intermediate_list_modified$`Lagged Adaptation Outlier Distortion Test`$glance$outlier_pval)

  # Delete the pvalue information
  intermediate_list_modified$`Lagged Adaptation Outlier Distortion Test`$glance$outlier_pval <- NULL



  modelsummary(intermediate_list_modified,
               statistic = "{std.error}",
               coef_rename = name_vec_tex,
               stars = c('*' = .05, '**' = .01, '***' = 0.001),
               fmt = 5,
               gof_map = gof_map_mod,
               coef_omit = "time|year|iso|iis",
               escape = FALSE,
               add_rows = data.frame("Two Way Fixed Effects", "Yes","Yes",""),
               output = "latex",
               #output = here(paste0("data-raw/projections/out/tables/",sig_level,"_models-Appendix.tex")),
               notes = c("(Standard Errors) and [p-values]",
                         "not exactly equal in the lagged model. Thus, its outlier distortion test statistics is not zero.",
                         "Please note that the estimated coefficients on Precipitation$^{2}$ by OLS and IIS are very close but ")) %>%

  column_spec(column = c(1,4), border_left = TRUE, border_right = FALSE) -> tab

  # insert caption
  tab <- gsub("\\begin{table}",
              paste0("\\begin{table}[H]
\\caption{OLS and IIS Panel Regression Results together with their difference in coefficients and the resulting outlier distortion test statistic. Coefficients on control variables are omitted. IIS selection was carried out at $t = ",sig_level,"$.}
\\label{tab_app1_estimates_appendix_",sig_level,"}"), tab, fixed = TRUE)

  # fixing the header
  tab <- gsub("\\begin{tabular}[t]{|>{}lcc|>{}c}",
              "\\resizebox{\\textwidth}{!}{\\begin{tabular}[t]{|L{5cm}cc|>{}C{2.5cm}|}",tab, fixed = TRUE)
  # fixing the footer
  tab <- gsub("\\end{tabular}",
              "\\end{tabular}}",tab, fixed = TRUE)

  cat(tab, file = here(paste0("data-raw/projections/out/tables/",sig_level,"_models-Appendix.tex")))
  #cat(tab, file = here("data-raw/text/v7 edit/application_main_table.tex"))




}



#'
# Testing Tables ----
#'
#' Make sure to include all of this header!
#'
#'
# \documentclass{article}
#
# \usepackage{graphicx}
# \usepackage{caption}
# \usepackage{float}
# \usepackage{xcolor}
# \usepackage{array}
# \usepackage{booktabs}
#
# \newcommand{\ignore}[1]{}
# % left fixed width:
# \newcolumntype{L}[1]{>{\raggedright\arraybackslash}p{#1}}
# % center fixed width:
# \newcolumntype{C}[1]{>{\centering\arraybackslash}p{#1}}
# % flush right fixed width:
# \newcolumntype{R}[1]{>{\raggedleft\arraybackslash}p{#1}}
#
#
# \begin{document}
#
#   \include{0.05_application_main_table}
#   \include{0.01_application_main_table}
#   \include{0.001_application_main_table}
#   \include{0.05_models-Appendix}
#   \include{0.01_models-Appendix}
#   \include{0.001_models-Appendix}
#
# \end{document}
