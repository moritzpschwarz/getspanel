isvarcor.isat <- function(isatobject){


  #################################################################################
  ########## 1. Consistency correction of sigma estimate (affects all regressors)
  vcov.mean.original <- isatobject$vcov.mean

  vcov.mean.cor.1 <- isatobject$vcov.mean * as.numeric(isvarcor(isatobject$aux$t.pval, 1)[2]^2)
  ###################################################################################

  ###############################################################################################################
  ######### 2. Correction for the variance of retained regressors (affects only fixed regressors, not impulses)
  vcov.mean.cor.2 <- vcov.mean.cor.1
  rel_names <- isatobject$aux$mXnames[!(isatobject$aux$mXnames %in% isatobject$ISnames)]
  mcor <- 1
  vcov.mean.cor.2[rel_names, rel_names] <- vcov.mean.cor.2[rel_names, rel_names] * as.numeric(isvareffcor(isatobject$aux$t.pval, 1, mcor)[2]^2)
  ###############################################################################################################

  isatobject <- isatobject
  isatobject$vcov <- vcov.mean.cor.2
  isatobject$vcov.mean <- vcov.mean.cor.2
  isatobject$vcov.mean.cor.1 <- vcov.mean.cor.1
  isatobject$vcov.mean.original <- vcov.mean.original

  # correcting the S.E. in isatobject
  isatobject$mean.results["std.error"] <- sqrt(diag(vcov.mean.cor.2))

  return(isatobject)
}


project <- function(stdmodel,
                    climate,
                    socioprojections,
                    socioprojections_type = "Mueller",
                    modelname = "m2",
                    coefsamples = 1,
                    seed = 123,

                    adaptation = FALSE,
                    adaptmodel = NULL,
                    max_GDP_restriction = TRUE,
                    no_worse_off_restriction = TRUE,
                    no_higher_baseline_restriction = TRUE,

                    parallel = TRUE,
                    verbose = TRUE,
                    save_dir_here = "data-raw/projections/projfiles"){

  # ISAT Variance correction ----
  if(class(stdmodel)=="isat"){
    stdmodel <- isvarcor.isat(stdmodel)
  }

  if(!is.null(adaptmodel) && class(adaptmodel) == "isat"){
    adaptmodel <- isvarcor.isat(adaptmodel)
  }


  ## Load Coefficients ----
  set.seed(seed)
  selection_coefs_standard <- MASS::mvrnorm(n = coefsamples, mu = stdmodel %>% coef(), Sigma = stdmodel %>% vcov()) %>%
    {if (is.vector(.)) t(.) else .} %>%
    data.frame() %>%
    rename_all(~ tolower(.)) %>%
    select(-starts_with(c("year", "iis", "time", "iso"))) %>%
    rename_all(~ paste0(., "_coef"))

  # If we are just using one coefsample, we simply use the mean estimate from the model
  if(coefsamples==1){
    stdmodel %>%
      coef %>%
      data.frame(variable = names(.),
                 coefficient = .,
                 row.names = NULL) %>%
      pivot_wider(names_from = "variable",values_from = "coefficient") %>%
      rename_all(~tolower(.)) %>%
      select(-starts_with(c("year","iis","time","iso"))) %>%
      rename_all(~paste0(.,"_coef")) -> selection_coefs_standard
  }



  ## Collect Relevant Information
  selection_coefs_standard %>%
    names %>%
    gsub("_coef","",.) %>%
    unique -> climate_vars

  selection_coefs_standard %>%
    names %>%
    gsub("_coef|_2","",.) %>%
    unique -> climate_vars_lin

  selection_coefs_standard %>%
    names %>%
    grep("_2",.,value = TRUE) %>%
    gsub("_coef|_2","",.) %>%
    unique -> climate_vars_sq


  if(adaptation){
    # Prepare max restriction
    if(socioprojections_type == "Mueller"){
      base_gdp <- socioprojections %>% filter(year==2017) %>%
        select(iso,gdp_cap_fifty,gdp_cap_hundred) %>%
        rename(base_fifty = gdp_cap_fifty,
               base_hundred = gdp_cap_hundred)
      max_overall_gdp <- max(base_gdp$base_fifty)
    } else if(socioprojections_type == "SSP"){
      NULL # TO DO
    } else {
      stop("Variable 'socioprojections_type' not correct. Please choose either 'SSP' or 'Mueller'")
    }


    ####simulate coefficients, draw them form joint normal distribution
    set.seed(123)
    selection_coefs_adapt <- MASS::mvrnorm(n = coefsamples, mu = adaptmodel %>% coef(), Sigma = adaptmodel %>% vcov()) %>%
      {if (is.vector(.)) t(.) else .} %>%
      data.frame() %>%
      rename_all(~ tolower(.)) %>%
      select(-starts_with(c("year", "iis", "time", "iso"))) %>%
      rename_all(~ paste0(., "_coef"))

    if(coefsamples==1){
      adaptmodel %>%
        coef %>%
        data.frame(variable = names(.),
                   coefficient = .,
                   row.names = NULL) %>%
        pivot_wider(names_from = "variable",values_from = "coefficient") %>%
        rename_all(~tolower(.)) %>%
        select(-starts_with(c("year","iis","time","iso"))) %>%
        rename_all(~paste0(.,"_coef")) -> selection_coefs_adapt
    }

    # Extract information from adaptation variables

    selection_coefs_adapt %>%
      names %>%
      gsub("_coef","",.) %>%
      grep("_int",.,value = TRUE) %>%
      gsub("_int","",.) %>%
      unique -> interaction_vars
  }


  ## Load climate data
  climate %>%
    # Commented out April 2021: Used to be needed if L1. of climate vars are included (e.g. L1.temp) but not needed anymore
    # Code should work!
    #
    # # add the lags if needed
    # {if(any(grepl("^l[0-9]\\.",climate_vars))){
    #   group_by(.,final_temp, iso) %>%
    #     mutate(across(.cols = all_of(grep("^l[0-9]\\.",climate_vars, invert = TRUE, value = TRUE)),
    #                   .fns = lag,
    #                   .names = "l1.{.col}")) %>%
    #     ungroup
    # }else{.}} %>%
  select(model,rcp,ensemble,final_temp,iso,year,all_of(climate_vars_lin),
         all_of(paste0(climate_vars_sq,"_2"))) %>%
    drop_na -> climate_subset



  internal_fun <- function(v,
                           verbose,
                           climate_subset,
                           climate_vars,
                           climate_vars_lin,
                           climate_vars_sq,
                           selection_coefs_standard,

                           adaptation,
                           selection_coefs_adapt,
                           max_GDP_restriction,
                           no_worse_off_restriction,
                           no_higher_baseline_restriction,

                           save_dir_here){

    # Calculate the standard climate effect
    effect_standard <- climate_subset
    for(var in climate_vars){
      #print(var)
      climate_subset %>%
        select(all_of(var)) %>%
        pull %>% "*"(selection_coefs_standard %>%
                       slice(v) %>%
                       select(all_of(paste0(var,"_coef"))) %>%
                       pull) %>%
        as_tibble %>%
        rename_all(~paste0(var,"_stdeffect"))  %>%
        bind_cols(effect_standard,.) -> effect_standard
    }

    if(!adaptation){

      done <- socioprojections %>%
        select(-contains("fifty")) %>%
        filter(year==2017) %>%
        left_join(effect_standard %>%
                    filter(year == 2017) %>%
                    select(-model,-ensemble,-rcp),by = c("iso","year")) %>%
        mutate(realisation = v) %>%
        relocate(c(realisation,final_temp), .after = year)

    } else if(adaptation){
      effect_interaction <- climate_subset
      # Calculate the climate effect
      for(var in climate_vars){
        #print(var)
        climate_subset %>%
          select(all_of(var)) %>%
          pull %>% "*"(selection_coefs_adapt %>%
                         slice(v) %>%
                         select(all_of(paste0(var,"_coef"))) %>%
                         pull) %>%
          as_tibble %>%
          rename_all(~paste0(var,"_logeffect"))  %>%
          bind_cols(effect_interaction,.) -> effect_interaction
      }

      effect_interaction %>%
        bind_cols(effect_standard %>% select(contains("stdeffect"))) -> effect_interation

      done <- mueller_df %>%
        select(-contains("fifty")) %>%
        filter(year==2017) %>%
        left_join(effect_interaction %>%
                    filter(year == 2017) %>%
                    select(-model,-ensemble),by = c("iso","year")) %>%

        #drop_na %>%
        mutate(realisation = v) %>%
        relocate(c(realisation,final_temp), .after = year)
    } else {stop("Error in 'adaptation' variable.")}


    # Projection Loop
    for(i in 2018:2099){
      if(verbose){print(paste0("Realisation ",v," Year: ",i))}
      done %>%
        filter(year == i-1) %>%
        select(iso, year, final_temp, realisation, contains("hundred")) %>% #,-contains("base")) %>%


        {if(!adaptation){
          left_join(.,effect_standard %>%
                      filter(year == i-1) %>%
                      mutate(realisation = v) %>%
                      select(-model,-ensemble,-rcp),by = c("iso","year","final_temp","realisation"))
        }else{
          left_join(.,effect_interaction %>%
                      filter(year == i-1) %>%
                      mutate(realisation = v) %>%
                      select(-model,-ensemble,-rcp),by = c("iso","year","final_temp","realisation"))
        }} %>%

        mutate(gdp_cap_hundred = gdp_cap_hundred*hundred_mean) %>% # Baseline effect

        mutate(total_stdeffect = rowSums(select(.,ends_with("stdeffect")))) %>% # Standard Climate Effect

        # Adaptation section
        {if(!adaptation){ # no adaptation

          mutate(.,gdp_cap_hundred_climate = gdp_cap_hundred_climate*(hundred_mean + total_stdeffect))

        } else { # this is with adaptation
          mutate(.,
                 # Effect of pure Climate variables in interaction model
                 total_climlogeffect = rowSums(select(.,ends_with("logeffect"),-contains("_int_"))),
                 # Effect of Climate - GDP interaction variables in interaction model
                 total_inteffect =  rowSums(select(.,ends_with("_int_logeffect")))
          ) %>%

            # max GDP Restriction
            {if(max_GDP_restriction){
              mutate(.,interaction_effect_hundred = total_climlogeffect + total_inteffect*log(

                ifelse(test = gdp_cap_hundred_climate>max_overall_gdp,
                       yes = max_overall_gdp,
                       no = gdp_cap_hundred_climate)))
            } else {
              mutate(.,interaction_effect_hundred = total_climlogeffect + total_inteffect*log(gdp_cap_hundred_climate))
            }} %>%


            # no worse off restriction
            # Checks if adaptation effect is lower than standard effect
            # always chooses higher of the two
            {if(no_worse_off_restriction){
              mutate(.,
                     total_effect_this_year_hundred = ifelse(
                       test = total_stdeffect > interaction_effect_hundred,
                       yes = total_stdeffect,
                       no = interaction_effect_hundred))
            }else{.}} %>%

            # no higher than baseline restriction
            # checks: when interaction effect flips the sign of the growth effect
            # (i.e. originally negative effect, but through interaction/adaptation now positive)
            # then the positive growth effect cannot be higher than the baseline growth effect for this year
            {if(no_higher_baseline_restriction) {
              mutate(.,
                     gdp_cap_hundred_climate = gdp_cap_hundred_climate *

                       ifelse(
                         test = sign(total_stdeffect)==-1 & sign(interaction_effect_hundred)==1, # is std effect negative but adaptation effect positive?
                         yes = ifelse(
                           test = hundred_mean<(total_effect_this_year_hundred+1), # is adaptation effect larger than baseline?
                           yes = hundred_mean, # yes: take baseline
                           no = (total_effect_this_year_hundred+1) # no: take adaptation effect
                         ),
                         no = hundred_mean + total_effect_this_year_hundred # just take baseline + adaptation effect
                       )
              )
            } else { . }}
        }} %>%

        # Finishing up
        mutate(year = i) %>%

        bind_rows(done,.) -> done

    }
    save(done,file = here(save_dir_here,paste0(socioprojections_type,"_",modelname,"_",v,".RData")))
  }



  #   for(i in 2018:2099){
  #     if(verbose){print(paste0("Realisation ",v," Year: ",i))}
  #     done %>%
  #       filter(year == i-1) %>%
  #       select(iso, year, realisation, final_temp,contains("hundred")) %>%
  #
  #       left_join(effect_standard %>%
  #                   filter(year == i-1) %>%
  #                   mutate(realisation = v) %>%
  #                   select(-model,-ensemble,-rcp),by = c("iso","year","final_temp","realisation")) %>%
  #       mutate(total_climate_effect = rowSums(select(., ends_with("effect")))) %>%
  #       mutate(gdp_cap_hundred = gdp_cap_hundred*hundred_mean,
  #              gdp_cap_hundred_climate = gdp_cap_hundred_climate*(hundred_mean + total_climate_effect),
  #              year = i) %>%
  #       bind_rows(done,.) -> done
  #
  #   }
  #
  #   save(done,file = here(save_dir_here,paste0("Mueller_",modelname,"_",v,".RData")))
  # }



  if(parallel){
    library(doMC)
    registerDoMC(if(coefsamples < detectCores()){coefsamples} else {detectCores()-1})  # coefsamples if enough cores available - otherwise total-1
    foreach(v=1:coefsamples,.packages = loadedNamespaces()) %dopar% {
      if(verbose){print(v)}
      internal_fun(v,
                   verbose,
                   climate_subset,
                   climate_vars,
                   climate_vars_lin,
                   climate_vars_sq,
                   selection_coefs_standard,

                   adaptation,
                   selection_coefs_adapt,
                   max_GDP_restriction,
                   no_worse_off_restriction,
                   no_higher_baseline_restriction,

                   save_dir_here = save_dir_here)

    }
  } else {
    for (v in 1:coefsamples){
      if(verbose){print(v)}
      internal_fun(v,
                   verbose,
                   climate_subset,
                   climate_vars,
                   climate_vars_lin,
                   climate_vars_sq,
                   selection_coefs_standard,

                   adaptation,
                   selection_coefs_adapt,
                   max_GDP_restriction,
                   no_worse_off_restriction,
                   no_higher_baseline_restriction,

                   save_dir_here = save_dir_here)
    }
  }
}
