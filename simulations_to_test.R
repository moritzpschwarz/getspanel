
###
# Generate k Unit Example
# Set up----

library(conflicted)
library(tidyverse)
conflicted::conflicts_prefer(dplyr::dplyr)
conflicted::conflicts_prefer(dplyr::rename)
conflicted::conflicts_prefer(dplyr::arrange)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::summarise)
conflicted::conflicts_prefer(dplyr::first)

devtools::load_all()

fesis_contr <- FALSE
tis_contr <- FALSE
iis_contr <- FALSE

base_time <- 1900
k = 6
n_time = 50
sigma = 0.5
fe_sigma = 5
#beta <- c(0.3)
beta <- c(0.3,0.7,-.3,0,0)

trend_mag <- 0.2

trendbreak_loc <- 0.65
trendbreak_mag <- -0.4

strbreak_loc <- 0.2
strbreak_mag <- 2 # will be fe + (1 + step_mag)

# this is the IDs which are treated
treated_step <- c(3,5)
treated_trend <- c(2)
treated_trendbreak <- 1


# Functions----


DGP <- function(input_dat, i, n_time, sigma, beta, treated_step, treated_trend, treated_trendbreak){

  id <- LETTERS[i]
  x <- matrix(rnorm(n_time*length(beta)),ncol = length(beta))
  eps <- rnorm(n_time, mean = 0, sd = sigma)
  fe <- means[i]

  y <- x %*% beta + fe + eps
  # ggplot(data.frame(x = 1:n_time, y = y),aes(x = x, y = y)) + geom_line()


  #if(i %in% treated_trendbreak & !i %in% treated_trend){stop("must have trend to impose trendbreak")}

  # trend
  y <- if(i %in% treated_trend){y + trend*trend_mag} else {y}
  # ggplot(data.frame(x = 1:n_time, y = y),aes(x = x, y = y)) + geom_line()

  # trendbreak
  y <- if(i %in% treated_trendbreak){y + trendbreak*trendbreak_mag} else {y}
  # ggplot(data.frame(x = 1:n_time, y = y),aes(x = x, y = y)) + geom_line()

  # structural break
  y <- if(i %in% treated_step){y + strbreak*strbreak_mag + fe} else {y}
  # ggplot(data.frame(x = 1:n_time, y = y),aes(x = x, y = y)) + geom_line()

  rbind(input_dat, data.frame(id = id,
                              time = (1:n_time)+base_time,
                              x = x,
                              y = y)) -> output_dat


  treatment_dat <- tibble() %>%
    {if(i %in% treated_trend){bind_rows(., tibble(id, treated = "trend", time = 1))}else{.}} %>%
    {if(i %in% treated_trendbreak){bind_rows(., tibble(id, treated = "trendbreak", time = ceiling(n_time*trendbreak_loc)))}else{.}} %>%
    {if(i %in% treated_step){bind_rows(., tibble(id, treated = "step", time = ceiling(n_time*strbreak_loc)))}else{.}}


  out <- list()
  out$output_dat <- output_dat
  out$treatment_dat <- treatment_dat

  return(out)

}


# Loop -----

overall <- tibble()
for(indic_method in c("fesis","tis","both")){
  for(n_time in c(20,30,50,100)){
    for(k in c(2,3,5,10)){
      for(eng in c("gets", "lasso")){
        set.seed(123)

        print(paste0("N:", n_time, ", K:", k, ", Engine:",eng, ", IndicMethod:",indic_method))

        means <- rnorm(k, sd = fe_sigma)

        # setup breaks, trends and trendbreaks
        # trend
        trend <- 1:n_time
        # impose a trendbreak
        trendbreak <- rep(0,n_time)
        trendbreak[ceiling(n_time*trendbreak_loc):n_time] <- 1:length(trendbreak[ceiling(n_time*trendbreak_loc):n_time]) # impose a trendbreak
        # create structural break
        strbreak <- rep(0,n_time)
        strbreak[ceiling(n_time*strbreak_loc):n_time] <- 1

        # Generate some random data for the countries
        current_data <- data.frame()
        treatment_collection <- data.frame()
        for(i in 1:k){
          dgp_out <- DGP(input_dat = current_data,
                         i = i,
                         n_time = n_time,
                         beta = beta,
                         sigma = sigma,
                         treated_trend = treated_trend,
                         treated_trendbreak = treated_trendbreak,
                         treated_step = treated_step)
          current_data <- dgp_out$output_dat
          treatment_collection <- bind_rows(treatment_collection, dgp_out$treatment_dat)
        }

        # plot the current data
        # current_data %>%
        #   pivot_longer(-c(id,time)) %>%
        #   ggplot(aes(x = time, y = value, color = name)) +
        #   geom_line() +
        #   facet_wrap(~id)

        # create formula object
        form <- as.formula(paste0("y ~ ",paste0(current_data %>%
                                                  select(-c(id,time,y)) %>%
                                                  names,
                                                collapse = " + ")))

        if(eng == "lasso"){
          for(adaptive in c(TRUE, FALSE)){
            for(alpha in c(1, 0.5)){

              lasso_opts <- if(eng != "lasso"){NULL}else{list(adaptive = adaptive,
                                                              alpha = alpha)}

              # run isatpanel
              result <- isatpanel(current_data, formula = form,
                                  effect = "individual",
                                  index = c("id","time"),
                                  fesis = if(indic_method %in% c("fesis","both")){TRUE}else{FALSE},
                                  tis = if(indic_method %in% c("tis","both")){TRUE}else{FALSE},
                                  iis = FALSE,
                                  engine = eng,
                                  lasso_opts = lasso_opts,
                                  print.searchinfo = FALSE)





              # store the results
              indv_tibble_result <- tibble(k,
                                           n_time,
                                           sigma,
                                           fe_sigma,
                                           beta = list(beta),
                                           trend_mag,
                                           trendbreak_loc,
                                           trendbreak_mag,
                                           strbreak_loc,
                                           strbreak_mag,
                                           treated_step = list(treated_step),
                                           treated_trend = list(treated_trend),
                                           treated_trendbreak = list(treated_trendbreak),
                                           fesis = fesis_contr,
                                           tis = tis_contr,
                                           iis = iis_contr,
                                           getspanel_obj = list(result),
                                           treatment_collection = list(treatment_collection),
                                           indicators = list(get_indicators(result)),
                                           lasso_opts = if(eng == "lasso"){list(lasso_opts)}else{NA},
                                           adaptive = if(eng == "lasso"){adaptive}else{NA},
                                           alpha = if(eng == "lasso"){alpha}else{NA},
                                           engine = eng,
                                           indic_method = indic_method
              )

              # add to the overall dataframe
              overall %>%
                bind_rows(indv_tibble_result) -> overall




            }
          }

        } else {

          # run isatpanel
          result <- isatpanel(current_data, formula = form,
                              effect = "individual",
                              index = c("id","time"),
                              fesis = if(indic_method %in% c("fesis","both")){TRUE}else{FALSE},
                              tis = if(indic_method %in% c("tis","both")){TRUE}else{FALSE},
                              iis = FALSE,
                              print.searchinfo = FALSE)



          # store the results
          indv_tibble_result <- tibble(k,
                                       n_time,
                                       sigma,
                                       fe_sigma,
                                       beta = list(beta),
                                       trend_mag,
                                       trendbreak_loc,
                                       trendbreak_mag,
                                       strbreak_loc,
                                       strbreak_mag,
                                       treated_step = list(treated_step),
                                       treated_trend = list(treated_trend),
                                       treated_trendbreak = list(treated_trendbreak),
                                       fesis = fesis_contr,
                                       tis = tis_contr,
                                       iis = iis_contr,
                                       getspanel_obj = list(result),
                                       treatment_collection = list(treatment_collection),
                                       indicators = list(get_indicators(result)),
                                       lasso_opts = if(eng == "lasso"){list(lasso_opts)}else{NA},
                                       adaptive = if(eng == "lasso"){adaptive}else{NA},
                                       alpha = if(eng == "lasso"){alpha}else{NA},
                                       engine = eng,
                                       indic_method = indic_method
          )

          # add to the overall dataframe
          overall %>%
            bind_rows(indv_tibble_result) -> overall

        }
      }
    }
  }
}


#save(overall, file = "overall_simulations.RData")
#load("overall_simulations.RData")



overall %>%
  mutate(index = 1:n()) %>%
  #select(index, treatment_collection) %>%
  unnest(treatment_collection) %>%
  mutate(treated = ifelse(treated == "trendbreak","trend",treated)) -> A


B <- overall %>%
  mutate(index = 1:n()) %>%
  #select(index, indicators) %>%
  unnest(indicators) %>%
  unnest(indicators) %>%
  select(-y, -value) %>%
  rowwise() %>%
  mutate(time = time - 1900,
         treated = case_when(grepl("^fesis",name) ~ "step",
                             grepl("^tis",name) ~ "trend")) %>%
  ungroup() %>%
  rename(time_ind = time)



full_join(A,B) %>%
  arrange(index) %>%
  mutate(diff = time_ind - time,
         incor = ifelse(is.na(diff),TRUE,FALSE),
         missed = NA,
         too_many = NA) %>%

  group_by(index,id) %>%
  mutate(correct_number = sum(!is.na(time)),
         indic_number = sum(!is.na(time_ind)),
         missed = case_when(indic_number == 0 & correct_number > 0 ~ treated)) %>%
  #too_many = case_when(indic_number > correct_number ~ TRUE, TRUE ~ FALSE)) %>% View
  ungroup -> intermediate_data


intermediate_data %>%
  filter(!incor | !is.na(missed)) -> correct


intermediate_data %>%
  filter(incor & is.na(missed)) %>%
  group_by(index,id) %>%
  mutate(correct_misspec = case_when(is.na(time) ~ list(treated))) %>%
  mutate(time = case_when(is.na(time) ~ list(time))) %>%

  unnest(correct_misspec, keep_empty = TRUE) %>%
  filter(correct_misspec != treated) %>%

  unnest(time, keep_empty = TRUE) %>%
  filter(!is.na(time)) %>%

  mutate(diff = time_ind - time,
         order = order(abs(diff)),
         additional = order > 1) %>%
  ungroup() -> incorrect


correct %>%
  bind_rows(incorrect) %>%
  arrange(index, id) -> final_data





final_data %>%
  summarise(.by = c(index,n_time,k),
            true_misspecification_missed = sum(!is.na(missed))) %>%
  ggplot(aes(x = n_time, y = true_misspecification_missed, color = as.factor(k))) +
  geom_line() +
  facet_wrap(~k) +
  scale_y_continuous(breaks = as.integer)


final_data %>%
  filter(!incor) %>%
  summarise(diff = sum(abs(diff)), .by = c(index,n_time,k)) %>%
  ggplot(aes(x = n_time, y = diff, color = as.factor(k))) +
  geom_line() +
  facet_wrap(~k)




overall %>%
  #filter(engine == "lasso") %>%
  mutate(no_indicators = map(getspanel_obj, function(x){
    if(is.null(x$isatpanel.result$ISNames)){
      x$finaldata %>%
        select(-starts_with("x."), -starts_with("id"),-time,-y) %>%
        names %>%
        length
    } else {
      length(x$isatpanel.result$ISnames)
    }
  })) %>%
  unnest(no_indicators) %>%

  #filter(n_time == 100, k == 10, engine == "lasso", adaptive)

  mutate(adaptive = case_when(is.na(adaptive) ~ "N/A (gets)",
                              TRUE ~ as.character(adaptive))) %>%


  ggplot(aes(x = as.factor(n_time), y = no_indicators, color = engine, shape = adaptive)) +
  #geom_line() +
  geom_point() +

  facet_grid(indic_method~k) +
  labs(x = "N per unit", y = "Number of Indicators") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA)) -> plt

ggsave(plt, "simulation_results_plot.png", width = 8, height = 8)

