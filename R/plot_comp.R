###############################################################################
###################### PLOTTING FUNCTIONS #####################################
###############################################################################

# The following contains three functions for plotting multiple models side by side. 
# plot_comp: The first plots multiple isatpanel objects by panel cross-section or 
#     model description using the break_uncertainty() function of getspanel
# plot_comp_quick: given plot_comp compiles somewhat slowly, this second function 
#     draws on the plotting output of plot_grid from getspanel for quicker rendering. 
#     Note that this function prints successive breaks as additive rather than independent! 
# plot_unit: final function can plot both multiple model specifications and multiple 
#     dependent variables for a single panel cross-section/unit for higher-dim comparisons 

###############################################################################


###############################################################################
#' Plotting multiple isatpanel objects for comparison across units
#'
#' The following function takes a dataframe of isatpanel objects with associated 
#' model names and builds a plot that allows for comparison of detected breaks 
#' by panel cross-section. 
#' Each line should represent a unique model specification-sample pair (no check implemented yet, user must verify).
#' The function can only handle one outcome/dependent variable at a time.
#'
#' @param mod Dataframe of model objects. At minimum, requires one column with 
#' one isatpanel object wrapped in a list per row and a second column with a unique model description.
#' @param sign Type of break to display in plot ("all", negative ("neg"), or positive ("pos")). 
#' @param panel Whether the results should be displayed by panel cross-section or by model ("model" or "unit")
#' @param main_text Plot title (optional)
#' @param blanks Boolean - whether to include models in which no break is detected for a panel cross-section.
#' @param t_range provide time range of panel from tt:TT.
#' @param id_list provide a character vector of unit names to isolate only certain units
#'
#' @return ggplot object that displays multiple isatpanel results by panel-cross section for quick comparison across multiple specifications.
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(here)
#' library(gridExtra)
#' library(gets)
#' library(getspanel)
#' library(assertthat)
#' res <- readRDS(here('data/standard_results_example.RDS')) 
#' res %>% select(is, model) %>% plot_comp(t_range = 2000:2021)
#' # Example isolating specific units
#' res %>% select(is, model) %>% plot_comp(t_range = 2000:2021, id_list = c("Argentina", "Bulgaria", "China"))
#'
#'
#'


plot_comp <- function(mod, sign, panel = "unit", main_text = NULL, blanks = TRUE, t_range, id_list = NULL){
  p_data <- tibble()
  if(nrow(mod) > 8){print(paste0("WARNING: Plotting ", nrow(mod), " models. Take care when plotting too many models."))}
  if(nrow(mod) == 0){
    stop("No models to plot.")
    }
  
  for(r in 1:nrow(mod)){
    print(r)
    tmp <- mod %>% 
      slice(r)
    
    coefficients <- tibble(
      name = names(coef(tmp$is[[1]]$isatpanel.result)),
      coef = coef(tmp$is[[1]]$isatpanel.result)
    )

    # Combine all tables in get_indicators by row and select id, time, name
    indicators <- get_indicators(tmp$is[[1]])
    # indicators <- indicators[!names(indicators) %in% "impulses"]
    indicators_flat <- do.call(rbind, lapply(indicators, function(x) x[, c("id", "time", "name"), drop = FALSE]))

    raw_breaks <- merge(indicators_flat, coefficients)
    
    if(missing(sign)){}
    else if(sign == "neg"){
      raw_breaks <- raw_breaks %>% filter(coef < 0)
    }else if(sign == "pos"){
      raw_breaks <- raw_breaks %>% filter(coef > 0) 
    }
    
    # Prepare indicator info before completing the time grid
    indicator_info <- raw_breaks %>%
      mutate(
        indicator_type = case_when(
          grepl("^iis[0-9]+$", name) ~ "iis",
          grepl("^fesis.+\\.[0-9]+$", name) ~ "fesis",
          grepl("^tis.+\\.[0-9]+$", name) ~ "tis",
          TRUE ~ "other"
        ),
        breaks = time
      ) %>%
      select(id, coef, indicator_type, breaks)
    
    # Create a full grid of id and time
    full_grid <- expand.grid(
      id = unique(indicator_info$id),
      time = t_range
    )

    # Old code: this only "extends" the absolute coefficient value of the last (fesis) break until the next break, this does not show the cumulative coefficient value (i.e. the net effect of all breaks)
    # p_data <- raw_breaks %>%
    #   mutate(breaks = time) %>% 
    #   select(id, time, coef, breaks) %>% 
    #   complete(id, time = t_range) %>% 
    #   group_by(id) %>% 
    #   fill(coef, .direction = "down") %>% 
    #   ungroup() %>% 
    #   mutate(model = tmp$model) %>% 
    #   rbind(p_data)

    # Variant 1: this shows the coefficient value of a break at the time of the break, but also does not show any cumulative effect
    # Note: works for iis and fesis but does not show the prolonged effect of fesis
    # p_data <- raw_breaks %>%
    #   mutate(breaks = time) %>%
    #   select(id, time, coef, breaks) %>%
    #   complete(id, time = t_range) %>%
    #   group_by(id, time) %>%
    #   summarise(coef = sum(coef, na.rm = TRUE), .groups = "drop") %>%
    #   group_by(id) %>%
    #   mutate(model = tmp$model) %>%
    #   ungroup() %>%
    #   rbind(p_data)
    
    # Variant 2: this shows the cumulative effect of all breaks, but the individual coefficient values are not distinguishable
    # Note: only works for fesis breaks
    # p_data <- raw_breaks %>%
    #   mutate(breaks = time) %>%
    #   select(id, time, coef, breaks) %>%
    #   complete(id, time = t_range) %>%
    #   arrange(id, time) %>%
    #   group_by(id) %>%
    #   mutate(coef = cumsum(replace_na(coef, 0))) %>%
    #   ungroup() %>%
    #   mutate(model = tmp$model) %>%
    #   rbind(p_data)
    
    # This Version extends the old code idea of creating the full coefficients table from only the breaks to also support iis and tis breaks
    # Requires some tweaking but is quite a bit faster (and potentially easier to understand) than the full indicators x panel method currently used in plot_grid and plot_indicators
    p_data <- full_grid %>%
      left_join(indicator_info, by = "id") %>%
      mutate(
        active = case_when(
          indicator_type == "iis"   & breaks == time ~ TRUE,
          indicator_type == "fesis" & breaks <= time ~ TRUE,
          indicator_type == "tis"   & breaks <= time ~ TRUE,
          TRUE ~ FALSE
        ),
        effect = case_when(
          indicator_type == "iis"   & active ~ coef,
          indicator_type == "fesis" & active ~ coef,
          indicator_type == "tis"   & active ~ coef * (time - breaks + 1),
          TRUE ~ 0
        )
      ) %>%
      group_by(id, time) %>%
      summarise(cum_coef = sum(effect, na.rm = TRUE), .groups = "drop") %>%
      mutate(model = tmp$model) %>%
      rbind(p_data)

    if(!is.null(id_list)){p_data <- p_data %>% filter(id %in% id_list)}
  }
  
  # makes sure that empty rows are shown as well (ie. where no breaks are detected)
  if(blanks){
    p_data <- p_data %>%
      complete(id, time, model)
  }
  
  if(panel == "model"){
    p_data <- p_data %>% rename(id = model, model = id)
  }

  p <- p_data %>%
    ggplot(aes(x = time, y = model)) +
    geom_tile(aes(fill = cum_coef), na.rm = TRUE) +  
    scale_fill_gradient2(na.value = NA, name = "Effect", mid = "white")+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0), limits = rev) +
    facet_grid(id~., scales = "free_y", space = "free") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(fill = NA),
          strip.background = element_blank(),
          axis.text.y = element_text(size = 6, color = "black"),
          axis.text.x = element_text(size = 10, color = "black"),
          strip.text.y = element_text(size = 12, angle = 0),
          plot.caption = element_text(size = 12, hjust = 0.5),
          legend.position = "bottom"
    ) +
    labs(x = NULL, y = NULL, title = main_text)
  
  return(p)
}


###############################################################################
#' Plotting multiple isatpanel objects for comparison across units
#'
#' The following function takes a dataframe of isatpanel objects with associated 
#' model names and builds a plot that allows for comparison of detected breaks 
#' by panel cross-section. In contrast to the function above, this does not rely 
#' on the break_uncertainty function of gets but rather pulls the plot data from 
#' plot_grid for faster rendering. 
#' 
#' NOTE: this function prints successive breaks as additive rather than independent! 
#' This significantly hampers interpretation of the grids as breaks are represented 
#' as their net effect on y rather than independent effect.
#'
#' @param mod Dataframe of model objects. At minimum, requires one column with 
#' one isatpanel object wrapped in a list per row and a second column with a unique model description.
#' @param sign Type of break to display in plot ("all", negative ("neg"), or positive ("pos")). 
#' @param panel Whether the results should be displayed by panel cross-section or by model ("model" or "unit")
#' @param na.rm Same as blanks in plot_comp - whether to include models in which no break is detected for a panel cross-section.
#' @param id_list provide a character vector of unit names to isolate only certain units
#' 
#' 
#' @return ggplot object that displays multiple isatpanel results by panel-cross section for quick comparison across multiple specifications.
#' @examples
#' 
#' res <- readRDS(here('standard_results_example.RDS')) 
#' res %>% select(is, model) %>% plot_comp_quick()
#' res %>% select(is, model) %>% plot_comp_quick(id_list = c("Chile", "Colombia", "CzechRepublic"))
#'
#'

# Possible that this incorrectly distinguishes negative and positive breaks by including impulse effects? To be checked and corrected.
plot_comp_quick <- function(mod,  sign, panel = "unit", na.rm = TRUE, id_list = NULL){
  if(nrow(mod) > 8){print(paste0("WARNING: Plotting ", nrow(mod), " models. Take care when plotting too many models."))}
  if(nrow(mod) == 0){
    stop("No models to plot.")
  }
  
  tmp <- tibble()
  for(m in 1:nrow(mod)){
    if(mod %>% slice(m) %>% pull(is) %>% first %>% plot_grid %>% ggplot_build %>% try %>% is.error){next}else{
      mod_name <- mod %>% slice(m) %>% pull(model)
      # Currently,this extracts the data used to build the plot_grid in isatpanel; not ideal
      grid_dat <- mod %>% slice(m) %>% pull(is) %>% first %>% plot_grid %>% ggplot_build
      grid_dat <- grid_dat$plot$data
      grid_dat$model <- mod_name
      tmp <- rbind(tmp, grid_dat)
    }
  }
  
  if(!is.null(id_list)){tmp <- tmp %>% filter(id %in% id_list)}

  
  if(panel == "model"){
    tmp <- tmp %>% rename(id = model, model = id)
  }
  
  if(na.rm == TRUE){
    tmp <- tmp %>% group_by(id, model) %>% filter(!all(is.na(effect)))
  }
  
  if(missing(sign)){}
  else if(sign == "pos"){
    tmp <- tmp %>% group_by(id, model) %>% filter(any(effect > 0))
    
  }else if(sign == "neg") { tmp <- tmp %>% group_by(id, model) %>% filter(any(effect < 0))}
  
  p <- tmp %>% 
    mutate(id = as.character(id)) %>% 
    ggplot(aes(x = time, y = model)) +
    geom_tile(aes(fill = effect), na.rm = TRUE) +
    scale_fill_gradient2(na.value = NA, name = "Effect")+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0), limits = rev) +
    facet_grid(id~., scales = "free") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(fill = NA),
          strip.background = element_blank(),
          axis.text = element_text(size = 12, color = "black"),
          strip.text.y = element_text(size = 14, angle = 0)) +
    labs(x = NULL, y = NULL, title= "Model Overview")

  return(p)
}


###############################################################################
#' This function plots multiple isatpanel objects by single panel cross-section allowing for 
#' comparison across both multiple dependent variables and multiple model specifications.
#'
#'
#' @param mod Dataframe of model objects. At minimum, requires one column with 
#' one isatpanel object wrapped in a list per row, a second column with a unique model description, 
#' and a third column with dependent variable names.
#' @param unit Exact string identifier of panel cross-section/unit of interest in isatpanel result.
#' @param blanks Boolean - whether to include models in which no break is detected for a panel cross-section.
#' @param t_range provide time range of panel from tt:TT.
#'
#' @return ggplot object that displays multiple isatpanel results for a single unit for quick comparison across multiple specifications.
#' @export
#' @examples
#' res <- readRDS(here('standard_results_example.RDS')) 
#' res %>% select(is, model, dep) %>% plot_unit(t_range = 2000:2021, unit = "Peru")
#'
#'

plot_unit <- function(mod, unit, blanks = TRUE, t_range){
  p_data <- tibble()
  if(nrow(mod) > 8){print(paste0("WARNING: Plotting ", nrow(mod), " models. Take care when plotting too many models."))}
  if(nrow(mod) == 0){
    stop("No models to plot.")
  }
  if(missing(t_range)){
    stop("Please specify t_range.")
  }
  
  for(r in 1:nrow(mod)){
    print(r)
    tmp <- mod %>% 
      slice(r)
    
    coefficients <- tibble(
      name = names(coef(tmp$is[[1]]$isatpanel.result)),
      coef = coef(tmp$is[[1]]$isatpanel.result)
    )

    # Combine all tables in get_indicators by row and select id, time, name
    indicators <- get_indicators(tmp$is[[1]])
    indicators_flat <- do.call(rbind, lapply(indicators, function(x) x[, c("id", "time", "name"), drop = FALSE]))

    raw_breaks <- merge(indicators_flat, coefficients)
    
    p_data <- raw_breaks %>%
      mutate(breaks = time) %>% 
      select(id, time, coef, breaks) %>% 
      complete(id, time = t_range) %>% 
      group_by(id) %>% 
      fill(coef, .direction = "down") %>% 
      ungroup() %>% 
      mutate(model = tmp$model,
             dep = tmp$dep) %>% 
      rbind(p_data)
  }
  cmod <- p_data %>% filter(id == unit)
  
  if(!blanks){
    cmod <- cmod %>% group_by(dep) %>% filter(!all(is.na(coef)))
  }
  if(nrow(cmod)== 0){return()}
  # Replace NAs with 0 value so that cases in which only one model reveals an effect are displayed correctly (limitation of facet_grid)
  cmod %>% mutate(coef = ifelse(is.na(coef), 0, coef)) %>%
    group_by(dep) %>%
    do(gg = {ggplot(., aes(x = time, y = model)) +
        geom_tile(aes(fill = coef)) +
        scale_fill_gradient2(na.value = NA, name = "Effect", oob = scales::squish) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_discrete(expand = c(0,0), limits = rev) +
        facet_grid(dep~.) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              strip.background = element_blank(),
              axis.text = element_blank(), #element_text(size = 12, color = "black"),
              strip.text.y = element_text(size = 12)) +
        labs(x = NULL, y = NULL, title = NULL)
    }) %>%
    .$gg %>% arrangeGrob(grobs = ., ncol = 1) %>% grid.arrange(top = unit)
}




