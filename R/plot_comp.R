###############################################################################
###################### PLOTTING FUNCTIONS #####################################
###############################################################################

# The following contains three functions for plotting multiple models side by side. 
# plot_comp: The first plots multiple isatpanel objects by panel cross-section or 
#     model description using the break_uncertainty() function of getspanel
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
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 scale_x_continuous scale_y_discrete facet_grid theme_bw theme labs element_text element_blank element_rect
#'
#' @examples
#' library(ggplot2)
#' library(gets)
#' library(getspanel)
#' res <- readRDS(here('data/standard_results_example.RDS')) 
#' plot_comp(res[c("is", "model")], t_range = 2000:2021)
#' # Example isolating specific units
#' plot_comp(res[c("is", "model")], t_range = 2000:2021, id_list = c("Argentina", "Bulgaria", "China"))
#'
#'
#'
plot_comp <- function(mod, sign, panel = "unit", main_text = NULL, blanks = TRUE, t_range, id_list = NULL, regex_exclude_indicators = NULL, mod_list = NULL){
  p_data <- data.frame()
  if(nrow(mod) == 0){
    stop("No models to plot.")
    }
  
  for(r in seq_len(nrow(mod))){
    print(r)
    tmp <- mod[r, ]
    
    df_long <- get_indicators(tmp$is[[1]], format = "long")
    df_long <- df_long[df_long$type %in% c("COMBINED"), ]
    
    # Create data frame with base R
    df_subset <- data.frame(
      id = df_long$id,
      time = df_long$time,
      model = tmp$model,
      effect = df_long$effect,
      stringsAsFactors = FALSE
    )
    
    p_data <- rbind(p_data, df_subset)
  }
  
  if(!is.null(id_list)){
    p_data <- p_data[p_data$id %in% id_list, ]
  }
  if(!is.null(mod_list)){
    p_data <- p_data[p_data$model %in% mod_list, ]
  }
  
  # Create complete grid for blanks
  if(blanks){
    # Get unique combinations
    unique_ids <- unique(p_data$id)
    unique_times <- unique(c(p_data$time, t_range))
    unique_models <- unique(p_data$model)
    
    # Create full grid
    full_grid <- expand.grid(
      id = unique_ids,
      time = unique_times,
      model = unique_models,
      stringsAsFactors = FALSE
    )
    
    # Merge with existing data
    p_data <- merge(full_grid, p_data, by = c("id", "time", "model"), all.x = TRUE)
  }
  
  # Handle panel switch
  if(panel == "model"){
    temp_id <- p_data$id
    p_data$id <- p_data$model
    p_data$model <- temp_id
  }

  p <- ggplot(p_data, aes(x = time, y = model)) +
    geom_tile(aes(fill = effect), na.rm = TRUE) +  
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
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 scale_x_continuous scale_y_discrete facet_grid theme_bw theme labs element_blank element_text
#' @importFrom gridExtra grid.arrange
#' @examples
#' res <- readRDS(here('standard_results_example.RDS')) 
#' plot_unit(res[c("is", "model", "dep")], t_range = 2000:2021, unit = "Peru")
#'
#'

plot_unit <- function(mod, unit, blanks = TRUE, t_range, regex_exclude_indicators = NULL){
  p_data <- data.frame()
  if(nrow(mod) == 0){
    stop("No models to plot.")
  }
  if(missing(t_range)){
    stop("Please specify t_range.")
  }
  
  for(r in seq_len(nrow(mod))){
    print(r)
    tmp <- mod[r, ]
    
    df_long <- get_indicators(tmp$is[[1]], format = "long")
    df_long <- df_long[df_long$type %in% c("COMBINED"), ]
    
    # Create data frame with base R
    df_subset <- data.frame(
      id = df_long$id,
      time = df_long$time,
      model = tmp$model,
      dep = tmp$dep,
      effect = df_long$effect,
      stringsAsFactors = FALSE
    )
    
    p_data <- rbind(p_data, df_subset)
  }
  
  # Filter for specific unit
  cmod <- p_data[p_data$id == unit, ]
  
  if(!blanks){
    cmod <- cmod[ave(!is.na(cmod$effect), cmod$dep, FUN = any), ]
  }
  if(nrow(cmod)== 0){return()}

  # Replace NAs with 0 for effect
  cmod$effect[is.na(cmod$effect)] <- 0

  # Split by dep and plot each
  dep_levels <- unique(cmod$dep)
  plot_list <- list()
  for(dep_val in dep_levels) {
    subdat <- cmod[cmod$dep == dep_val, ]
    p <- ggplot(subdat, aes(x = time, y = model)) +
      geom_tile(aes(fill = effect)) +
      scale_fill_gradient2(na.value = NA, name = "Effect") +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_discrete(expand = c(0,0), limits = rev, labels = function(x) x) +
      facet_grid(dep~.) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            legend.position = "bottom",
            strip.background = element_blank(),
            axis.text = element_text(size = 10, color = "black"),
            strip.text.y = element_text(size = 12)) +
      labs(x = NULL, y = NULL, title = NULL)
    plot_list[[dep_val]] <- p
  }
  grid.arrange(grobs = plot_list, ncol = 1, top = unit)
}