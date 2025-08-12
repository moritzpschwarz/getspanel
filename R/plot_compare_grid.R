#' Plotting multiple isatpanel objects for comparison across units
#'
#' Creates a comparative visualization of detected breaks across multiple isatpanel model specifications. The function displays results as a heatmap similar to \code{\link{plot_grid}}.
#'
#' @param mod A data frame containing isatpanel objects and model descriptions.
#'   Must have at least two columns: one with isatpanel objects (wrapped in lists) and another with unique model descriptions.
#' @param is_col Character. Name of the column containing isatpanel objects.
#'   Default is "is".
#' @param model_col Character. Name of the column containing model descriptions.
#'   Default is "model".
#' @param panel Character. How to organize the plot panels: "unit" (default) displays results by cross-sectional unit, "model" displays by model.
#' @param title Character. Plot title. If NULL, no title is displayed.
#' @param include_blanks Logical. Whether to include combinations where no indicators are detected (creates blank rows). Default is TRUE.
#' @param id_list Character vector. If provided, only these units will be included in the plot.
#' @param mod_list Character vector. If provided, only these models will be included in the plot.
#' @param sign Character. If "pos", only positive effects are shown; if "neg", only negative effects are shown; if NULL (default), all effects are shown.
#' @param regex_exclude_indicators A regular expression to filter out indicators from the plot. Combine multiple expressions with \code{"|"}. Default is \code{NULL}, meaning no indicators are excluded. See \code{\link{get_indicators}} for details on this parameter.
#'
#' @return A ggplot object displaying the indicator effects across the specified models and units.
#'
#' @details
#' The function extracts indicator data from each isatpanel object and combines
#' them into a single visualization. Only combined effects from IIS/TIS/FESIS indicators are plotted, but types can be filtered using the \code{regex_exclude_indicators} parameter (e.g. \code{regex_exclude_indicators = "^iis|^tis"} to only show FESIS indicator effects). For separate plots of IIS, TIS, and FESIS indicators in a single model, use \code{\link{plot_indicators}}.
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 scale_x_continuous scale_y_discrete facet_grid theme_bw theme labs element_text element_blank element_rect
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(gets)
#' library(getspanel)
#'
#' # Load example data
#' data(compare_models_example, package = "getspanel")
#'
#' # Basic comparison plot
#' plot_comp(compare_models_example)
#'
#' # Example showing only FESIS indicator effects
#' plot_comp(compare_models_example, regex_exclude_indicators = "^iis|^tis")
#'
#' # Example isolating a specific unit
#' plot_comp(compare_models_example, id_list = c("Argentina"))
#'
#' # Example isolating a specific model
#' plot_comp(compare_models_example, mod_list = compare_models_example$model[[1]], panel = "model")
#' # This creates the same plot as plot_grid(compare_models_example$is[[1]])
#' }
#'
plot_compare_grid <- function(mod, is_col = "is", model_col = "model", panel = "unit", title = NULL, include_blanks = TRUE, id_list = NULL, mod_list = NULL, sign = NULL, regex_exclude_indicators = NULL) {
  # Input validation -----------------------------------------------------------
  # Check basic data frame structure
  if (!is.data.frame(mod) || ncol(mod) < 2) {
    stop("The 'mod' must be a data frame with at least two columns: one with isatpanel objects and another with model descriptions.", call. = FALSE)
  }

  if (nrow(mod) == 0) {
    stop("No models to plot.", call. = FALSE)
  }

  # Check column existence
  if (!is_col %in% names(mod)) {
    stop(paste("Column", is_col, "not found in data frame."), call. = FALSE)
  }

  if (!model_col %in% names(mod)) {
    stop(paste("Column", model_col, "not found in data frame."), call. = FALSE)
  }

  if (!panel %in% c("unit", "model")) {
    stop("The 'panel' must be either 'unit' or 'model'.", call. = FALSE)
  }

  if (!is.null(sign) && !sign %in% c("pos", "neg")) {
    stop("The 'sign' must be one of: 'pos', 'neg', NULL (both).", call. = FALSE)
  }

  # Check for duplicate model names
  if (any(duplicated(mod[[model_col]]))) {
    warning("Duplicate model names detected. This may cause issues in the plot.", call. = FALSE)
  }

  # Data extraction and preparation --------------------------------------------
  # Pre-allocate list for plot data of each model
  plot_data_list <- vector("list", nrow(mod))

  # Extract indicator data from each model
  for (n in seq_len(nrow(mod))) {
    tmp <- mod[n, ]

    # Validate individual model object
    if (!is.list(tmp[[is_col]]) || length(tmp[[is_col]]) != 1 || !inherits(tmp[[is_col]][[1]], "isatpanel")) {
      stop(paste("Row", n, "does not contain a valid isatpanel object in column", is_col), call. = FALSE)
    }

    if (!is.character(tmp[[model_col]]) || length(tmp[[model_col]]) != 1) {
      stop(paste("Row", n, "does not contain a valid model description in column", model_col), call. = FALSE)
    }

    # Get indicators in long format and filter out CSIS and CFESIS indicators
    if (is.null(regex_exclude_indicators)) {
      regex_exclude_indicators <- "csis|cfesis"
    } else {
      regex_exclude_indicators <- paste0(regex_exclude_indicators, "|csis|cfesis")
    }
    df_long <- get_indicators(tmp[[is_col]][[1]], format = "long", regex_exclude_indicators = regex_exclude_indicators)

    # Only keep COMBINED data for this plot
    df_long <- df_long[df_long$type %in% c("COMBINED"), ]

    # Add model description to the data and add data to the list
    if (nrow(df_long) > 0) {
      plot_data_list[[n]] <- data.frame(
        id = df_long$id,
        time = df_long$time,
        coef = df_long$coef,
        effect = df_long$effect,
        model = tmp[[model_col]],
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine all datasets
  plot_data <- do.call(rbind, plot_data_list[!sapply(plot_data_list, is.null)])

  if (is.null(plot_data) || nrow(plot_data) == 0) {
    stop("No valid indicator data found in any of the models.", call. = FALSE)
  }

  # Apply user filters ---------------------------------------------------------
  if (!is.null(id_list)) {
    plot_data <- plot_data[plot_data$id %in% id_list, , drop = FALSE]
  }

  if (!is.null(mod_list)) {
    plot_data <- plot_data[plot_data$model %in% mod_list, , drop = FALSE]
  }

  if (!is.null(sign)) {
    if (sign == "pos") {
      plot_data <- plot_data[plot_data$coef > 0, , drop = FALSE]
    } else if (sign == "neg") {
      plot_data <- plot_data[plot_data$coef < 0, , drop = FALSE]
    }
  }

  if (nrow(plot_data) == 0) {
    stop("No data remaining after applying filters.", call. = FALSE)
  }

  if (!include_blanks) {
    # get_indicators() long format returns a full grid of values for every model/id combination
    # We remove all NA values here to get rid of model/id combinations where all effect values are NA
    # ggplot2 will still plot full rows where there is at least one non-NA value
    plot_data <- plot_data[!is.na(plot_data$effect), ]
  }

  # Handle panel grouping switch
  if (panel == "model") {
    temp_id <- plot_data$id
    plot_data$id <- plot_data$model
    plot_data$model <- temp_id
  }

  # Create and return the plot -------------------------------------------------
  p <- ggplot(plot_data, aes(x = .data$time, y = .data$model)) +
    geom_tile(aes(fill = .data$effect), na.rm = TRUE) +
    scale_fill_gradient2(na.value = NA, name = "Effect", mid = "white") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    facet_grid(id ~ ., scales = "free_y", space = "free_y") +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA),
      strip.background = element_blank(),
      strip.text.y = element_text(size = 12, angle = 0),
      plot.caption = element_text(size = 12, hjust = 0.5),
      legend.position = "bottom"
    ) +
    labs(x = NULL, y = NULL, title = title)

  return(p)
}
