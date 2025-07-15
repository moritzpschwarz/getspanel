#' Plot isatpanel indicator coefficients
#'
#' @param object An object produced by the \code{\link{isatpanel}} function.
#' @param title Plot title. Must be a character vector. Default is \code{NULL}.
#' @param zero_line Logical. If \code{TRUE}, plot a horizontal line at y = 0. Default is \code{FALSE}.
#' @param scales The scales argument for \code{\link[ggplot2]{facet_wrap}}. Default is \code{"fixed"}.
#'   Use \code{"free"} to allow different y-axis scales for each facet.
#' @param regex_exclude_indicators A regular expression to filter out indicators from the plot. Combine multiple expressions with \code{"|"}. Default is \code{NULL}, meaning no indicators are excluded.
#' @param id_list A character vector of ids to include only those in the plot. 
#'   Default is \code{NULL}, meaning all ids are included.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object that plots the effect of retained IIS/FESIS/TIS-indicators.
#'
#' @details
#' The regex allows filtering multiple indicators by matching their type (e.g., \code{regex_exclude_indicators = "^iis|^tis"}) or parts of indicator names (e.g., \code{regex_exclude_indicators = "iis1|Austria|2008"}).
#' Use \code{get_indicators(object, format = "table")} to see the names of all indicators.
#'
#' The function automatically filters out CSIS and CFESIS indicators as these indicators act on different variables than IIS/FESIS/TIS indicators. See \code{\link{plot_grid}} for plotting these indicators.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' plot_indicators(isatpanel_result)
#'
#' # With title and zero line
#' plot_indicators(isatpanel_result, title = "Indicator Effects", zero_line = TRUE)
#'
#' # Exclude specific indicators
#' plot_indicators(isatpanel_result, regex_exclude_indicators = "^iis|2008")
#'
#' # Plot only specific countries
#' plot_indicators(isatpanel_result, id_list = c("Austria", "Germany"))
#' }
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_hline facet_wrap labs theme element_blank element_rect element_line scale_color_manual scale_fill_manual guides theme_minimal theme_bw guide_legend
plot_indicators <- function(object, title = NULL, zero_line = FALSE, scales = "fixed", regex_exclude_indicators = NULL, id_list = NULL) {
  # Input validation -----------------------------------------------------------
  if (!inherits(object, "isatpanel")) {
    stop("The 'object' must be of class 'isatpanel'.", call. = FALSE)
  }

  if (!scales %in% c("fixed", "free", "free_x", "free_y")) {
    stop("The 'scales' must be one of: 'fixed', 'free', 'free_x', 'free_y'.", call. = FALSE)
  }

  if (!is.null(regex_exclude_indicators) && !is.character(regex_exclude_indicators)) {
    stop("The 'regex_exclude_indicators' must be a character vector.", call. = FALSE)
  }

  # Data preparation -----------------------------------------------------------
  # Get indicators in long format and filter out CSIS and CFESIS indicators
  if (is.null(regex_exclude_indicators)) {
    regex_exclude_indicators <- "csis|cfesis"
  } else {
    regex_exclude_indicators <- paste0(regex_exclude_indicators, "|csis|cfesis")
  }
  df_long <- get_indicators(object, format = "long", regex_exclude_indicators = regex_exclude_indicators)

  # Filter by id_list if provided
  if (!is.null(id_list)) {
    df_long <- df_long[df_long$id %in% id_list, ]
  }

  # Set combined effect values to 0 to plot a complete line in all facets
  df_long$effect[df_long$type == "COMBINED" & is.na(df_long$effect)] <- 0

  # Plotting -------------------------------------------------------------------
  # Define all styling information in one place
  # This is used to keep plot aesthetics and legend consistent (The mix of bar and line styles tends to create an inconsistent legend)
  # This also allows easy updates to styles in one place. Switching between bar and line elements requires additional modification in the plot code.
  indicator_styles <- list(
    "IIS" = list(
      color = "grey",
      fill = "grey",
      shape = 15,
      linetype = "blank",
      width = 0.75,
      legend_fill = "grey"
    ),
    "FESIS" = list(
      color = "red",
      fill = "red",
      shape = NA,
      linetype = "solid",
      width = 0.75,
      legend_fill = "transparent"
    ),
    "TIS" = list(
      color = "lightblue",
      fill = "lightblue",
      shape = NA,
      linetype = "solid",
      width = 0.75,
      legend_fill = "transparent"
    ),
    "COMBINED" = list(
      color = "black",
      fill = "black",
      shape = NA,
      linetype = "dashed",
      width = 0.5,
      legend_fill = "transparent"
    )
  )

  # Set factor levels to ensure consistent ordering and always show all types
  df_long$type <- factor(df_long$type, levels = names(indicator_styles))

  # Create the plot
  g <- ggplot(df_long, aes(x = time, y = effect, group = name, color = type, fill = type)) +
    # Bars for IIS
    geom_col(
      data = subset(df_long, type %in% c("IIS")),
      width = indicator_styles[["IIS"]]$width,
      show.legend = TRUE
    ) +
    # Lines for FESIS, TIS
    geom_line(
      data = subset(df_long, type %in% c("FESIS", "TIS")),
      # Both FESIS and TIS use same linewidth
      linewidth = indicator_styles[["FESIS"]]$width,
      show.legend = TRUE
    ) +
    # Dashed line for COMBINED
    geom_line(
      data = subset(df_long, type == "COMBINED"),
      linewidth = indicator_styles[["COMBINED"]]$width,
      linetype = indicator_styles[["COMBINED"]]$linetype,
      show.legend = TRUE
    ) +
    # Set colors for the indicators, drop = FALSE ensures all levels shown
    scale_color_manual(values = sapply(indicator_styles, function(x) x$color), drop = FALSE) +
    scale_fill_manual(values = sapply(indicator_styles, function(x) x$color), drop = FALSE) +
    # Create custom legend using centralized style information
    guides(
      color = guide_legend(
        title = "Indicator Type",
        override.aes = list(
          shape = sapply(indicator_styles, function(x) x$shape),
          linetype = sapply(indicator_styles, function(x) x$linetype),
          linewidth = sapply(indicator_styles, function(x) x$width),
          fill = sapply(indicator_styles, function(x) x$legend_fill)
        )
      ),
      fill = "none"
    ) +
    # Facet by id
    facet_wrap(~id, scales = scales) +
    theme_minimal() +
    theme_bw() +
    theme(panel.grid = element_line(),
          panel.border = element_rect(fill = NA),
          strip.background = element_blank()) +
    labs(x = NULL, y = "Indicator Effect (Coefficient * Value)")

  # Add title if provided
  if (!is.null(title)) {
    g <- g + labs(title = title)
  }

  # Add zero line if requested
  if (zero_line) {
    g <- g + geom_hline(
      aes(yintercept = 0),
      linewidth = 0.25,
      color = "blue"
    )
  }

  return(g)
}
