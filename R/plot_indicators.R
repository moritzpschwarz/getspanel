#' Plot isatpanel indicator coefficients
#'
#' @param x An object produced by the isatpanel function
#' @param title Plot title. Must be a character vector.
#' @param regex_exclude_indicators A regular expression to filter out indicators from the plot. Filter out multiple indicators using "|" (e.g. regex_exclude_indicators = "cfesis|csis". Filter out Unit or Time fixed effects indicators using "id" or "time". Default is NULL, meaning no indicators are excluded.
#' @param zero_line Logical. If TRUE, plot a horizontal line at y = 0. Default is FALSE.
#' @param include_fixed_effects Logical. If TRUE, fixed effects are included in the plot. Default is FALSE.
#'
#' @return A ggplot2 plot that plots an 'isatpanel' object and shows observed data, the fitted values, and all identified breaks and impulses.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_hline facet_wrap labs theme element_blank element_rect element_line scale_color_manual scale_fill_manual guides theme_minimal theme_bw guide_legend
#' @importFrom gets coef.gets
plot_indicators <- function(x, title = NULL, regex_exclude_indicators = NULL, zero_line = FALSE, include_fixed_effects = FALSE) {
  df_long <- get_indicators(x, format = "long")
  df_long <- df_long[!df_long$type %in% c("CSIS", "CFESIS"), ]
  df_long$effect[df_long$type == "COMBINED" & is.na(df_long$effect)] <- 0

  indicator_colors <- c(
    "IIS" = "grey",
    "CSIS" = "orange",
    "FESIS" = "red",
    "CFESIS" = "darkgreen",
    "TIS" = "lightblue",
    "COMBINED" = "black",
    "Unit Fixed Effects" = "purple",
    "Time Fixed Effects" = "violetred4"
  )

  # Create the plot
  g <- ggplot(df_long, aes(x = time, y = effect, group = name, color = type, fill = type)) +
    # Bars for IIS and CSIS
    geom_col(
      data = subset(df_long, type %in% c("IIS")),
      width = 0.75,
    ) +
    # Lines for FESIS, CFESIS, TIS
    geom_line(
      data = subset(df_long, type %in% c("FESIS", "CFESIS", "TIS", "CSIS", "Time Fixed Effects", "Unit Fixed Effects")),
      linewidth = 0.75
    ) +
    # Line for Combined, set color manually to black
    geom_line(
      data = subset(df_long, type == "COMBINED"),
      aes(color = type),
      linewidth = 0.75,
      linetype = "dashed"
    ) +
    facet_wrap(~id, scales = "free") +
    theme_minimal() +
    # Customize colors for each indicator type
    scale_color_manual(
      values = indicator_colors,
      name = "Indicator Type"
    ) +
    scale_fill_manual(
      values = indicator_colors,
      name = "Indicator Type"
    ) +
    # Customize the legend to show line and bar styles in one legend
    guides(
      color = guide_legend(
        override.aes = list(
          linetype = c(
            setNames(
              rep("solid", length(unique(df_long$type))),
              unique(df_long$type)
            ),
            COMBINED = "dashed"
          )[levels(factor(df_long$type, levels = unique(df_long$type)))],
          shape = NA
        )
      )
    ) +
    theme_bw() +
    theme(panel.grid = element_line(),
          panel.border = element_rect(fill = NA),
          strip.background = element_blank()) +
    labs(x = NULL, y = NULL)

  # Add title if provided
  if (!is.null(title)) {
    g <- g + labs(title = title)
  }

  # Add zero line if requested
  if (zero_line) {
    g <- g + geom_hline(
      aes(yintercept = 0),
      linewidth = 0.5,
      color = "blue"
    )
  }

  return(g)
}
