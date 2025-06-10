#' Plot isatpanel indicator coefficients
#'
#' @param x An object produced by the isatpanel function
#' @param title Plot title. Must be a character vector.
#' @param regex_exclude_indicators A regular expression to filter out indicators from the plot. Default is NULL, meaning no indicators are excluded.
#' @param zero_line Plot a horizontal line at y = 0. Default is FALSE.
#'
#' @return A ggplot2 plot that plots an 'isatpanel' object and shows observed data, the fitted values, and all identified breaks and impulses.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_line facet_wrap labs theme element_blank element_rect element_line scale_color_manual scale_fill_manual guides
#' @importFrom dplyr mutate left_join bind_rows group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
plot_indicators <- function(x, title = NULL, regex_exclude_indicators = NULL, zero_line = FALSE) {
  panel_rows <- x$estimateddata
  indicator_table <- x$isatpanel.result$aux$mX

  # Remove input variables and fixed effects from the indicator table
  indicator_table <- indicator_table[, !colnames(indicator_table) %in% names(panel_rows), drop = FALSE]
  indicator_table <- indicator_table[, !grepl("^id|^time", colnames(indicator_table)), drop = FALSE]

  # If regex_exclude_indicators is provided, filter out those indicators
  if (!is.null(regex_exclude_indicators)) {
    indicator_table <- indicator_table[, !grepl(regex_exclude_indicators, colnames(indicator_table)), drop = FALSE]
  }

  # Stop if no indicators are left to plot
  if (ncol(indicator_table) == 0) {
    stop("No indicators found in the model. Please check your model specification.")
  }

  # Remove input variables from the panel rows and only keep id and time columns
  panel_rows <- panel_rows[, c("id", "time"), drop = FALSE]
  df <- cbind(panel_rows, indicator_table)

  # Reshape the data to long format where each id, time, and indicator combination is a row
  df_long <- df %>%
    pivot_longer(
      cols = -c(id, time),
      names_to = "indicator",
      values_to = "value"
    )

  # Extract the indicator type
  df_long <- df_long %>%
    mutate(
      indicator_type = case_when(
        grepl("^iis[0-9]+$", indicator) ~ "IIS",
        grepl("^fesis[a-zA-Z]+\\.[0-9]+$", indicator) ~ "FESIS",
        grepl("^tis[a-zA-Z]+\\.[0-9]+$", indicator) ~ "TIS",
        grepl("^.+\\.cfesis.+\\.[0-9]+$", indicator) ~ "CFESIS",
        grepl("^.+\\.csis[0-9]+$", indicator) ~ "CSIS",
        TRUE ~ "Other"
      )
    )

  # Add coefficients to the data and calculate effects when indicators apply
  coefficients <- tibble(
    indicator = names(coef(x$isatpanel.result)),
    coef = coef(x$isatpanel.result)
  )

  # TODO: Need to set CSIS and CFESIS value to 1 first?
  df_long <- df_long %>%
    left_join(coefficients, by = "indicator") %>%
    mutate(effect = value * coef)

  # Replace 0 effects with NA for plotting
  df_long <- df_long %>%
    mutate(
      effect = ifelse(effect == 0, NA, effect)
    )

  # Combined effects per indicator_type per id and time
  # test <- df_long %>%
  #   group_by(id, time, indicator_type) %>%
  #   summarise(effect = sum(effect, na.rm = TRUE), .groups = "drop")

  # Combined effects of all indicators per id and time
  test <- df_long %>%
    bind_rows(
      df_long %>%
        group_by(id, time) %>%
        summarise(
          indicator = "combined",
          value = NA,
          indicator_type = "Combined",
          coef = NA,
          effect = sum(effect, na.rm = TRUE),
          .groups = "drop"
        )
    )

  # Create the plot
  g <- ggplot(test, aes(x = time, y = effect, group = indicator, color = indicator_type, fill = indicator_type)) +
    # Bars for IIS and CSIS
    geom_col(
      data = subset(test, indicator_type %in% c("IIS")),
      width = 0.75,
    ) +
    # Lines for FESIS, CFESIS, TIS
    geom_line(
      data = subset(test, indicator_type %in% c("FESIS", "CFESIS", "TIS", "CSIS")),
      size = 0.75
    ) +
    # Line for Combined, set color manually to black
    geom_line(
      data = subset(test, indicator_type == "Combined"),
      aes(color = indicator_type),
      size = 0.75,
      linetype = "dashed"
    ) +
    facet_wrap(~id, scales = "free") +
    theme_minimal() +
    # Customize colors for each indicator type
    scale_color_manual(
      values = c(
        "IIS" = "grey",
        "CSIS" = "orange",
        "FESIS" = "red",
        "CFESIS" = "darkgreen",
        "TIS" = "lightblue",
        "Combined" = "black"
      ),
      name = "Indicator Type"
    ) +
    scale_fill_manual(
      values = c(
        "IIS" = "grey",
        "CSIS" = "orange",
        "FESIS" = "red",
        "CFESIS" = "darkgreen",
        "TIS" = "lightblue",
        "Combined" = "black"
      ),
      name = "Indicator Type"
    ) +
    # Customize the legend to show line and bar styles in one legend
    guides(
      color = guide_legend(
        override.aes = list(
          linetype = c(
            setNames(
              rep("solid", length(unique(test$indicator_type))),
              unique(test$indicator_type)
            ),
            Combined = "dotted"
          )[levels(factor(test$indicator_type, levels = unique(test$indicator_type)))],
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
