#' Plot the Indicators for an isatpanel object
#'
#' @param x An object produced by the isatpanel function
#' @param max.id.facet The resulting plot will be faceted for each individual in the panel. Beyond a certain number, this might result in unreadable figures. Default set at 16.
#' @param facet.scales To be passed to ggplot2::facet_wrap. Default is "free" (i.e. a separate y axis for each panel group/id). Alternatives are: "fixed", "fixed_y", and "fixed_x".
#' @param title Plot title. Must be a character vector.
#' @param zero_line Plot a horizontal line at y = 0. Default is FALSE.
#' @param ... Further arguments to be passed to ggplot2.
#'
#' @return A ggplot2 plot that plots an 'isatpanel' object and shows observed data, the fitted values, and all identified breaks and impulses.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs theme element_blank element_rect element_line geom_hline geom_vline scale_color_identity scale_linetype
#'
plot_indicators <- function(x, include.fixed.effects = FALSE, max.id.facet = 16, facet.scales = "free", title = NULL, zero_line = FALSE, regex_exclude_indicators = NULL, ...) {
  # interactive = TRUE, currently not implemented. Roxygen: Logical (TRUE or FALSE). Default is TRUE. When True, plot will be passed to plotly using ggplotly.


  df <- x$estimateddata
  indicators <- x$isatpanel.result$aux$mX
  indicators <- indicators[, !colnames(indicators) %in% names(df), drop = FALSE]
  fixed_effects <- cbind(df[, c("id", "time")], indicators[, grepl("^id|^time", colnames(indicators)), drop = FALSE])
  indicators <- indicators[, !grepl("^id|^time", colnames(indicators)), drop = FALSE]
  if (!is.null(regex_exclude_indicators)) {
    indicators <- indicators[, !grepl(regex_exclude_indicators, colnames(indicators)), drop = FALSE]
  }

  df <- cbind(df, indicators)


  if (dim(indicators)[2] != 0) {
    if (is.null(x$isatpanel.result$fit)) {
      fitted <- as.numeric(x$isatpanel.result$mean.fit)
    } else {
      fitted <- as.numeric(x$isatpanel.result$fit)
    }

    # fixed effects
    varying_vars_FE <- names(fixed_effects)[!names(fixed_effects) %in% c("id", "time")]
    fixed_effects_l <- reshape(fixed_effects,
      varying = varying_vars_FE,
      idvar = c("id", "time"),
      v.names = "value",
      timevar = "name",
      times = varying_vars_FE,
      direction = "long"
    )


    fixed_effects_l_merged <- merge(fixed_effects_l,
      data.frame(
        name = names(coef(x$isatpanel.result)),
        coef = coef(x$isatpanel.result)
      ),
      by = "name", all.x = TRUE
    )

    fixed_effects_l_merged$effect <- fixed_effects_l_merged$value * fixed_effects_l_merged$coef
    fixed_effects_toplot <- aggregate(fixed_effects_l_merged$effect, by = list(fixed_effects_l_merged$time, fixed_effects_l_merged$id), function(x) {
      sum(x, na.rm = TRUE)
    })
    names(fixed_effects_toplot) <- c("time", "id", "fixed.effect")
    fixed_effects_toplot$id <- factor(fixed_effects_toplot$id, levels = rev(unique(fixed_effects_toplot$id))) # swapping to make sure they are in order

    # fixed effects
    indicators_df <- cbind(df[, names(df) %in% c("id", "time")], indicators)
    varying_vars <- names(indicators_df)[!names(indicators_df) %in% c("id", "time", "y", "fitted")]

    indicators_l <- reshape(indicators_df,
      varying = varying_vars,
      idvar = c("id", "time"),
      v.names = "value",
      timevar = "name",
      times = varying_vars,
      direction = "long"
    )


    # introduce facets
    default_facet_name <- "Intercept (IIS, FESIS, TIS)"
    # default_facet_name <- "Intercept (IIS, FESIS)"
    indicators_l$facet <- default_facet_name

    # Deal with TIS within facets
    # indicators_l[grepl("^tis",indicators_l$name),"value"] <- ifelse(indicators_l[grepl("^tis",indicators_l$name),"value"] != 0, 1, 0)
    # indicators_l[grepl("^tis",indicators_l$name),"facet"] <- "TIS"

    # Deal with CSIS within facets
    indicators_l[grepl("\\.csis[0-9]+", indicators_l$name), "value"] <- ifelse(indicators_l[grepl("\\.csis[0-9]+", indicators_l$name), "value"] != 0, 1, 0)
    indicators_l[grepl("\\.csis[0-9]+", indicators_l$name), "facet"] <- paste0("CSIS: ", gsub("\\.csis[0-9]+", "", indicators_l[grepl("\\.csis[0-9]+", indicators_l$name), "name"]))

    # Deal with CFESIS within facets
    indicators_l[grepl("\\.cfesis[0-9]+", indicators_l$name), "value"] <- ifelse(indicators_l[grepl("\\.cfesis[0-9]+", indicators_l$name), "value"] != 0, 1, 0)
    indicators_l[grepl("\\.cfesis[0-9]+", indicators_l$name), "facet"] <- paste0("CFESIS: ", gsub("\\.[0-9]+$", "", gsub("\\.cfesis[0-9]+", "", indicators_l[grepl("\\.cfesis[0-9]+", indicators_l$name), "name"])))

    # Control the order of the facets
    facet_order <- unique(indicators_l$facet)
    facet_order <- c(default_facet_name, facet_order[!facet_order %in% default_facet_name])
    indicators_l$facet <- factor(indicators_l$facet, levels = facet_order)

    indicators_l_merged <- merge(indicators_l,
      data.frame(
        name = names(coef(x$isatpanel.result)),
        coef = coef(x$isatpanel.result)
      ),
      by = "name", all.x = TRUE
    )

    indicators_l_merged$effect <- indicators_l_merged$value * indicators_l_merged$coef

    # Aggregate by time, id, facet, and indicator name to keep lines separate
    indicators_toplot <- aggregate(
      indicators_l_merged$effect,
      by = list(
        time = indicators_l_merged$time,
        id = indicators_l_merged$id,
        facet = indicators_l_merged$facet,
        name = indicators_l_merged$name
      ),
      FUN = function(x) sum(x, na.rm = TRUE)
    )

    # indicators_toplot <- aggregate(indicators_l_merged$effect, by = list(indicators_l_merged$time, indicators_l_merged$id, indicators_l_merged$facet), function(x){sum(x,na.rm = TRUE)})
    names(indicators_toplot) <- c("time", "id", "facet", "name", "effect")
    # indicators_toplot[indicators_toplot$effect == 0,"effect"] <- NA
    indicators_toplot$id <- factor(indicators_toplot$id, levels = sort(unique(indicators_toplot$id))) # swapping the order of the factors to make sure they are in alphabetical order in the plot

    indicators_toplot <- indicators_toplot %>%
      group_by(id, facet, name) %>%
      mutate(
        first_nonzero = min(time[effect != 0], na.rm = TRUE),
        effect = ifelse(time < first_nonzero, NA, effect)
      ) %>%
      ungroup() %>%
      select(-first_nonzero)

    # library(dplyr)

    indicators_toplot <- indicators_toplot %>%
      group_by(name) %>%
      mutate(
        # Find the id where this indicator is nonzero at least once
        id_with_indicator = id[which.max(abs(effect))],
        # Set effect to NA for all other ids
        effect = ifelse(id == id_with_indicator, effect, NA)
      ) %>%
      ungroup() %>%
      select(-id_with_indicator)

    # Calculate combined effect per country (id) and time
    combined_effect <- indicators_toplot %>%
      group_by(time, id) %>%
      summarise(combined_effect = sum(effect, na.rm = TRUE), .groups = "drop")

    if (include.fixed.effects) {
      indicators_toplot <- merge(indicators_toplot, fixed_effects_toplot, by = c("time", "id"), all = TRUE)
      indicators_toplot$effect <- indicators_toplot$effect + indicators_toplot$fixed.effect
    }

    # # Figure out the colours if there is only one break
    # col_limits <- c(min(indicators_toplot$effect, na.rm = TRUE), max(indicators_toplot$effect, na.rm = TRUE))

    # if(col_limits[1] == col_limits[2]){
    #   if(col_limits[1]<0){col_limits[2] <- col_limits[2] * -1} else {col_limits[1] <- col_limits[1] * -1}
    # } else if(sign(col_limits[1]) == sign(col_limits[2])){
    #   if(sign(col_limits[1]) < 0){col_limits[2] <- col_limits[1]*-1} else {col_limits[1] <- col_limits[2]*-1}
    # }
    # print(names(indicators_toplot))

    # Add a flag for iis indicators
    indicators_toplot <- indicators_toplot %>%
      mutate(
        iis_indicator = grepl("^iis", name, ignore.case = TRUE),
        indicator_type = tolower(sub("^([a-z]+).*", "\\1", name))
      )

    ggplot(indicators_toplot, aes(x = .data$time, y = .data$effect, group = .data$name, color = .data$indicator_type)) +
      # IIS bars: grey fill, legend label "IIS"
      geom_col(
        data = subset(indicators_toplot, iis_indicator),
        aes(fill = .data$indicator_type),
        position = "identity",
        show.legend = TRUE
      ) +
      # All lines (FESIS, TIS, etc.): colored by indicator_type, shown in legend
      geom_line(
        data = subset(indicators_toplot, !iis_indicator),
        linewidth = 0.75,
        show.legend = TRUE
      ) +
      # Combined effect line
      geom_line(
        data = combined_effect,
        linetype = "dashed",
        aes(x = time, y = combined_effect, group = id),
        color = "black",
        linewidth = 0.5,
        inherit.aes = FALSE,
        show.legend = FALSE
      ) +
      facet_wrap(~ .data$id, scales = facet.scales) +
      scale_fill_manual(
        name = "Indicator",
        values = c("iis" = "grey", "fesis" = "red", "tis" = "lightblue"),
        breaks = c("iis", "fesis", "tis"),
        labels = c("IIS", "FESIS", "TIS")
      ) +
      scale_color_manual(
        name = "Indicator",
        values = c("iis" = "grey", "fesis" = "red", "tis" = "lightblue"),
        breaks = c("iis", "fesis", "tis"),
        labels = c("IIS", "FESIS", "TIS")
      ) +
      theme_bw() +
      theme(
        panel.grid = element_line(),
        panel.border = element_rect(fill = NA),
        strip.background = element_blank()
      ) +
      labs(x = NULL, y = NULL)

    # ggplot(indicators_toplot, aes(x = .data$time, y = .data$effect, color = .data$name)) +
    #   geom_line() +
    #   facet_wrap(~.data$id, scales = facet.scales) +
    #   theme_bw() +
    #   theme(panel.grid = element_blank(),
    #         panel.border = element_rect(fill = NA),
    #         strip.background = element_blank()) +
    #   labs(x = NULL, y = NULL)

    # ggplot(indicators_toplot, aes(x = .data$time, y = .data$effect)) +
    #   geom_line() +
    #   facet_wrap(~.data$id, scales = facet.scales) +
    #   theme_bw() +
    #   theme(panel.grid = element_blank(),
    #         panel.border = element_rect(fill = NA),
    #         strip.background = element_blank()) +
    #   labs(x = NULL, y = NULL)

    # ggplot(indicators_toplot, aes(x = .data$time, y = .data$id, fill = .data$effect)) +
    #   geom_tile(na.rm = TRUE) +
    #   #scale_fill_viridis_c(na.value = NA) +
    #   scale_fill_gradient2(limits = col_limits, na.value = NA, name = "Effect", midpoint = 0) +
    #   scale_x_continuous(expand = c(0, 0)) +
    #   scale_y_discrete(expand = c(0, 0)) +
    #   facet_wrap(~.data$facet) +
    #   theme_bw() +
    #   theme(panel.grid = element_blank(),
    #         panel.border = element_rect(fill = NA),
    #         strip.background = element_blank()) +
    #   labs(x = NULL, y = NULL)
  } else {
    message("No indicators identified in the isatpanel object. No plot produced.")
  }
}
