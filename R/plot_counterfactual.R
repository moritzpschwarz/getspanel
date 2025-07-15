#' Plot the Counterfactual Path
#'
#' @param x An object produced by the isatpanel function
#' @param plus_t Number of time periods for the counterfactual to be displayed (default = 5).
#' @param facet.scales To be passed to ggplot2::facet_wrap. Default is "free" (i.e. a separate y axis for each panel group/id). Alternatives are: "fixed", "fixed_y", "fixed_x", and "centered" (same y-axis range centered around each panel's mean).
#' @param title Plot title. Must be a character vector.
#' @param zero_line Plot a horizontal line at y = 0. Default is FALSE.
#' @param regex_exclude_indicators A regex character vector to exclude the inclusion of certain indicators in the plot. Default = NULL. Use with care, experimental.
#' @param sign Character. If "pos", only effects of indicators with positive coefficients are shown; if "neg", only negative effects are shown; if NULL (default), all indicator effects are shown
#'
#' @return A ggplot2 plot that plots an 'isatpanel' object and shows the counterfactuals for each break.
#' @export
#'
#' @importFrom ggplot2 geom_ribbon guides geom_rect
#'
#' @examples
#' \donttest{
#'data(EU_emissions_road)
#'
#'# Group specification
#'EU15 <- c("Austria", "Germany", "Denmark", "Spain", "Finland", "Belgium",
#'          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg",
#'          "Netherlands", "Greece", "Portugal", "Sweden")
#'
#'# Prepare sample and data
#'EU_emissions_road_short <- EU_emissions_road[
#'EU_emissions_road$country %in% EU15 &
#' EU_emissions_road$year >= 2000,
#' ]
#'
#'# Run
#' result <- isatpanel(
#'   data = EU_emissions_road_short,
#'   formula = ltransport.emissions ~ lgdp + I(lgdp^2) + lpop,
#'   index = c("country", "year"),
#'   effect = "twoways",
#'   fesis = TRUE,
#'   plot = FALSE,
#'   t.pval = 0.01
#' )
#' plot(result)
#' plot_grid(result)
#' plot_counterfactual(result)
#'}

plot_counterfactual <- function(x, plus_t = 5, facet.scales = "free", title = NULL, zero_line = FALSE, regex_exclude_indicators = NULL, sign = NULL) {

  df <- x$estimateddata
  indicators <- x$isatpanel.result$aux$mX
  indicators <- indicators[,!colnames(indicators) %in% names(df)]

  if(!is.null(regex_exclude_indicators)){
    indicators <- indicators[,!grepl(regex_exclude_indicators,colnames(indicators)),drop = FALSE]
  }

  df <- cbind(df,indicators)

  df_ident_fesis <- getspanel:::identify_indicator_timings(df)$fesis

  if(is.null(x$isatpanel.result$fit)){
    fitted <- as.numeric(x$isatpanel.result$mean.fit)
  } else {
    fitted <- as.numeric(x$isatpanel.result$fit)
  }

  max_times <- aggregate(x$estimateddata$time,by = list(x$estimateddata$id),FUN = function(x){max(x, na.rm = TRUE)})
  names(max_times) <- c("id","maxtime")

  df_ident <- break_uncertainty(x)
  df_ident <- merge(df_ident, max_times, by = "id")
  df_ident$origtime <- df_ident$time

  if (!is.null(sign)) {
    if (sign == "pos") {
      keep <- df_ident[df_ident$coef > 0, "name"]
      df_ident <- df_ident[df_ident$name %in% keep, , drop = FALSE]
      df_ident_fesis <- df_ident_fesis[df_ident_fesis$name %in% keep, , drop = FALSE]
    } else if (sign == "neg") {
      keep <- df_ident[df_ident$coef < 0, "name"]
      df_ident <- df_ident[df_ident$name %in% keep, , drop = FALSE]
      df_ident_fesis <- df_ident_fesis[df_ident_fesis$name %in% keep, , drop = FALSE]
    }
  } 

  if(!is.null(regex_exclude_indicators)){
    df_ident <- df_ident[!grepl(regex_exclude_indicators,df_ident$name),,drop = FALSE]
  }

  # make sure the preceding observation collapses on the last observation
  df_ident_start <- df_ident
  df_ident_start$time <- df_ident_start$time - 1
  df_ident_start$coef <- 0
  df_ident_start$sd <- 0
  df_ident_start$tci <- NA

  df_ident_overall <- rbind(df_ident_start, df_ident)
  for(i in 1:plus_t){
    intermed <- df_ident
    intermed$time <- intermed$time + i
    intermed$time <- ifelse(intermed$time > intermed$maxtime, intermed$maxtime, intermed$time)
    df_ident_overall <- rbind(df_ident_overall, intermed)
  }
  df_ident_overall <- df_ident_overall[order(df_ident_overall$name, df_ident_overall$time),]
  df_ident_overall <- df_ident_overall[!duplicated(df_ident_overall),]


  effects <- merge(x$estimateddata, df_ident_overall, by = c("id","time"), all.x = TRUE)
  # time-id pairs can also be duplicated if there are multiple rows at a time
  # this is why effects can have more rows than the original data (and we need to use merge in the row below)
  effects <- merge(effects,data.frame(x$estimateddata[,c("id","time")], fitted), by = c("id","time"))

  effects$cf <-  (effects$coef * (-1)) +  effects$fitted
  effects$cf_upr <- ((effects$coef + (1.96 * effects$sd)) * (-1)) +  effects$fitted
  effects$cf_lwr <- ((effects$coef - (1.96 * effects$sd)) * (-1)) +  effects$fitted
  effects$cf_upr99 <- ((effects$coef + (2.57 * effects$sd)) * (-1)) +  effects$fitted
  effects$cf_lwr99 <- ((effects$coef - (2.57 * effects$sd)) * (-1)) +  effects$fitted

  effects$start_rect <- effects$origtime - effects$tci
  effects$end_rect <- effects$origtime + effects$tci

  effects$cf_upr[is.na(effects$cf_upr)] <- effects$fitted[is.na(effects$cf_upr)]
  effects$cf_lwr[is.na(effects$cf_lwr)] <- effects$fitted[is.na(effects$cf_lwr)]

  sub_title <- NULL

  # Handle centered scaling option
  centered_scaling <- FALSE
  step_size <- 1.0  # Default step size
  if(facet.scales == "centered") {
    centered_scaling <- TRUE
    facet.scales <- "free"  # Use free scaling but we'll add invisible points to control limits
    
    # Find the panel with the maximum range to determine the standard range
    panel_data <- merge(df, data.frame(id = df$id, time = df$time, fitted = fitted), by = c("id", "time"))
    
    # Calculate range for each panel individually
    panel_ranges <- by(panel_data, panel_data$id, function(panel_subset) {
      all_panel_values <- c(panel_subset$y, panel_subset$fitted, 
                           effects$cf[effects$id == panel_subset$id[1]], 
                           effects$cf_upr[effects$id == panel_subset$id[1]], 
                           effects$cf_lwr[effects$id == panel_subset$id[1]])
      all_panel_values <- all_panel_values[!is.na(all_panel_values)]
      diff(range(all_panel_values))
    })
    
    # Use the maximum range as the standard range for all panels
    standard_range <- max(unlist(panel_ranges))
    
    # Calculate mean for each panel
    panel_means <- aggregate(cbind(y = panel_data$y, fitted = panel_data$fitted), 
                            by = list(id = panel_data$id), FUN = mean, na.rm = TRUE)
    panel_means$mean_all <- (panel_means$y + panel_means$fitted) / 2
    
    # Create invisible points to control y-axis limits for each panel
    # Each panel gets the same range (standard_range) centered around its own mean
    limit_points <- data.frame(
      id = rep(panel_means$id, each = 2),
      time = rep(range(df$time), length(panel_means$id)),
      y_limit = as.vector(sapply(panel_means$mean_all, function(mean) c(mean - standard_range/2, mean + standard_range/2)))
    )
    
    # Determine appropriate step size for consistent y-axis labels
    # Choose step size based on the standard range
    if(standard_range <= 2) {
      step_size <- 0.5
    } else if(standard_range <= 5) {
      step_size <- 1.0
    } else if(standard_range <= 10) {
      step_size <- 2.0
    } else if(standard_range <= 20) {
      step_size <- 5.0
    } else {
      step_size <- 10.0
    }
  }


  ggplot(df, aes(
    x = .data$time,
    y = fitted,
    group = .data$id
  )) -> g



  if(zero_line){g = g + geom_hline(aes(yintercept = 0))}

  # Add invisible points to control y-axis limits for centered scaling
  if(centered_scaling) {
    g <- g + ggplot2::geom_point(data = limit_points, aes(x = .data$time, y = .data$y_limit), alpha = 0, size = 0)
  }

  g +
    geom_line(aes(y = .data$y, color = "black"), linewidth = 0.7) +

    geom_rect(data = effects, aes(xmin = .data$start_rect, xmax = .data$end_rect, ymin = -Inf, ymax = Inf, group = .data$name),fill = "grey",alpha = 0.1, na.rm = TRUE) +

    geom_line(aes(color = "blue"),linetype = 1, linewidth = 0.5) +

    # fesis
    geom_vline(data = df_ident_fesis, aes(xintercept = .data$time,color="red")) +

    geom_ribbon(data = effects, aes(ymin = .data$cf_lwr, ymax = .data$cf_upr, fill = "red", group = .data$name), alpha = 0.5, na.rm = FALSE) +

    geom_line(data = effects, aes(y = .data$cf, color = "red", group = .data$name), na.rm = TRUE) +

    # Faceting
    facet_wrap("id", scales = facet.scales) +

    scale_color_identity(name = NULL,
                         breaks = c("black", "blue", "grey", "purple", "red","darkgreen", "orange"),
                         labels = c("y","Fitted","IIS","SIS","FESIS","CFESIS", "CSIS"),
                         guide = "legend") +

    scale_linetype(name = "Variable") +
    guides(fill = "none") +

    theme(
      strip.background = element_blank(),
      legend.key = element_rect(fill = NA),
      panel.border = element_rect(colour = "grey",fill = NA),
      panel.background = element_blank()#,
    ) +

    labs(title = title, subtitle = sub_title, y = NULL, x = NULL) -> plotoutput

  # Apply consistent y-axis breaks for centered scaling
  if(centered_scaling) {
    # Force evaluation of step_size to capture it in the closure
    step_size <- force(step_size)
    plotoutput <- plotoutput + 
      ggplot2::scale_y_continuous(breaks = function(limits) {
        # Ensure step_size is positive
        step_size <- abs(step_size)
        
        # Calculate the range of the limits
        range_size <- diff(limits)
        
        # Adjust step_size if it would result in too few breaks
        # We want at least 3-5 breaks per panel
        if(range_size / step_size < 3) {
          # Find a nice step size that gives us about 4-5 breaks
          # Use a selection of "nice" numbers
          nice_steps <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.25, 0.5, 1, 2, 2.5, 5, 10, 20, 25, 50, 100)
          target_step <- range_size / 4
          
          # Find the closest nice step size
          step_size <- nice_steps[which.min(abs(nice_steps - target_step))]
        }
        
        # Calculate the range of breaks
        min_break <- ceiling(limits[1]/step_size) * step_size
        max_break <- floor(limits[2]/step_size) * step_size
        
        # Generate sequence - only if max_break >= min_break
        if(max_break >= min_break) {
          breaks <- seq(from = min_break, to = max_break, by = step_size)
          
          # Ensure we have at least 3 breaks by expanding the range if needed
          if(length(breaks) < 3) {
            # Add breaks outside the current range
            breaks <- c(min_break - step_size, breaks, max_break + step_size)
          }
          
          # Filter breaks to only include those within reasonable bounds
          breaks[breaks >= limits[1] - step_size & breaks <= limits[2] + step_size]
        } else {
          # Fallback to default breaks if the range is too small
          pretty(limits, n = 5)
        }
      })
  }

  return(plotoutput)

}
