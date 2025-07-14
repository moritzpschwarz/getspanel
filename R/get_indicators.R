#' Extract the retained indicators from an `isatpanel` object
#'
#' @param object An object produced by the `isatpanel` function.
#' @param uis_breaks A string with the names of user-specified indicators.
#' @param format A string indicating the format of the output. Can be "list", "table" or "long". Default is "list".
#'
#' @return A list of data frames (if `format = "list"`) or a single data frame (if `format = "table"` or `format = "long"`) containing information about the indicators identified in the `isatpanel` object. The list is provided for backward-compatibility and contains differently shaped data frames for each indicator type that can be accessed by name ("impulses", "fesis", "tis", "cfesis", "csis", "uis_breaks"). The "table" format returns a single data frame with all indicators that can be filtered by the "type" column. The "long" format returns a panel-shaped data frame that can be used for plotting (see `plot_indicators` and `plot_comp`). The column names between the formats differ slightly (see examples).
#'
#' @export
#'
#' @importFrom gets coef.gets
#' @importFrom stats reshape aggregate
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
#'
#' # Different ways to retrieve information about the indicators:
#' get_indicators(result, format = "list")$impulses # Legacy list format
#' get_indicators(result, format = "table")[type == "IIS"] # Table format with additional columns "coef" and "variable" (used for CFESIS/CSIS)
#' get_indicators(result, format = "long") # Long format with additional "coef", "value", and "effect" columns (useful to see the time-varying impact of e.g. trend indicators) and a "combined" indicator that sums up all IIS/TIS/FESIS-effects per id/time
#' plot_indicators(result) # Plots the long format
#' }
get_indicators <- function(object, uis_breaks = NULL, format = "list") {
  # Get the indicator matrix from the isatpanel object
  df <- object$isatpanel.result$aux$mX

  # Add the panel rows
  panel_rows <- object$finaldata[, c("id", "time"), drop = FALSE]
  df <- cbind(panel_rows, df)

  # Get indicator names from the isatpanel object
  indicator_names <- object$isatpanel.result$ISnames

  # Reshape to long format (one row for every id/time/indicator combination)
  all_indicators_long <- reshape(
    df,
    varying = indicator_names,
    idvar = c("id", "time"),
    v.names = "value",
    timevar = "name",
    times = indicator_names,
    direction = "long"
  )
  # Only keep rows where indicators are active (indicated by any non-zero value)
  all_indicators_long <- all_indicators_long[all_indicators_long$value != 0, ]

  # Add coefficients
  coefficients <- data.frame(
    name = colnames(object$isatpanel.result$aux$mX),
    coef = object$isatpanel.result$coefficients
  )
  all_indicators_long <- merge(all_indicators_long, coefficients, by = c("name"), all.x = TRUE)

  # Initialize output based on format
  if (format == "list") {
    output <- list()
  } else {
    output <- data.frame()
  }

  # IIS - Impulse Indicators
  iis_result <- process_indicators(all_indicators_long, "^iis[0-9]+", "IIS", format)
  if (format == "list") {
    output$impulses <- iis_result
  } else {
    output <- rbind(output, iis_result)
  }

  # FESIS - Fixed Effects Step Indicators
  fesis_result <- process_indicators(all_indicators_long, "^fesis.+\\.[0-9]+$", "FESIS", format)
  if (format == "list") {
    output$fesis <- fesis_result
  } else {
    output <- rbind(output, fesis_result)
  }

  # TIS - Trend Indicators
  tis_result <- process_indicators(all_indicators_long, "^tis.+\\.[0-9]+$", "TIS", format)
  if (format == "list") {
    output$tis <- tis_result
  } else {
    output <- rbind(output, tis_result)
  }

  # CFESIS - Conditional Fixed Effects Step Indicators
  cfesis_result <- process_indicators(all_indicators_long, "^.+\\.cfesis.+\\.[0-9]+$", "CFESIS", format, extract_variable = TRUE)
  if (format == "list") {
    # Some adjustments to column names to be compatible with plot.isatpanel
    rownames(cfesis_result) <- cfesis_result[, "name"]
    cfesis_result[, c("name")] <- cfesis_result[, c("variable")]
    cfesis_result <- cfesis_result[, c("id", "time", "name", "type", "coef")]
    output$cfesis <- cfesis_result
  } else {
    output <- rbind(output, cfesis_result)
  }

  # CSIS - Common Step Indicators
  csis_result <- process_indicators(all_indicators_long, "^.+\\.csis[0-9]+$", "CSIS", format, extract_variable = TRUE)
  if (format == "list") {
    # Some adjustments to column names to be compatible with plot.isatpanel
    csis_result <- csis_result[!duplicated(csis_result[, "name"]), ]
    rownames(csis_result) <- csis_result[, "name"]
    csis_result[, c("name")] <- csis_result[, c("variable")]
    csis_result <- csis_result[, c("time", "name", "type", "coef")]
    output$csis <- csis_result
  } else {
    output <- rbind(output, csis_result)
  }

  # Handle UIS (User-specified Indicators) if present
  if (!is.null(uis_breaks)) {
    uis_result <- process_indicators(all_indicators_long[all_indicators_long$name %in% uis_breaks, ], ".*", "UIS", format)
    if (list) {
      output$uis_breaks <- uis_result
    } else {
      output <- rbind(output, uis_result)
    }
  }

  if (format == "long") {
    # Calculate combined effect for each id/time by summing effect
    # Exclude cfesis and csis indicators from combined effect
    combined <- aggregate(
      effect ~ id + time,
      data = output[!output$type %in% c("CFESIS", "CSIS"), ],
      sum
    )
    # Expand to full id/time grid to deliver correct bounds for plotting
    # Default effect = NA when no indicators are active, so plotting can handle how to display these (e.g., plot_comp filters for NA when blanks = FALSE)
    all_ids <- unique(panel_rows$id)
    all_times <- unique(panel_rows$time)
    full_grid <- expand.grid(id = all_ids, time = all_times)
    combined <- merge(full_grid, combined, by = c("id", "time"), all.x = TRUE)
    combined$effect[is.na(combined$effect)] <- NA
    combined$type <- "COMBINED"
    combined$name <- "combined"
    combined$variable <- NA
    combined$coef <- NA
    combined$value <- NA
    combined <- combined[, c("id", "time", "name", "type", "variable", "value", "coef", "effect")]
    output <- rbind(output, combined)
  }

  return(output)
}

# Helper function for common indicator processing
process_indicators <- function(long_data, pattern, type, format, extract_variable = FALSE) {
  # Filter by pattern
  filtered <- long_data[grepl(pattern, long_data$name), ]

  if (nrow(filtered) == 0) {
    return(NULL)
  }

  # Add type
  filtered$type <- type

  # Extract variable name if needed, otherwise set to NA
  # Used for CFESIS/CSIS indicators where the variable name is part of the name
  if (extract_variable) {
    split_list <- strsplit(x = filtered$name, split = "\\.")
    filtered$variable <- unlist(lapply(split_list, `[[`, 1))
  } else {
    filtered$variable <- NA
  }

  # Reduce output to one row per indicator for list and table format
  if (format == "list" || format == "table") {
    filtered <- filtered[!duplicated(filtered[, c("id", "name")]), ]
  }

  if (format == "list" || format == "table") {
    # Don't need value column for list/table format
    return(filtered[, c("id", "time", "name", "type", "variable", "coef")])
  } else {
    # For long format, keep value column and add effect
    filtered$effect <- filtered$value * filtered$coef
    return(filtered[, c("id", "time", "name", "type", "variable", "value", "coef", "effect")])
  }
}
