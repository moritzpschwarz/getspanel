#' Extract the retained indicators from an `isatpanel` object
#'
#' This function extracts and processes indicator information from an isatpanel object,
#' returning the results in different formats suitable for analysis and plotting.
#'
#' @param object An object of class "isatpanel" produced by the `isatpanel()` function.
#' @param uis_breaks A character vector with the names of user-specified indicators.
#'   If NULL (default), user-specified indicators are not included in the result.
#' @param format A character string indicating the format of the output. Must be one of:
#'   \itemize{
#'     \item "list" (default): Returns a list of data frames for backward compatibility
#'     \item "table": Returns a single data frame with all indicators
#'     \item "long": Returns a panel-shaped data frame suitable for plotting
#'   }
#' @param regex_exclude_indicators A regular expression to filter out indicators from the plot. Combine multiple expressions with \code{"|"}. Default is \code{NULL}, meaning no indicators are excluded.
#'
#' @return Depending on the `format` parameter:
#'   \itemize{
#'     \item If `format = "list"`: A list of data frames with elements named "impulses", "fesis", "tis", "cfesis", "csis", and optionally "uis_breaks"
#'     \item If `format = "table"`: A single data frame with all indicators and one row per indicator that can be filtered by the "type" column
#'     \item If `format = "long"`: A panel-shaped data frame with all indicators plus a "COMBINED" type that sums all effects per id/time
#'   }
#'
#' @details
#' The resulting data frame(s) contain the following columns:
#' \itemize{
#'   \item id: The panel identifier. For compatibility with `plot.isatpanel`, CSIS indicators do not have an id column in the list format.
#'   \item time: The panel time period
#'   \item name: The name of the indicator. For compatibility with `plot.isatpanel`, CFESIS and CSIS indicators are renamed to their variable name in the list format.
#'   \item type: The type of the indicator (e.g., "IIS", "FESIS", "TIS", "CFESIS", "CSIS", "UIS")
#'   \item coef: The coefficient of the indicator
#'   \item variable: The corresponding variable name for CFESIS/CSIS indicators
#'   \item value: The value of the indicator (1 for IIS/FESIS, >=1 for TIS, value of the variable for CFESIS/CSIS). Only present in the "long" format.
#'   \item effect: The effect of the indicator (value * coef). Only present in the "long" format.
#' }
#' Dimensions of the output depend on the `format` parameter:
#' \itemize{
#'   \item If `format = "list"`: Returns a list with data frames for each indicator type with one row per indicator.
#'   \item If `format = "table"`: Returns a single data frame with one row per indicator/id combination (resulting in multiple rows for CSIS each indicator).
#'   \item If `format = "long"`: Returns a panel-shaped data frame with one row per id/time/indicator combination where an indicator is active.
#' }
#' The "COMBINED" type in the long format sums all effects of IIS, TIS, and FESIS indicators for each id/time combination, providing a comprehensive view of the overall impact of these indicators on the model.
#'
#' regex_exclude_indicators allows filtering multiple indicators by matching their type (e.g., \code{regex_exclude_indicators = "^iis|^tis"}) or parts of indicator names (e.g., \code{regex_exclude_indicators = "iis1|Austria|2008"}).
#' Use \code{get_indicators(object, format = "table")} to see the names of all indicators.
#'
#' @seealso \code{\link{isatpanel}}, \code{\link{plot_indicators}}, \code{\link{plot_comp}}, \code{\link{plot.isatpanel}}
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
get_indicators <- function(object, uis_breaks = NULL, format = "list", regex_exclude_indicators = NULL) {
  # Input validation ----------------------------------------------------------
  if (!inherits(object, "isatpanel")) {
    stop("object must be an isatpanel object")
  }

  if (!format %in% c("list", "table", "long")) {
    stop("format must be one of 'list', 'table', or 'long'")
  }

  if (!is.null(uis_breaks) && !is.character(uis_breaks)) {
    stop("uis_breaks must be a character vector")
  }

  # Prepare the data ----------------------------------------------------------
  # Get the indicator matrix from the isatpanel object
  df <- object$isatpanel.result$aux$mX

  # Add the panel rows
  panel_rows <- object$finaldata[, c("id", "time"), drop = FALSE]
  df <- cbind(panel_rows, df)

  # Get indicator names from the isatpanel object
  indicator_names <- object$isatpanel.result$ISnames
  if (!is.null(regex_exclude_indicators)) {
    indicator_names <- indicator_names[!grepl(regex_exclude_indicators, indicator_names)]
  }

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
  output <- if (format == "list") list() else data.frame()

  # Process indicators ---------------------------------------------------------
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
    cfesis_result[, "name"] <- cfesis_result[, "variable"]
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
    csis_result[, "name"] <- csis_result[, "variable"]
    csis_result <- csis_result[, c("time", "name", "type", "coef")]
    output$csis <- csis_result
  } else {
    output <- rbind(output, csis_result)
  }

  # Handle UIS (User-specified Indicators) if present
  if (!is.null(uis_breaks)) {
    uis_result <- process_indicators(
      all_indicators_long[all_indicators_long$name %in% uis_breaks, ],
      ".*",
      "UIS",
      format
    )
    if (format == "list") {
      output$uis_breaks <- uis_result
    } else {
      output <- rbind(output, uis_result)
    }
  }

  if (format == "long") {
    output <- add_combined_effect(output, panel_rows)
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

  if (format %in% c("list", "table")) {
    # Reduce output to one row per indicator for list/table format
    filtered <- filtered[!duplicated(filtered[, c("id", "name")]), ]
    # Don't need value column for list/table format
    return(filtered[, c("id", "time", "name", "type", "variable", "coef")])
  } else {
    # Keep value column and add effect for long format
    filtered$effect <- filtered$value * filtered$coef
    return(filtered[, c("id", "time", "name", "type", "variable", "value", "coef", "effect")])
  }
}

# Helper function to add combined effect for long format
add_combined_effect <- function(output, panel_rows) {
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
  full_grid <- expand.grid(id = all_ids, time = all_times, stringsAsFactors = FALSE)
  combined <- merge(full_grid, combined, by = c("id", "time"), all.x = TRUE)
  combined$effect[is.na(combined$effect)] <- NA
  combined$type <- "COMBINED"
  combined$name <- "combined"
  combined$variable <- NA
  combined$coef <- NA
  combined$value <- NA
  combined <- combined[, c("id", "time", "name", "type", "variable", "value", "coef", "effect")]

  return(rbind(output, combined))
}