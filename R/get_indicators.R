#' Extract the retained indicators from an `isatpanel` object
#'
#' @param object An object produced by the isatpanel function.
#' @param uis_breaks A string with the names of user-specified indicators.
#' @param list Logical. If TRUE (default), returns a list with each indicator type as a separate element. If FALSE, returns a single flat data frame.
#'
#' @return A list of indicators (if list=TRUE) or a single data frame (if list=FALSE).
#' @export
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
#'
#' # print the retained indicators
#' get_indicators(result)
#' }

# Helper function for common indicator processing
process_indicators <- function(long_data, pattern, type, deduplicate = TRUE, extract_variable = FALSE) {
  # Filter by pattern
  filtered <- long_data[grepl(pattern, long_data$name), ]
  
  if (nrow(filtered) == 0) {
    return(NULL)
  }
  
  # Filter for value == 1 (except for some special cases)
  if (!(type %in% c("TIS", "CFESIS", "CSIS"))) {
    filtered <- filtered[filtered$value == 1, ]
  }
  
  # Extract variable name if needed
  if (extract_variable) {
    split_list <- strsplit(x = filtered$name, split = "\\.")
    filtered$variable <- unlist(lapply(split_list, `[[`, 1))
  } else {
    filtered$variable <- NA
  }
  
  # Remove duplicates if requested
  if (deduplicate) {
    filtered <- filtered[!duplicated(filtered[, c("id", "name")]), ]
  }
  
  # Add type
  filtered$type <- type
  
  return(filtered[,c("id", "time", "name", "type", "variable", "value")])
}



get_indicators <- function(object, uis_breaks = NULL, format = "list"){
  print("Using the new function")
  df <- object$isatpanel.result$aux$mX
  panel_rows <- object$finaldata[, c("id", "time"), drop = FALSE]
  df <- cbind(panel_rows, df)
  indicator_names <- object$isatpanel.result$ISnames

  # Single transformation to long format for all indicators
  all_indicators_long <- reshape(
    df, 
    varying = indicator_names,
    idvar = c("id", "time"), 
    v.names = "value", 
    timevar = "name", 
    times = indicator_names, 
    direction = "long"
  )
  print(dim(all_indicators_long))
  
  # Filter for non-zero values only once
  all_indicators_long <- all_indicators_long[all_indicators_long$value != 0, ]
  print(dim(all_indicators_long))
  
  # Initialize output based on format
  if (format == "list") {
    output <- list()
  } else {
    output <- data.frame(matrix(nrow = 0, ncol = 6))
    colnames(output) <- c("id", "time", "name", "type", "variable", "value")
  }

  # IIS - Impulse Indicators
  iis_result <- process_indicators(all_indicators_long, "^iis[0-9]+", "IIS", deduplicate = FALSE)
  if (!is.null(iis_result)) {
    if (format == "list") {
      output$impulses <- iis_result
    } else {
      output <- rbind(output, iis_result)
    }
  }

  # FESIS - Fixed Effects Step Indicators
  if (format == "long") {
    fesis_result <- process_indicators(all_indicators_long, "^fesis.+\\.[0-9]+$", "FESIS", deduplicate = FALSE)
    output <- rbind(output, fesis_result)
  } else {
    fesis_result <- process_indicators(all_indicators_long, "^fesis.+\\.[0-9]+$", "FESIS", deduplicate = TRUE)
    if (format == "list") {
      output$fesis <- fesis_result
    } else {
      output <- rbind(output, fesis_result)
    }
  }

  # TIS - Trend Indicators
  if (format == "long") {
    tis_result <- process_indicators(all_indicators_long, "^tis.+\\.[0-9]+$", "TIS", deduplicate = FALSE)
    output <- rbind(output, tis_result)
  } else {
    tis_result <- process_indicators(all_indicators_long, "^tis.+\\.[0-9]+$", "TIS", deduplicate = TRUE)
    if (format == "list") {
      output$tis <- tis_result
    } else {
      output <- rbind(output, tis_result)
    }
  }

  # CFESIS - Conditional Fixed Effects Step Indicators
  if (format == "long") {
    cfesis_result <- process_indicators(all_indicators_long, "^.+\\.cfesis.+\\.[0-9]+$", "CFESIS", deduplicate = FALSE, extract_variable = TRUE)
    output <- rbind(output, cfesis_result)
  } else {
    cfesis_result <- process_indicators(all_indicators_long, "^.+\\.cfesis.+\\.[0-9]+$", "CFESIS", deduplicate = TRUE, extract_variable = TRUE)
    if (format == "list") {
      rownames(cfesis_result) <- cfesis_result[,"name"]
      cfesis_result[,c("name")] <- cfesis_result[,c("variable")]
      cfesis_result <- cfesis_result[,c("id", "time", "name")]
      output$cfesis <- cfesis_result
        } else {
      output <- rbind(output, cfesis_result)
    }
  }

  # CSIS - Common Step Indicators
  if (format == "long"){
    csis_result <- process_indicators(all_indicators_long, "^.+\\.csis[0-9]+$", "CSIS", deduplicate = FALSE, extract_variable = TRUE)
    output <- rbind(output, csis_result)
  } else {
    csis_result <- process_indicators(all_indicators_long, "^.+\\.csis[0-9]+$", "CSIS", deduplicate = TRUE, extract_variable = TRUE)
    if (format == "list") {
      # Some adjustments to be compatible with plot.isatpanel
      csis_result <- csis_result[!duplicated(csis_result[, "name"]), ]
      rownames(csis_result) <- csis_result[,"name"]
      csis_result[,c("name")] <- csis_result[,c("variable")]
      csis_result <- csis_result[,c("time", "name")]
      output$csis <- csis_result
    } else {
      output <- rbind(output, csis_result)
    }
  }

  # Handle UIS (User-specified Indicators) if present
  if(!is.null(object$arguments$uis)){
    uis_breaks <- colnames(object$arguments$uis)
    uis_result <- process_indicators(all_indicators_long[all_indicators_long$name %in% uis_breaks, ], ".*", "UIS", deduplicate = FALSE)
    if (!is.null(uis_result)) {
      if (list) {
        output$uis_breaks <- uis_result
      } else {
        output <- rbind(output, uis_result)
      }
    }
  }

  return(output)
}

# TODO:
# [x] change cfesis when list is used
# [ ] add coefficients
# [ ] combined mit rübernehmen und bei plot_comp ausprobieren
# [ ] überlegen ob fixed effects (und alle anderen) auch hier sinn ergeben

