format_estimates <- function (est, estimate = "estimate", statistic = "std.error",
                              vcov = NULL, conf_level = 0.95, fmt = "%.3f", stars = FALSE,
                              group_name = NULL, ...)
{
  estimate_glue <- ifelse(estimate == "conf.int", "[{conf.low}, {conf.high}]",
                          estimate)
  statistic_glue <- ifelse(statistic == "conf.int", "[{conf.low}, {conf.high}]",
                           statistic)
  estimate_glue <- ifelse(grepl("\\{", estimate_glue), estimate_glue,
                          sprintf("{%s}", estimate_glue))
  if (!is.null(vcov) && is.character(vcov) && length(vcov) >
      1) {
    statistic_glue <- ifelse(grepl("\\{", statistic_glue),
                             statistic_glue, sprintf("{%s}", statistic_glue))
  }
  else {
    statistic_glue <- ifelse(grepl("\\{", statistic_glue),
                             statistic_glue, sprintf("({%s})", statistic_glue))
  }
  estimate_glue <- c(estimate_glue, statistic_glue)
  is_star <- !isFALSE(stars) || isTRUE(any(grepl("\\{stars\\}",
                                                 estimate_glue)))
  is_glue <- grepl("\\{", estimate)
  if (is_star) {
    if (isFALSE(stars)) {
      stars <- TRUE
    }
    if (!"p.value" %in% colnames(est)) {
      stop("To use the `stars` argument, the `tidy` function must produce a column called \"p.value\"")
    }
    est$stars <- make_stars(est$p.value, stars)
  }
  if (isTRUE(is_star) && isFALSE(is_glue)) {
    estimate_glue[1] <- paste0(estimate_glue[1], "{stars}")
  }
  estimate_glue_strip <- regmatches(estimate_glue, gregexpr("\\{[^\\}]*\\}",
                                                            estimate_glue))
  estimate_glue_strip <- sort(unique(unlist(estimate_glue_strip)))
  estimate_glue_strip <- gsub("\\{|\\}", "", estimate_glue_strip)
  estimate_glue_strip <- setdiff(estimate_glue_strip, colnames(est))
  if (length(estimate_glue_strip) > 0) {
    stop(sprintf("These estimates or statistics do not seem to be available: %s. You can use the `get_estimates` function to see which statistics are available.",
                 paste(estimate_glue_strip, collapse = ", ")))
  }
  for (n in colnames(est)) {
    est[[n]] <- modelsummary:::rounding(est[[n]], fmt)
  }
  for (i in seq_along(estimate_glue)) {
    s <- estimate_glue[i]
    est[[paste0("modelsummary_tmp", i)]] <- as.character(glue::glue_data(est,
                                                                         s))
    est[[paste0("modelsummary_tmp", i)]][est[[s]] == ""] <- ""
  }
  if (!is.null(group_name) && group_name %in% colnames(est)) {
    est[["group"]] <- est[[group_name]]
  }
  else if (!is.null(group_name)) {
    est[["group"]] <- ""
    warning(sprintf("Group name \"%s\" was not found in the extracted data. The \"group\" argument must be a column name in the data.frame produced by `get_estimates(model)`.  If you wish to combine models with and without grouped estimates, you will find examples on the modelsummary website:\nhttps://vincentarelbundock.github.io/modelsummary",
                    group_name))
  }
  else {
    est[["group"]] <- ""
  }
  cols <- c("group", "term", paste0("modelsummary_tmp", seq_along(estimate_glue)))
  cols <- intersect(cols, colnames(est))
  est <- est[, cols, drop = FALSE]
  est$term <- factor(est$term, unique(est$term))
  browser()
  est <- stats::reshape(data.frame(est), varying = grep("modelsummary_tmp\\d+$",
                                                        colnames(est), value = TRUE),
                        times = grep("modelsummary_tmp\\d+$",colnames(est), value = TRUE),
                        v.names = "value", timevar = "statistic",
                        direction = "long")
  est$id <- NULL
  est <- est[order(est$term, est$statistic), ]
  est$term <- as.character(est$term)
  est <- est[!est$value %in% c("", "()", "(NA)"), ]
  return(est)
}


modelsummary <- function (models, output = "default", fmt = 3, estimate = "estimate",
          statistic = "std.error", vcov = NULL, conf_level = 0.95,
          stars = FALSE, coef_map = NULL, coef_omit = NULL, coef_rename = NULL,
          gof_map = NULL, gof_omit = NULL, group = term ~ model, group_map = NULL,
          add_rows = NULL, align = NULL, notes = NULL, title = NULL,
          ...)
{
  modelsummary:::sanity_ellipsis(vcov, ...)
  models <- modelsummary:::sanitize_models(models)
  vcov <- modelsummary:::sanitize_vcov(vcov, length(models), ...)
  number_of_models <- max(length(models), length(vcov))
  estimate <- modelsummary:::sanitize_estimate(estimate, number_of_models)
  group <- modelsummary:::sanitize_group(group)
  modelsummary:::sanity_group_map(group_map)
  modelsummary:::sanity_statistic(statistic)
  modelsummary:::sanity_conf_level(conf_level)
  modelsummary:::sanity_coef(coef_map, coef_rename, coef_omit)
  modelsummary:::sanity_gof_map(gof_map, gof_omit)
  modelsummary:::sanity_stars(stars)
  modelsummary:::sanity_fmt(fmt)
  modelsummary:::sanity_output(output)
  output_format <- modelsummary:::sanitize_output(output)$output_format
  if (!any(grepl("conf", c(estimate, statistic)))) {
    conf_level <- NULL
  }
  if (is.null(names(models))) {
    model_names <- paste("Model", 1:number_of_models)
  }
  else {
    model_names <- names(models)
  }
  msl <- modelsummary:::get_list_of_modelsummary_lists(models = models, conf_level = conf_level,
                                        vcov = vcov, ...)
  names(msl) <- model_names
  if (output_format == "modelsummary_list") {
    if (length(msl) == 1) {
      return(msl[[1]])
    }
    else {
      return(msl)
    }
  }
  est <- list()
  for (i in seq_along(msl)) {
    tmp <- format_estimates(est = msl[[i]]$tidy, fmt = fmt,
                            estimate = estimate[[i]], statistic = statistic,
                            vcov = vcov[[i]], conf_level = conf_level, stars = stars,
                            group_name = group$group_name, ...)
    tmp <- modelsummary:::map_omit_rename_estimates(tmp, coef_rename = coef_rename,
                                     coef_map = coef_map, coef_omit = coef_omit, group_map = group_map)
    colnames(tmp)[4] <- model_names[i]
    est[[model_names[i]]] <- tmp
  }
  term_order <- unique(unlist(lapply(est, function(x) x$term)))
  group_order <- unique(unlist(lapply(est, function(x) x$group)))
  f <- function(x, y) merge(x, y, all = TRUE, sort = FALSE,
                            by = c("group", "term", "statistic"))
  est <- Reduce(f, est)
  est <- group_reshape(est, group$lhs, group$rhs, group$group_name)
  est$part <- "estimates"
  est <- est[, unique(c("part", names(est)))]
  est[is.na(est)] <- ""
  if ("term" %in% colnames(est)) {
    if (!is.null(coef_map)) {
      term_order <- coef_map
      est$term <- factor(est$term, unique(term_order))
    }
    else {
      est$term <- factor(est$term, unique(term_order))
    }
    if (!is.null(group_map)) {
      group_order <- group_map
      est$group <- factor(est$term, group_order)
    }
    else {
      est$group <- factor(est$group, unique(est$group))
    }
  }
  else if ("model" %in% colnames(est)) {
    est$model <- factor(est$model, model_names)
  }
  est <- est[do.call(order, as.list(est)), ]
  for (col in c("term", "group", "model")) {
    if (col %in% colnames(est)) {
      est[[col]] <- as.character(est[[col]])
    }
  }
  if ("term" %in% colnames(est)) {
    idx <- paste(as.character(est$term), est$statistic)
    if (is.null(group) && anyDuplicated(idx) > 0) {
      warning("The table includes duplicate term names. This can sometimes happen when a model produces \"grouped\" terms, such as in a multinomial logit or a gamlss model. Consider using the the `group` argument.")
    }
  }
  gof <- list()
  for (i in seq_along(msl)) {
    gof[[i]] <- format_gof(msl[[i]]$glance, fmt = fmt, gof_map = gof_map,
                           ...)
    colnames(gof[[i]])[2] <- model_names[i]
  }
  f <- function(x, y) merge(x, y, all = TRUE, sort = FALSE,
                            by = "term")
  gof <- Reduce(f, gof)
  gof <- map_omit_gof(gof, gof_omit, gof_map)
  if (nrow(gof) > 0 && all(colnames(gof) %in% colnames(est))) {
    tab <- bind_rows(est, gof)
  }
  else {
    tab <- est
  }
  tab[is.na(tab)] <- ""
  if (is.null(coef_map) && "term" %in% colnames(tab) && output_format !=
      "rtf") {
    idx <- tab$part != "gof"
    tab$term <- ifelse(idx, gsub(":", "ERROR", tab$term),tab$term)
  }
  hrule <- match("gof", tab$part)
  if (!is.na(hrule) && !is.null(add_rows) && !is.null(attr(add_rows,
                                                           "position"))) {
    hrule <- hrule + sum(attr(add_rows, "position") < hrule)
  }
  if (is.na(hrule)) {
    hrule <- NULL
  }
  if (!isFALSE(stars) && !any(grepl("\\{stars\\}", c(estimate,
                                                     statistic)))) {
    stars_note <- make_stars_note(stars)
    if (is.null(notes)) {
      notes <- stars_note
    }
    else {
      notes <- c(stars_note, notes)
    }
  }
  if (output_format != "dataframe") {
    tab <- redundant_labels(tab, "model")
    tab <- redundant_labels(tab, "group")
    tab <- redundant_labels(tab, "term")
    tab$statistic <- tab$part <- NULL
    if ("term" %in% colnames(tab))
      colnames(tab)[colnames(tab) == "term"] <- "       "
    if ("model" %in% colnames(tab))
      colnames(tab)[colnames(tab) == "model"] <- "         "
  }
  tmp <- setdiff(group$lhs, c("model", "term"))
  if (length(tmp) == 0) {
    tab$group <- NULL
  }
  else if (output_format != "dataframe") {
    colnames(tab)[colnames(tab) == "group"] <- "        "
  }
  if (is.null(align)) {
    if (!is.null(group) && length(group$lhs) == 2) {
      align <- paste0("ll", strrep("c", ncol(tab) - 2))
    }
    else {
      align <- paste0("l", strrep("c", ncol(tab) - 1))
    }
  }
  factory(tab, align = align, fmt = fmt, hrule = hrule, notes = notes,
          output = output, title = title, add_rows = add_rows,
          ...)
}
