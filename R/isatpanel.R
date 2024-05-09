############ Panel estimation function within R-package "gets"

########### panel isat function

#' Indicator Saturation for Panel Data
#' @description
#' This function is essentially a wrapper function around the [gets::isat()] function from the \code{gets} package.
#' This function allows the running of various different indicator saturation techniques that can, for example, be used to answer reverse causal questions.
#' Indicator Saturation techniques fully saturate a model with indicators (for example dummy-indicators or step-indicators) and then use an automated block-search
#' algorithm to retain only relevant indicators that improve the model (based on a chosen information criterion).
#'
#'
#' @param y Deprecated. The dependent variable. Can be used when data, index, and formula are not specified.
#' @param id Deprecated. Can be used when data, index, and formula are not specified. Must be a vector of the grouping variable as a character or factor
#' @param time Deprecated. Can be used when data, index, and formula are not specified. Must be a vector of the time variable as an integer or numeric.
#' @param mxreg Deprecated.The co-variates matrix. Superseded by the formula argument.
#' @param effect Fixed Effect specification. Possible arguments: "twoways" (Default), "individual", "time", or "none".
#' @param na.remove remove NAs
#' @param engine Estimation function to use. Default is NULL, which uses the default estimation procedure of the gets package. Alternatives are "fixest", "plm", or "felm".
#' @param user.estimator Use a user.estimator
#' @param cluster cluster Standard Errors at this level. Default is "none". Possible values are: "individual", "time", or "twoways".
#' @param plm_model Type of PLM model (only if engine = "PLM")
#' @param ar Autoregressive Term to be included. default is 0.
#' @param iis Logical. Use Impulse Indicator Saturation.
#' @param jiis Logical. Use Joint Impulse Indicator Saturation (Outliers are common across all units). This is essentially just a time fixed effect, but this allows selection of FE.
#' @param jsis Logical. Use Join Step Indicator Saturation (steps are common across all units). Will only be retained if time fixed effects are not included (i.e. effect = 'none' or 'individual'), as they are collinear otherwise.
#' @param fesis Logical. Use Fixed Effect Step Indicator Saturation. Constructed by multiplying a constant (1) with group Fixed Effects. Default is \code{FALSE}.
#' @param tis Logical. Use Trend Indicator Saturation. Constructed by fitting a trend for each unit from every observation. Default is \code{FALSE}.
#' @param csis Logical. Use Coefficient Step Indicator Saturation. Constructed by Default is FALSE.
#' @param cfesis Logical. Use Coefficient-Fixed Effect Indicator Saturation. Default is FALSE.
#' @param uis Matrix or List. This can be used to include a set of UIS (User Specified Indicators). Must be equal to the sample size (so it is recommended to use this only with datasets without \code{NA} values. Default is \code{NULL}. See the reference by Genaro Sucarrat (2020) below for an explanation of the UIS system.
#' @param t.pval numeric value between 0 and 1. The significance level used for the two-sided regressor significance t-tests
#' @param ... Further arguments to [gets::isat()]
#' @param data The input data.frame object.
#' @param formula Formula argument. The dependent variable will be the left-most element, separated by a ~ symbol from the remaining regressors (e.g. y ~ x + z). Note the intercept will always be removed unless the effect is "none" - this means that if any fixed effects are specified, the intercept will always be removed.
#' @param index Specify the name of the group and time column in the format c("id", "time").
#' @param csis_var The CSIS method can be conducted for all (default) variables or just a subset of them. If you want to use a subset, please specify the column names of the variable in a character vector.
#' @param fesis_id The FESIS method can be conducted for all (default) individuals/units (i.e. looking for breaks in individual countries) or just a subset of them. If you want to use a subset, specify the individuals/units for which you want to test the stability of the fixed effect in a character vector.
#' @param tis_id The TIS method can be conducted for all (default) individuals/units (i.e. looking for trends in individual countries) or just a subset of them. If you want to use a subset, specify the individuals/units for which you want to test the trend in a character vector.
#' @param tis_time The TIS method can be conducted for all (default) time periods (i.e. looking for trends at every time period) or just a subset of them. If you want to use a subset, specify the time periods as a numeric vector (for all id's the same like \code{1:10}) or as a list with an equal number of elements as there are id's e.g. \code{list(A = 1:10, B = NULL, C = 5:10)}.
#' @param fesis_time The FESIS method can be conducted for all (default) time periods (i.e. looking for Fixed Effect Step-shifts at every time period) or just a subset of them. If you want to use a subset, specify the time periods as a numeric vector (for all id's the same like \code{1:10}) or as a list with an equal number of elements as there are id's e.g. \code{list(A = 1:10, B = NULL, C = 5:10)}.
#' @param cfesis_time The CFESIS method can be conducted for all (default) time periods (i.e. looking for Coefficient Step Shifts per unit at every time period) or just a subset of them. If you want to use a subset, specify the time periods as a numeric vector (for all id's the same like \code{1:10}) or as a list with an equal number of elements as there are id's e.g. \code{list(A = 1:10, B = NULL, C = 5:10)}.
#' @param csis_time The CSIS method can be conducted for all (default) time periods (i.e. looking for Coefficient Step Shifts across all units at every time period) or just a subset of them. If you want to use a subset, specify the time periods as a numeric vector (e.g. \code{1:10}).'
#' @param cfesis_var The CFESIS method can be conducted for all variables (default) or just a subset of them. If you want to use a subset, please specify the column names of the variable in a character vector.
#' @param cfesis_id The CFESIS method can be conducted for all individuals/units (default) or just a subset of them. If you want to use a subset, please specify the individuals/units to be tested in a character vector.
#' @param plot Logical. Should the final object be plotted? Default is TRUE. The output is a combination of \code{plot()} and [plot_grid()] using the \code{cowplot} package.
#' @param print.searchinfo logical. If \code{TRUE} (default), then detailed information is printed.
#' @param lasso_opts list. Only meaningful if \code{engine = "lasso"}, arguments to be used within \code{glmnet} where appropriate. It is recommended to consult the LASSO vignette: vignette("lasso",package = "getspanel")\cr
#' Must be a list or \code{NULL}. Any of the following arguments can be omitted (i.e. be \code{NULL}) or can take only the following types:\cr
#' Arguments 'adaptive' (for Adaptive LASSO) and 'standardize' must be logical (\code{TRUE} or \code{FALSE}).\cr
#' Arguments 'nfolds', 'alpha', and 'foldid' must be numeric. Foldid must be between 0 and 1 (representing the share of observations that are removed for each id). The default for 'nfolds' is 100. 'nfolds' and 'foldid' cannot be used at the same time. When both are supplied, 'foldid' will be used.
#' Argument 's' can either be numeric, 'min' (which chooses the minimum lambda based on Cross Validation) or 'BIC' (OLS models are run for all lambda values and then chosen by minimum BIC).\cr
#' Argument 'scale_by' (this standardises LASSO selection to mean 0 and SD 1) can either be \code{NULL} or 'time' or 'id'. Default is 'id' (meaning that the sample is scaled to be mean = 0 and SD = 1 within each id).\cr
#' Example of 'lasso_opts': list(adaptive = TRUE, standardize = FALSE, nfolds = 50, s = "min")
#'
#' @return A list with class 'isatpanel'.
#' @export
#' @importFrom fastDummies dummy_cols
#'
#' @seealso [gets::isat()]
#'
#' @references Felix Pretis and Moritz Schwarz (2022). Discovering What Mattered: Answering Reverse Causal Questions by Detecting Unknown Treatment Assignment and Timing as Breaks in Panel Models. January 31, 2022. Available at SSRN: https://ssrn.com/abstract=4022745 or http://dx.doi.org/10.2139/ssrn.4022745
#' @references Genaro Sucarrat. User-Specified General-to-Specific and Indicator Saturation Methods, The R Journal (2020) 12:2, pages 388-401. Available at: https://journal.r-project.org/archive/2021/RJ-2021-024/index.html
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
#'}


isatpanel <- function(
    data = NULL,
    formula = NULL,
    index = NULL,
    effect = c("twoways"),

    na.remove = TRUE,
    engine = NULL,
    user.estimator = NULL,
    cluster = "none",

    ar = 0,
    iis = FALSE,
    #sis = FALSE,
    jiis = FALSE,
    jsis = FALSE,
    fesis = FALSE,
    tis = FALSE,
    csis = FALSE,
    cfesis = FALSE,

    fesis_id = NULL,
    fesis_time = NULL,
    tis_id = NULL,
    tis_time = NULL,
    csis_var = NULL,
    csis_time = NULL,
    cfesis_var = NULL,
    cfesis_id = NULL,
    cfesis_time = NULL,

    uis = NULL,

    t.pval = 0.001,

    plot = TRUE,
    print.searchinfo = TRUE,
    plm_model = "within",
    lasso_opts = list(adaptive = TRUE, standardize = FALSE, nfolds = 100, foldid = NULL, s = "min", scale_by = "id"),

    y = NULL,
    id = NULL,
    time = NULL,
    mxreg = NULL,
    ...
){
  requireNamespace("gets")

  # Error checks -----
  if (!effect %in% c("twoways", "individual", "time","none")) {stop("Error in Fixed Effect Specification (effect). Possible values for effect are: 'twoways', 'individual', 'time', or 'none'.")}
  if (missing(effect) & print.searchinfo){warning("New default for effect in 'isatpanel': Used to be 'individual', now 'twoways'. To quiet this message provide the argument 'effect' or select 'print.searchinfo = FALSE'.")}

  if ((effect == "both" | effect == "time") & jiis == TRUE) {stop("You cannot use time fixed effects and jiis = TRUE. These would be perfectly collinear. Either set jiis = FALSE or use effect = 'individual'.")}

  if (!is.numeric(ar)) {stop("The ar argument must be numeric.")}
  if (ar < 0) {stop("The ar argument must be greater than or equal to 0.")}
  if (!ar %% 1 == 0) {stop("The ar argument must be an integer.")} # check if the numeric value of ar is an integer - the is.integer checks the type

  # Transformations (TODO: time, country and mxbreak as grepl character vectors)
  if (is.data.frame(mxreg)) {mxreg <- as.matrix(mxreg)}

  # Formula, Index and Data arguments
  if ((!is.null(y) | !is.null(mxreg) | !is.null(time) | !is.null(id)) & (!is.null(formula) | !is.null(data))) {
    stop("Either specify your model using the data, formula, and index arguments or through y, id, time, and mxreg. Specifying both is not allowed.")
  }

  if (is.null(engine) & cluster != "none") {stop("Cluster specifications are currently only implemented for engine = 'fixest'. Either select cluster = 'none' or engine = 'fixest'.")}

  #if (sis & (effect == "twoways" | effect == "time")) {warning("The argument 'sis' cannot be specified with isatpanel when time fixed effects are used as SIS will then be collinear.")}
  if (!is.null(match.call()$sis)) {stop("The argument 'sis' cannot be specified with isatpanel. Use 'jsis' instead.")}

  # csis and cfesis
  if (csis == FALSE & (!missing(csis_var))) {stop("You cannot specify csis_var when csis = FALSE.")}
  if (cfesis == FALSE & (!missing(cfesis_var) | !missing(cfesis_id))) {stop("You cannot specify cfesis_id or cfesis_var when cfesis = FALSE.")}
  if (fesis == FALSE & (!missing(fesis_id))) {stop("You cannot specify fesis_id when fesis = FALSE.")}
  if (tis == FALSE & (!missing(tis_id))) {stop("You cannot specify tis_id when tis = FALSE.")}

  # checking that the time restrictions are not activated when the appropriate indicator saturation is not done
  if (csis == FALSE & (!missing(csis_time))) {stop("You cannot specify csis_time when csis = FALSE.")}
  if (cfesis == FALSE & (!missing(cfesis_time))) {stop("You cannot specify cfesis_time when cfesis = FALSE.")}
  if (fesis == FALSE & (!missing(fesis_time))) {stop("You cannot specify fesis_time when fesis = FALSE.")}
  if (tis == FALSE & (!missing(tis_time))) {stop("You cannot specify tis_time when tis = FALSE.")}

  if (is.null(y) & is.null(mxreg) & is.null(time) & is.null(id) & is.null(index)) {stop("When you specify the function by using a 'data' and a 'formula' argument, you must also supply an 'index' argument.")}
  if (!is.null(index) & !all(index %in% names(data))){stop("The values for 'index' not found as column names in the 'data' argument. Can only name columns that exist.")}
  # check that the index for time is not a character
  if (is.character(data[,names(data) %in% index[2],drop = TRUE])){stop("The column for 'time' is containing characters. Be sure to specify the 'index' variable in the format 'index = c('id','time')' where 'id' and 'time' must be replaced with the column names in your data.")}

  # checking lasso
  #if (!is.null(lasso_opts) & !identical(engine, "lasso")){stop("'lasso_opts' can only be supplied when 'engine' is set to 'lasso'.")}
  if (!is.null(lasso_opts)){
    if(!is.list(lasso_opts)){stop("'lasso_opts' must be a list.")}
    if(!all(names(lasso_opts) %in% c("standardize","adaptive","nfolds","foldid", "s", "alpha", "scale_by"))){stop("'lasso_opts' must be a list and can only take the elements 'adaptive', 'standardize', 'nfolds', 'alpha', 'scale_by', 'foldid'.")}
  }
  if (!(is.null(lasso_opts[["scale_by"]]) | lasso_opts[["scale_by"]]  %in% c("id","time"))) {stop("The lasso option 'scale_by' must be either NULL, 'id' or 'time'. Any other values are not allowed.")}




  if (is.null(y) & is.null(mxreg) & is.null(time) & is.null(id) & (!is.null(formula) & !is.null(data) & !is.null(index))) {
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- FALSE
    mf$na.action <- "na.pass"
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())

    mt <- attr(mf, "terms")

    attr(mt,"intercept") <- 0 # This forces no intercept!!!

    y <- model.response(mf, "numeric")

    if(all(is.na(y))){stop("All values of dependent variable are NA - check specification.")}

    x <- model.matrix(mt, mf)

    id <- factor(data[,index[1], drop = TRUE])
    time <- data[,index[2], drop = TRUE]
    mxreg <- x

    if (missing(csis_var)) {csis_var <- colnames(mxreg)}
    if (missing(cfesis_var)) {cfesis_var <- colnames(mxreg)}

  }

  # Set up Infrastructure ------
  out <- list()
  out$inputdata <- data.frame(id,time,y,mxreg)

  # check for duplicates
  if (any(duplicated(out$inputdata[,names(out$inputdata) %in% c("id","time")]))){stop("Your input data contains duplicates when considering the 'id' and 'time' column.")}

  # Remove any spaces in the id variables (e.g. United Kingdom becomes UnitedKingdom)
  # id_orig <- dplyr::tibble(id_orig = id,
  #                          id_used = gsub(" ","",id))
  id <- gsub(" ","",id)


  mxnames <- colnames(mxreg)
  if (!is.null(mxreg)) {
    mxreg <- as.matrix(mxreg)

    # if the mxreg does not have column names, they get x1, x2, etc. below
    if (is.null(mxnames)) {
      mxnames <- paste("x", seq_len(NCOL(mxreg)), sep = "")
    }
  } else {
    mxnames <- NULL
  }


  # if either nothing was provided in csis_var or cfesis_var or if those are NULL, then take all columns
  if (missing(csis_var) | is.null(csis_var)){csis_var <- colnames(mxreg)}
  if (missing(cfesis_var) | is.null(cfesis_var)){cfesis_var <- colnames(mxreg)}

  ## Some more checks -------
  if (csis & !is.vector(csis_var)) {stop("Specify csis_var as a vector of names that correspond to columns names in mxreg.")}
  if (cfesis & !is.vector(cfesis_var)) {stop("Specify cfesis_var as a vector of names that correspond to columns names in mxreg.")}

  # If the coefficient based methods are not specified for a group subset, they will be applied to all id's.
  # An example would be: test for separate coefficient estimates for post-soviet countries
  if (is.null(fesis_id)) {fesis_id <- unique(id)}
  if (is.null(cfesis_id)) {cfesis_id <- unique(id)}
  if (is.null(tis_id)) {tis_id <- unique(id)}

  if (!all(cfesis_id %in% id)) {stop("Some or all id names in 'cfesis_id' not found in the data. Please check the input under 'cfesis_id'.")}
  if (!all(fesis_id %in% id)) {stop("Some or all id names in 'fesis_id' not found in the data. Please check the input under 'fesis_id'.")}
  if (!all(cfesis_var %in% mxnames)) {stop("Some or all variable names in 'cfesis_var' not found in the data. Please check the input under 'cfesis_var' with that in the data.")}
  if (!all(csis_var %in% mxnames)) {stop("Some or all variable names in 'csis_var' not found in the data. Please check the input under 'csis_var' with that in the data.")}

  # check all the time vectors
  if(!is.null(tis_time)){check.time.subset.vectors(time.vector = tis_time, vector.name = "tis_time", id = id, time = time)}
  if(!is.null(fesis_time)){check.time.subset.vectors(time.vector = fesis_time, vector.name = "fesis_time", id = id, time = time)}
  if(!is.null(csis_time)){check.time.subset.vectors(time.vector = csis_time, vector.name = "csis_time", id = id, time = time)}
  if(!is.null(cfesis_time)){check.time.subset.vectors(time.vector = cfesis_time, vector.name = "cfesis_time", id = id, time = time)}

  ## Remove NA observations --------

  if (na.remove)  {

    if (!is.null(mxreg))
    {
      #rel.xy <- cbind(y, mx)
      rel.xy <- cbind(y, mxreg)
    } else {
      rel.xy <- y
    }

    if (any(!complete.cases(rel.xy)))  {

      remove <- which(!complete.cases(rel.xy) == TRUE)

      y <- y[-remove]
      # mx <- mx[-remove,]
      id <- id[-remove]
      time <- time[-remove]
      mxreg <- mxreg[-remove,]
    }
  }


  ############# Get sample length and id out
  Tsample  <- length(unique(time))
  N <- length(unique(id))


  df <- data.frame(id,time)

  # Autoregressive Term
  if (ar > 0) {

    if(is.null(colnames(mxreg))){
      mx_all <- mxreg
      colnames(mx_all) <- "x1"
    } else {
      mx_all <- mxreg
    }

    ar_df <- cbind(cbind(df,y),mx_all)
    ar_df_processed <- by(ar_df,INDICES = ar_df$id, function(x) {
      y = x$y
      mx <- x[, names(x) %in% colnames(mx_all)]
      gets::regressorsMean(y = y, mxreg = mx,ar = c(1:ar),return.as.zoo = FALSE)
    })

    ar_df_processed <- as.data.frame(do.call("rbind",as.list(ar_df_processed)[unique(ar_df$id)])) # the [unique(ar_df$id)] is necessary to retain the original order

    # get data back out
    y <- ar_df_processed$y
    mxreg <- ar_df_processed[,!names(ar_df_processed) %in% c("y")]
    Tsample <- Tsample - ar

    # Adjust the time and id vectors
    # The code below removes the minimum time period for each group as many times as the AR term sets out
    for (a in 1:max(ar)) {
      df <- data.frame(do.call("rbind",
                               by(df,df$id, function(x){
                                 x[!x$time == min(x$time),]
                               })[unique(ar_df$id)]), # the [unique(ar_df$id)] is necessary to retain the original order
                       row.names = NULL)
    }

    time <- df$time
    id <- df$id

    # Adjust mxnames
    mxnames <- append(paste0("ar",1:ar),mxnames)
  }

  df_base <- df

  # Break Methods ------------

  BreakList <- list()
  ## jsis = TRUE --------
  if (jsis) {
    jsis_df <- as.data.frame(cbind(time = unique(time),gets::sim(Tsample)))

    # merge with df to ensure order is correct
    current <- merge(df,jsis_df, by = "time", all.x = TRUE, sort = FALSE)
    # delete any columns that are 0 (can be included if panel not balanced)
    current <- current[, colSums(current != 0) > 0]
    # remove any duplicate columns
    drop <- union(which(duplicated(as.list(current),fromLast = TRUE)),which(duplicated(as.list(current),fromLast = FALSE)))
    if (!identical(drop,integer(0))) {
      current <- current[,-drop]
    }
    current <- current[order(current$id, current$time),]
    # Add to Breaklist (uis list)
    BreakList <- c(BreakList,list(jsis = as.matrix(current[,!names(current) %in% c("id","time")])))
    if(effect %in% c("twoways","time")){message("\nInformation: JSIS will normally not be retained when effects are 'twoways' or 'time' as they will be collinear with the Time Fixed Effects. This is not a problem though as they are dropped in the estimation procedure.\n")}
  }

  #jiis = TRUE - This is just like a time FE
  if (jiis) {
    jiis_df <- as.data.frame(cbind(time = unique(time),gets::iim(Tsample)))

    # merge with df to ensure order is correct
    current <- merge(df,jiis_df, by = "time", all.x = TRUE, sort = FALSE)
    # delete any columns that are 0 (can be included if panel not balanced)
    current <- current[, colSums(current != 0) > 0]
    # remove any duplicate columns
    drop <- union(which(duplicated(as.list(current),fromLast = TRUE)),which(duplicated(as.list(current),fromLast = FALSE)))
    if (!identical(drop,integer(0))) {
      current <- current[,-drop]
    }
    # Add to Breaklist (uis list)
    BreakList <- c(BreakList,list(jiis = as.matrix(current[,!names(current) %in% c("id","time")])))
  }

  ## fesis = TRUE ------
  if (fesis) {
    # Create a balanced data.frame
    df_balanced <- data.frame(id = rep(unique(id),each = Tsample),time = rep(unique(time),N))
    # Extract the names for the balanced data.frame
    # find the minimum time for each id
    fesis_names_mintime <- aggregate(df$time, by = list(df$id), min)
    names(fesis_names_mintime) <- c("id","mintime")
    # remove the mintime for each id
    fesis_names_merged <- merge(df_balanced, fesis_names_mintime, by = "id", sort = FALSE)
    fesis_names_intermed <- fesis_names_merged[fesis_names_merged$time != fesis_names_merged$mintime,c("id","time")]

    fesis_names <- paste0("fesis",fesis_names_intermed$id,".",fesis_names_intermed$time)

    sistlist <- do.call("list", rep(list(as.matrix(gets::sim(Tsample))), N))
    fesis_df <- as.data.frame(as.matrix(Matrix::bdiag(sistlist)))
    names(fesis_df) <- fesis_names

    fesis_df <- cbind(df_balanced,fesis_df)

    # Set all indicators to 0 for ids which are not in fesis_id
    fesis_df[!fesis_df$id %in% fesis_id,!names(fesis_df) %in% c("id","time")] <- 0

    # Set all indicators to 0 for time periods which are not in fesis_time
    if(!is.null(fesis_time)){
      if(!is.list(fesis_time)){
        fesis_df[,!names(fesis_df) %in% c("time","id",paste0("fesis",fesis_names_intermed$id,".",fesis_time))] <- 0
      } else {
        fesis_time_df <- data.frame()
        # cycle through the elements of the list and create a data.frame with the ids and times
        for(fesis_time_len in 1:length(fesis_time)){
          if(is.null(fesis_time[[fesis_time_len]])){next}
          fesis_time_df <- rbind(fesis_time_df,
                                 data.frame(id = names(fesis_time)[fesis_time_len],
                                            time = fesis_time[[fesis_time_len]]))
        }
        fesis_df[,!names(fesis_df) %in% c("time","id",paste0("fesis",fesis_time_df$id,".",fesis_time_df$time))] <- 0
      }
    }


    # merge with df to ensure order is correct
    current <- merge(df,fesis_df,by = c("id","time"),all.x = TRUE, sort = FALSE)
    # delete any columns that are 0 (can be included if panel not balanced)
    current <- current[, colSums(current != 0) > 0]
    # remove any duplicate columns
    drop <- union(which(duplicated(as.list(current),fromLast = TRUE)),which(duplicated(as.list(current),fromLast = FALSE)))
    if (!identical(drop,integer(0))) {
      current <- current[,-drop]
    }


    # Add to Breaklist (uis list)
    BreakList <- c(BreakList,list(fesis = as.matrix(current[,!names(current) %in% c("id","time")])))
  }

  ## csis = TRUE --------------
  if (csis) {
    csis_init <- cbind(time = unique(time),gets::sim(Tsample))
    names(csis_init) <- c("time",paste0("csis",unique(time)[-1]))
    csis_init <- merge(df_base,csis_init,by = "time",sort = FALSE)
    csis_df <- csis_init[,c("id","time")]

    for (i in csis_var) {
      csis_intermed <- csis_init[,!names(csis_init) %in% c("id","time")] * mxreg[,i]
      names(csis_intermed) <- paste0(i,".",names(csis_intermed))
      if(!is.null(csis_time)){
        csis_intermed[,!names(csis_intermed) %in% c("time","id",paste0(i,".csis",csis_time))] <- 0
      }
      csis_df <- cbind(csis_df,csis_intermed)
    }



    # merge with df to ensure order is correct
    current <- merge(df,csis_df,by = c("id","time"),all.x = TRUE, sort = FALSE)
    # delete any columns that are 0 (can be included if panel not balanced)
    current <- current[, colSums(current != 0) > 0]
    # remove any duplicate columns
    drop <- union(which(duplicated(as.list(current),fromLast = TRUE)),which(duplicated(as.list(current),fromLast = FALSE)))
    if (!identical(drop,integer(0))) {
      current <- current[,-drop]
    }
    # Add to Breaklist (uis list)
    BreakList <- c(BreakList,list(csis = as.matrix(current[,!names(current) %in% c("id","time")])))
  }

  ## cfesis=TRUE -------
  if (cfesis) {
    # Create a balanced data.frame
    df_balanced <- data.frame(id = rep(unique(id),each = Tsample),time = rep(unique(time),N))
    # Extract the names for the balanced data.frame
    # find the minimum time for each id
    cfesis_names_mintime <- aggregate(df$time, by = list(df$id), min)
    names(cfesis_names_mintime) <- c("id","mintime")
    # remove the mintime for each id
    cfesis_names_merged <- merge(df_balanced, cfesis_names_mintime, by = "id")
    cfesis_names_intermed <- cfesis_names_merged[cfesis_names_merged$time != cfesis_names_merged$mintime,c("id","time")]
    cfesis_names <- paste0("cfesis",cfesis_names_intermed$id,".",cfesis_names_intermed$time)

    sistlist <- do.call("list", rep(list(as.matrix(gets::sim(Tsample))), N))
    cfesis_df <- as.data.frame(as.matrix(Matrix::bdiag(sistlist)))
    names(cfesis_df) <- cfesis_names

    cfesis_df <- cbind(df_balanced,cfesis_df)
    cfesis_out <- df_balanced

    for (i in cfesis_var) {
      cfesis_intermed <- cfesis_df[,!names(cfesis_df) %in% c("id","time")]*mxreg[,i]
      names(cfesis_intermed) <- paste0(i,".",names(cfesis_intermed))

      # Set all indicators to 0 for time periods which are not in cfesis_time
      if(!is.null(cfesis_time)){
        if(!is.list(cfesis_time)){
          cfesis_intermed[,!names(cfesis_intermed) %in% c("time","id",paste0(i,".cfesis",cfesis_names_intermed$id,".",cfesis_time))] <- 0
        } else {
          cfesis_time_df <- data.frame()
          # cycle through the elements of the list and create a data.frame with the ids and times
          for(cfesis_time_len in 1:length(cfesis_time)){
            if(is.null(cfesis_time[[cfesis_time_len]])){next}
            cfesis_time_df <- rbind(cfesis_time_df,
                                    data.frame(id = names(cfesis_time)[cfesis_time_len],
                                               time = cfesis_time[[cfesis_time_len]]))
          }
          cfesis_intermed[,!names(cfesis_intermed) %in% c("time","id",paste0(i,".cfesis",cfesis_time_df$id,".",cfesis_time_df$time))] <- 0
        }

      }
      cfesis_out <- cbind(cfesis_out,cfesis_intermed)
    }

    # Set all indicators to 0 for ids which are not in cfesis_id
    cfesis_out[!cfesis_out$id %in% cfesis_id,!names(cfesis_df) %in% c("id","time")] <- 0

    # merge with df to ensure order is correct
    current <- merge(df,cfesis_out,by = c("id","time"),all.x = TRUE, sort = FALSE)
    # delete any columns that are 0 (can be included if panel not balanced)
    current <- current[, colSums(current != 0) > 0]
    # remove any duplicate columns
    drop <- union(which(duplicated(as.list(current),fromLast = TRUE)),which(duplicated(as.list(current),fromLast = FALSE)))
    if (!identical(drop,integer(0))) {
      current <- current[,-drop]
    }
    # Add to Breaklist (uis list)
    BreakList <- c(BreakList,list(cfesis = as.matrix(current[,!names(current) %in% c("id","time")])))
  }

  ## tis = TRUE ----------
  if (tis) {

    # Create a balanced data.frame
    df_balanced <- data.frame(id = rep(unique(id),each = Tsample),time = rep(unique(time),N))
    # Extract the names for the balanced data.frame
    # find the minimum time for each id
    tis_names_mintime <- aggregate(df$time, by = list(df$id), min)
    names(tis_names_mintime) <- c("id","mintime")
    # remove the mintime for each id
    tis_names_merged <- merge(df_balanced, tis_names_mintime, by = "id", sort = FALSE)
    tis_names_intermed <- tis_names_merged[tis_names_merged$time != tis_names_merged$mintime,c("id","time")]

    tis_names <- paste0("tis",tis_names_intermed$id,".",tis_names_intermed$time)

    tistlist <- do.call("list", rep(list(as.matrix(gets::tim(Tsample))), N))
    tis_df <- as.data.frame(as.matrix(Matrix::bdiag(tistlist)))
    names(tis_df) <- tis_names

    tis_df <- cbind(df_balanced,tis_df)

    # Set all indicators to 0 for ids which are not in tis_id
    tis_df[!tis_df$id %in% tis_id,!names(tis_df) %in% c("id","time")] <- 0

    # Set all indicators to 0 for time periods which are not in tis_time
    if(!is.null(tis_time)){
      if(!is.list(tis_time)){
        tis_df[,!names(tis_df) %in% c("time","id",paste0("tis",tis_names_intermed$id,".",tis_time))] <- 0
      } else {
        tis_time_df <- data.frame()
        # cycle through the elements of the list and create a data.frame with the ids and times
        for(tis_time_len in 1:length(tis_time)){
          if(is.null(tis_time[[tis_time_len]])){next}
          tis_time_df <- rbind(tis_time_df,
                               data.frame(id = names(tis_time)[tis_time_len],
                                          time = tis_time[[tis_time_len]]))
        }
        tis_df[,!names(tis_df) %in% c("time","id",paste0("tis",tis_time_df$id,".",tis_time_df$time))] <- 0
      }
    }

    # merge with df to ensure order is correct
    current <- merge(df,tis_df,by = c("id","time"),all.x = TRUE, sort = FALSE)
    # delete any columns that are 0 (can be included if panel not balanced)
    current <- current[, colSums(current != 0) > 0]
    # remove any duplicate columns
    drop <- union(which(duplicated(as.list(current),fromLast = TRUE)),which(duplicated(as.list(current),fromLast = FALSE)))
    if (!identical(drop,integer(0))) {
      current <- current[,-drop]
    }

    # Add to Breaklist (uis list)
    BreakList <- c(BreakList,list(tis = as.matrix(current[,!names(current) %in% c("id","time")])))
  }


  if (any(fesis,jsis,jiis,csis,cfesis,tis)) {
    sispanx <- BreakList
  } else {
    sispanx = FALSE
  }

  # If someone supplies the uis argument to pass a user-specified indicator list to the function
  if(!missing(uis)){
    if(identical(sispanx,FALSE)){
      sispanx <- uis
    } else {
      sispanx <- c(sispanx, uis)
    }
    uis_args <- uis
    rm(uis)
  } else {uis_args <- NULL}


  if ((is.null(engine) | identical(engine, "lasso")) && effect != "none") {

    # Generating Fixed effects ------------------------------------------------
    FE <- vector()
    if (effect %in% c("individual", "twoways")) {FE <- append(FE,"id")}
    if (effect %in% c("time", "twoways")) {FE <- append(FE,"time")}

    dummies <- dummy_cols(df,select_columns = FE,remove_first_dummy = FALSE,remove_selected_columns = FALSE)
    dummies <- dummies[,!names(dummies) %in% c("id","time")]
    names(dummies) <- gsub("_","",names(dummies))

    if (effect == "twoways") {
      mx <- cbind(mxreg,dummies[-1])
    } else if (effect == "individual"|effect == "time") {
      mx <- cbind(mxreg,dummies)
    }


  } else {
    # If an engine is selected, we don't create fixed effects
    mx <- mxreg
    colnames(mx) <- mxnames
  }


  # Set Engine --------
  if (is.null(user.estimator) && !is.null(engine)) {
    if (!engine %in% c("felm","fixest","plm", "lasso")) {
      stop("Specified engine not available. Choose either 'felm', 'lasso', or 'fixest'.")
    }
    if (engine == "felm") {
      #require(lfe, quietly = TRUE)
      user.estimator <- list(
        name = felmFun,
        time = time,
        id = id,
        effect = effect,
        cluster = cluster
      )
      mc = FALSE
    }
    if (engine == "fixest") {
      #require(fixest, quietly = TRUE)
      user.estimator <- list(
        name = fixestFun,
        time = time,
        id = id,
        effect = effect,
        cluster = cluster,
        envir = environment()
      )
      mc = FALSE
    }
    if (engine == "plm") {
      #require(plm, quietly = TRUE)
      # alternatively use requireNamespace
      user.estimator <- list(
        name = plmFun,
        time = time,
        id = id,
        effect = effect,
        cluster = cluster,
        model = plm_model
      )
      mc = FALSE
    }
  }

  if (is.null(engine)) {
    user.estimator <- NULL
  }

  # add a manual intercept if no FE selected
  if (effect == "none") {
    mx <- cbind(mconst = 1, mx)
    mxreg <- cbind(mconst = 1, mxreg)
  }

  estimateddata <- data.frame(id,time,y)
  if (is.null(engine)) {
    # if FE are manually created we only use mxreg (which is with the id and time column but not the FE)
    estimateddata <- cbind(estimateddata,mxreg)
  } else {
    # if FE are not manually created, i.e. an engine is used, we use mx
    estimateddata <- cbind(estimateddata,mx)
  }

  out$estimateddata <- estimateddata

  # Estimate using gets ------
  if(is.null(engine) | !identical(engine,"lasso")){
    # Save original arx mc warning setting and disable it here
    tmpmc <- options("mc.warning")
    on.exit(options(tmpmc)) # set the old mc warning on exit

    options(mc.warning = FALSE)
    #sis <- FALSE # don't allow sis argument - does not make sense in a panel context, only JSIS makes sense
    ispan <- gets::isat(
      y,
      mxreg = mx,
      iis = iis,
      sis = FALSE,
      uis = sispanx,
      user.estimator = user.estimator,
      mc = FALSE,
      t.pval = t.pval,
      print.searchinfo = print.searchinfo,
      ...
    )

  }


  # Estimate using LASSO ------
  if(!is.null(engine) & identical(engine,"lasso")){

    # create a dataset where all indicators are inputed as a large dataframe
    full_indic_df <- data.frame()
    for(sipanx_length in seq_along(sispanx)){
      if(nrow(full_indic_df) == 0){
        full_indic_df <- sispanx[[sipanx_length]]
      } else {
        full_indic_df <- cbind(full_indic_df,sispanx[[sipanx_length]])
      }
    }
    if(iis){
      full_indic_df <- cbind(full_indic_df, gets::iim(id))
    }

    ## LASSO options and setup ----
    nflds <- if(!is.null(lasso_opts$nfolds)){lasso_opts$nfolds} else {100}
    nflds <- if(!is.null(lasso_opts$foldid)){NULL} else {nflds}
    foldid_var <- if(!is.null(lasso_opts$foldid)){lasso_opts$foldid} else {NULL}

    stdize <- if(!is.null(lasso_opts$standardize)){lasso_opts$standardize} else {FALSE}
    adptive <- if(!is.null(lasso_opts$adaptive)){lasso_opts$adaptive} else {TRUE}
    s_variable <- if(!is.null(lasso_opts[["s"]])){lasso_opts[["s"]]} else {"min"}
    alpha_variable <- if(!is.null(lasso_opts[["alpha"]])){lasso_opts[["alpha"]]} else {1}
    scale_by <- if(!is.null(lasso_opts[["scale_by"]])){lasso_opts[["scale_by"]]}else{NULL}

    # LASSO Scaling data ----
    if(!is.null(scale_by)){
      mx_lasso <- mx
      mx_lasso_int <- mx_lasso[,mxnames]
      if(scale_by == "id"){
        # check which columns can be scaled (we cannot scale columns by id when for one id all values are 0)
        cols_to_scale_within <- !apply(X = mx_lasso_int, MARGIN = 2, function(x){
          any(as.vector(by(x, id, function(x){all(x == 0)})))
        })
        # for the identified columns, scale the relevant columns
        mx_lasso_int[,cols_to_scale_within] <- apply(mx_lasso_int[,cols_to_scale_within], MARGIN = 2, function(x){
          do.call(rbind,(by(x,id, scale)))
        })
        mx_lasso[,mxnames] <- mx_lasso_int
        # scale the y variable
        y_lass <- as.vector(do.call(rbind,by(y, id, scale)))
      } else if(scale_by == "time"){
        # check which columns can be scaled (we cannot scale columns by id when for one id all values are 0)
        cols_to_scale_within <- !apply(X = mx_lasso_int, MARGIN = 2, function(x){
          any(as.vector(by(x, time, function(x){all(x == 0)})))
        })
        # for the identified columns, scale the relevant columns
        mx_lasso_int[,cols_to_scale_within] <- apply(mx_lasso_int[,cols_to_scale_within], MARGIN = 2, function(x){
          do.call(rbind,(by(x,time, scale)))
        })
        mx_lasso[,mxnames] <- mx_lasso_int
        # scale the y variable
        y_lass <- as.vector(do.call(rbind,by(y, time, scale)))
      }
    } else {
      mx_lasso <- mx
      y_lass <- y
    }

    lasso_output <- list()
    lasso_data <- as.matrix(cbind(mx_lasso, full_indic_df))
    lasso_names <- colnames(lasso_data)


    # LASSO First Stage ----
    #foldid_var <- if(!is.null(lasso_opts$foldid)){lasso_opts$foldid} else {as.numeric(as.factor(id))}
    mod_fit_adap1 = glmnet::glmnet(
      y = y_lass,
      x = lasso_data,
      family = "gaussian",
      standardize = stdize,
      alpha = alpha_variable,
      intercept = FALSE, # unclear what to do with this
      penalty.factor = c(rep(0,ncol(mx)),
                         rep(1,ncol(full_indic_df)))
    )

    if(!is.null(foldid_var) & is.numeric(foldid_var)){
      # Assuming 'id' is a vector of identifiers and 'foldid_var' is some variable
      # Initialize empty vector to store results
      foldid_group <- numeric(length(id))

      # Convert 'id' to factor, then to numeric, and calculate id_num
      id_num <- as.numeric(factor(id)) * (1 / foldid_var)

      # Generate sequence 1:length(id)
      x <- 1:length(id)

      # Calculate foldid_group using cut function
      for (i in unique(id)) {
        # Subset x based on id
        x_subset <- x[id == i]
        # Calculate foldid_group using cut function
        foldid_group_subset <- cut(x_subset, breaks = 1 / foldid_var, labels = FALSE)
        foldid_group_subset <- foldid_group_subset + as.numeric(factor(id)[id == i]) * 1/foldid_var - 1/foldid_var

        # Assign calculated foldid_group to corresponding indices in foldid_group vector
        foldid_group[id == i] <- foldid_group_subset
      }
      foldid_group <- round(foldid_group)
    } else {foldid_group = NULL}


    mod_cv_adap1 <- glmnet::cv.glmnet(
      y = y_lass,
      x = lasso_data,
      family = "gaussian",
      standardize = stdize,
      alpha = alpha_variable,
      nfolds = nflds,
      foldid = foldid_group,
      grouped = FALSE,
      intercept = FALSE, # unclear what to do with this
      penalty.factor = c(rep(0,ncol(mx)),
                         rep(1,ncol(full_indic_df)))
    )

    lasso_output$firststage <- mod_fit_adap1
    lasso_output$firststage.cv <- mod_cv_adap1
    lasso_output$firststage.cv.lambda_min <- mod_cv_adap1$lambda.min

    #coef(mod_fit_adap1, mod_fit$lambda[1])

    if(s_variable == "min" | is.numeric(s_variable)){
      best_lass_coef <- coef(mod_cv_adap1, s = if(s_variable == "min"){mod_cv_adap1$lambda.min} else {s_variable})
    } else if(s_variable == "BIC"){
      # we can also choose our model using the BIC
      # this is done by running the OLS model for each lambda
      # and then comparing the BIC of each model, thereby choosing the best
      lambda_BIC_collection <- data.frame()
      for(lass_mod_lambda in mod_fit_adap1$lambda){
        best_lass_coef <- as.numeric(coef(mod_fit_adap1, s = lass_mod_lambda))
        ret_cv <- which(as.vector(best_lass_coef)!=0)
        int_lasso_retained <- lasso_names[ret_cv-1] # -1 for the intercept

        ### Re-estimate final model
        lasso_data_ret_int <- lasso_data[,c(colnames(lasso_data) %in% c(colnames(mx),int_lasso_retained))]

        # Save original arx mc warning setting and disable it here
        tmpmc <- options("mc.warning")
        on.exit(options(tmpmc)) # set the old mc warning on exit

        options(mc.warning = FALSE)
        suppressWarnings(int_arx_lasso <- gets::arx(y = y, mc = FALSE, mxreg = lasso_data_ret_int))
        lambda_BIC_collection <- rbind(lambda_BIC_collection, data.frame(lambda = lass_mod_lambda, BIC = BIC(int_arx_lasso)))
      }

      best_lambda <- lambda_BIC_collection[lambda_BIC_collection$BIC == min(lambda_BIC_collection$BIC),"lambda"]
      best_lambda <- min(best_lambda) # in case two models with the same BIC
      best_lass_coef <- coef(mod_fit_adap1, s = best_lambda)
    } else {
      stop("For LASSO models the parmeter 's' must be either numeric, 'min' or 'BIC'. The parameter is specified as a list in lasso_opts, e.g. as lasso_opts = list(s = 'min')")
    }

    penalty.factor = 1 / abs(as.numeric(best_lass_coef))
    names(penalty.factor) <- row.names(best_lass_coef)
    penalty.factor[colnames(mx)]  <- 0 # set all x variables to 0 (i.e. don't select over them)
    penalty.factor <- penalty.factor[!names(penalty.factor) == "(Intercept)"] # this will always exclude the intercept (that is added even though intercept = FALSE)

    # LASSO Second Stage ----
    if(adptive & !all(penalty.factor %in% c(0, Inf))) {

      mod_fit_adap2 = glmnet::glmnet(
        y = y_lass,
        x = lasso_data,
        family = "gaussian",
        standardize = stdize,
        alpha = alpha_variable,
        intercept = FALSE, # unclear what to do with this
        penalty.factor = penalty.factor)

      # coef(mod_fit_adap2, mod_fit_adap2$lambda[2])
      mod_cv_adap2 <- glmnet::cv.glmnet(
        y = y_lass,
        x = lasso_data,
        family = "gaussian",
        standardize = stdize,
        alpha = alpha_variable,
        nfolds = nflds,
        foldid = foldid_group,
        grouped = FALSE,
        intercept = FALSE, # unclear what to do with this
        penalty.factor = penalty.factor)

      lasso_output$secondstage <- mod_fit_adap2
      lasso_output$secondstage.cv <- mod_cv_adap2
      lasso_output$secondstage.cv.lambda_min <- mod_cv_adap2$lambda.min



      if(s_variable == "min" | is.numeric(s_variable)){
        rel.coefs.adap2 <- coef(mod_cv_adap2, s = if(s_variable == "min"){mod_cv_adap2$lambda.min} else {s_variable})
      } else if(s_variable == "BIC"){
        # we can also choose our model using the BIC
        # this is done by running the OLS model for each lambda
        # and then comparing the BIC of each model, thereby choosing the best
        lambda_BIC_collection <- data.frame()
        for(lass_mod_lambda in mod_fit_adap2$lambda){
          best_lass_coef <- as.numeric(coef(mod_fit_adap2, s = lass_mod_lambda))
          ret_cv <- which(as.vector(best_lass_coef)!=0)
          int_lasso_retained <- lasso_names[ret_cv-1] # -1 for the intercept

          ### Re-estimate final model
          lasso_data_ret_int <- lasso_data[,c(colnames(lasso_data) %in% c(colnames(mx),int_lasso_retained))]

          # Save original arx mc warning setting and disable it here
          tmpmc <- options("mc.warning")
          on.exit(options(tmpmc)) # set the old mc warning on exit

          options(mc.warning = FALSE)
          suppressWarnings(int_arx_lasso <- gets::arx(y = y, mc = FALSE, mxreg = lasso_data_ret_int))
          lambda_BIC_collection <- rbind(lambda_BIC_collection, data.frame(lambda = lass_mod_lambda, BIC = BIC(int_arx_lasso)))
        }
        best_lambda <- lambda_BIC_collection[lambda_BIC_collection$BIC == min(lambda_BIC_collection$BIC),"lambda"]
        best_lambda <- min(best_lambda) # in case two models with the same BIC
        rel.coefs.adap2 <- coef(mod_fit_adap2, s = best_lambda)
      } else {
        stop("For LASSO models the parmeter 's' must be either numeric, 'min' or 'BIC'. The parameter is specified as a list in lasso_opts, e.g. as lasso_opts = list(s = 'min')")
      }

      ret_cv <- which(as.vector(rel.coefs.adap2) != 0)
      final_lasso_retained <- lasso_names[ret_cv-1] # -1 for the intercept

      if(all(final_lasso_retained %in% names(mx))){
        if(print.searchinfo){
          message("LASSO did not identify any indicators. General Unrestricted Model will be estimated.")
        }
      }


    } else {
      ret_cv <- which(as.vector(best_lass_coef)!=0)
      final_lasso_retained <- lasso_names[ret_cv-1] # -1 for the intercept
      if(all(final_lasso_retained %in% names(mx))){
        if(print.searchinfo){message("LASSO did not identify any indicators. General Unrestricted Model will be estimated.")}
      }
    }

    # Re-estimate OLS Model ----
    ### Re-estimate final model - but use the unscaled data!
    lasso_data_ret <- as.matrix(cbind(mx_lasso, full_indic_df))[,c(colnames(lasso_data) %in% c(colnames(mx),final_lasso_retained))]

    # Save original arx mc warning setting and disable it here
    tmpmc <- options("mc.warning")
    on.exit(options(tmpmc)) # set the old mc warning on exit

    options(mc.warning = FALSE)
    suppressWarnings(arx_lasso_output <- gets::arx(y = y, mc = FALSE, mxreg = lasso_data_ret))

    ispan <- arx_lasso_output
    # this next line just ensures that aux$mX has the correct column names
    ispan$aux$mX <- lasso_data_ret[,colnames(lasso_data_ret) %in% arx_lasso_output$aux$mXnames]

  }


  # Return output ------------

  out$isatpanel.result <- ispan

  # Create a final data object
  indicators <- out$isatpanel.result$aux$mX
  if (is.null(engine)) {
    indicators <- indicators[,!colnames(indicators) %in% c(names(estimateddata),if (exists("dummies")) {names(dummies)}else{NULL}), drop = FALSE]
  } else {
    indicators <- indicators[,!colnames(indicators) %in% names(estimateddata), drop = FALSE]
  }

  out$indicator_matrix <- sispanx

  out$finaldata <- data.frame(estimateddata, indicators)

  out$arguments <- list()
  out$arguments$index <- index
  out$arguments$engine <- engine
  out$arguments$user.estimator <- user.estimator
  out$arguments$cluster <- cluster
  out$arguments$effect <- effect
  out$arguments$uis <- if(!is.null(uis_args)){uis_args}else{NULL}
  out$arguments$lasso_opts <- lasso_opts
  out$lasso_output <- if(exists("lasso_output")){lasso_output}
  #out$arguments$id_orig <- id_orig

  #out$arguments <- mget(names(formals()),sys.frame(sys.nframe()))
  class(out) <- "isatpanel"

  try(
    if (plot == TRUE) {
      if(identical(list(),get_indicators(out))){
        print(plot(out, zero_line = FALSE))
      } else {
        print(cowplot::plot_grid(plot(out, zero_line = FALSE),
                                 plot_grid(out),
                                 nrow = 2))
      }
    }
  )


  return(out)


} ###ispan function ends


