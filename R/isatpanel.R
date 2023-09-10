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
#' @param mxreg The co-variates matrix
#' @param effect Fixed Effect specification. Possible arguments: "twoways", "individual", "time", or "none".
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
#' @param csis Logical. Use Coefficient Step Indicator Saturation. Constructed by Default is FALSE.
#' @param cfesis Logical. Use Coefficient-Fixed Effect Indicator Saturation. Default is FALSE.
#' @param uis Matrix or List. This can be used to include a set of UIS (User Specified Indicators). Must be equal to the sample size (so it is recommended to use this only with datasets without \code{NA} values. Default is \code{NULL}. See the reference by Genaro Sucarrat (2020) below for an explanation of the UIS system.
#' @param ... Further arguments to [gets::isat()]
#' @param data The input data.frame object.
#' @param formula Please specify a formula argument. The dependent variable will be the left-most element, separated by a ~ symbol from the remaining regressors. Note the intercept will always be removed, if effect is not "none" - this means that if any fixed effects are specified, the intercept will always be removed.
#' @param index Specify the name of the group and time column in the format c("id", "time").
#' @param csis_var The csis method can be conducted for all (default) variables or just a subset of them. If you want to use a subset, please specify the column names of the variable in a character vector.
#' @param fesis_id The fesis method can be conducted for all (default) individuals/units (i.e. looking for breaks in individual countries) or just a subset of them (joint breaks in the coefficients for a few individuals). If you want to use a subset, please specify the individuals/units for which you want to test the stability in a character vector.
#' @param cfesis_var The cfesis method can be conducted for all variables (default) or just a subset of them. If you want to use a subset, please specify the column names of the variable in a character vector.
#' @param cfesis_id The cfesis method can be conducted for all individuals/units (default) or just a subset of them. If you want to use a subset, please specify the individuals/units to be tested in a character vector.
#' @param plot Logical. Should the final object be plotted? Default is TRUE. The output is a combination of \code{plot} and [plot_grid()] using the \code{cowplot} package.
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
    data=NULL,
    formula=NULL,
    index=NULL,
    effect = c("individual"),

    na.remove = TRUE,
    engine = NULL,
    user.estimator = NULL,
    cluster = "none",

    ar=0,
    iis = FALSE,
    #sis = FALSE,
    jiis = FALSE,
    jsis = FALSE,
    fesis = FALSE,
    csis = FALSE,
    cfesis = FALSE,
    csis_var = colnames(mxreg),
    fesis_id = NULL,
    cfesis_var = colnames(mxreg),
    cfesis_id = NULL,
    uis = NULL,

    plot = TRUE,
    plm_model = "within",

    y=NULL,
    id=NULL,
    time=NULL,
    mxreg=NULL,
    ...
){
  requireNamespace("gets")

  # Error checks
  if (!effect %in% c("twoways", "individual", "time","none")) {stop("Error in Fixed Effect Specification (effect). Possible values for effect are: 'twoways', 'individual', 'time', or 'none'.")}

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

  if (is.null(y) & is.null(mxreg) & is.null(time) & is.null(id) & is.null(index)) {stop("When you specify the function by using a 'data' and a 'formula' argument, you must also supply an 'index' argument.")}
  if(!is.null(index) & !all(index %in% names(data))){stop("The values for 'index' not found as column names in the 'data' argument. Can only name columns that exist.")}

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

  # Set up Infrastructure
  out <- list()
  out$inputdata <- data.frame(id,time,y,mxreg)

  # Remove any spaces in the id variables (e.g. United Kingdom becomes UnitedKingdom)
  # id_orig <- dplyr::tibble(id_orig = id,
  #                          id_used = gsub(" ","",id))
  id <- gsub(" ","",id)


  # Some more checks
  if (csis & !is.vector(csis_var)) {stop("Specify csis_var as a vector of names that correspond to columns names in mxreg.")}
  if (cfesis & !is.vector(cfesis_var)) {stop("Specify cfesis_var as a vector of names that correspond to columns names in mxreg.")}

  # If the coefficient based methods are not specified for a group subset, they will be applied to all id's.
  # An example would be: test for separate coefficient estimates for post-soviet countries
  if (is.null(fesis_id)) {fesis_id <- unique(id)}
  if (is.null(cfesis_id)) {cfesis_id <- unique(id)}

  if (!all(cfesis_id %in% id)) {stop("Some or all id names in 'cfesis_id' not found in the data. Please check the input under 'cfesis_id'.")}
  if (!all(fesis_id %in% id)) {stop("Some or all id names in 'fesis_id' not found in the data. Please check the input under 'fesis_id'.")}

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

  #################################
  ########## Remove NA observations
  #################################

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
    ar_df_processed <- by(ar_df,INDICES = ar_df$id, FUN = function(x) {
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
                               by(df,df$id,FUN = function(x){
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

  # Break Methods -----------------------------------------------------------

  BreakList <- list()
  # jsis = TRUE
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

  # fesis = TRUE
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

  # csis = TRUE
  if (csis) {
    csis_init <- cbind(time = unique(time),gets::sim(Tsample))
    names(csis_init) <- c("time",paste0("csis",unique(time)[-1]))
    csis_init <- merge(df_base,csis_init,by = "time",sort = FALSE)
    csis_df <- csis_init[,c("id","time")]

    for (i in csis_var) {
      csis_intermed <- csis_init[,!names(csis_init) %in% c("id","time")] * mxreg[,i]
      names(csis_intermed) <- paste0(i,".",names(csis_intermed))

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

  # cfesis=TRUE
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

  if (any(fesis,jsis,jiis,csis,cfesis)) {
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


  if (is.null(engine) && effect != "none") {

    ####
    # Generating Fixed effects ------------------------------------------------
    ####
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


  #############################
  ####### Set Engine
  ###############################
  if (is.null(user.estimator) && !is.null(engine)) {
    if (!engine %in% c("felm","fixest","plm")) {
      stop("Specified engine not available. Choose either 'felm' or 'fixest'.")
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
  estimateddata <- data.frame(id,time,y)
  if (is.null(engine)) {
    # if FE are manually created we only use mxreg (which is with the id and time column but not the FE)
    estimateddata <- cbind(estimateddata,mxreg)
  } else {
    # if FE are not manually created, i.e. an engine is used, we use mx
    estimateddata <- cbind(estimateddata,mx)
  }

  out$estimateddata <- estimateddata

  # add a manual intercept if no FE selected
  if (effect == "none") {mx <- cbind(mconst = 1, mx)}

  #############################
  ####### Estimate
  ###############################
  # Save original arx mc warning setting and disable it here
  tmpmc <- options("mc.warning")
  on.exit(options(tmpmc)) # set the old mc warning on exit

  options(mc.warning = FALSE)
  #sis <- FALSE # don't allow sis argument - does not make sense in a panel context, only JSIS makes sense
  ispan <- gets::isat(y, mxreg = mx, iis = iis, sis = FALSE, uis = sispanx, user.estimator = user.estimator, mc = FALSE, ...)
  #ispan <- isat.short(y, mxreg = mx, iis=iis, sis=FALSE, uis=sispanx, user.estimator = user.estimator, mc=FALSE, ...)

  ###############################
  ############## Return output
  ############################


  out$isatpanel.result <- ispan


  # Create a final data object
  indicators <- out$isatpanel.result$aux$mX
  if (is.null(engine)) {
    indicators <- indicators[,!colnames(indicators) %in% c(names(estimateddata),if (exists("dummies")) {names(dummies)}else{NULL})]
  } else {
    indicators <- indicators[,!colnames(indicators) %in% names(estimateddata)]
  }


  out$finaldata <- data.frame(estimateddata, indicators)

  out$arguments <- list()
  out$arguments$index <- index
  out$arguments$engine <- engine
  out$arguments$user.estimator <- user.estimator
  out$arguments$cluster <- cluster
  out$arguments$effect <- effect
  out$arguments$uis <- if(!is.null(uis_args)){uis_args}else{NULL}
  #out$arguments$id_orig <- id_orig

  #out$arguments <- mget(names(formals()),sys.frame(sys.nframe()))
  class(out) <- "isatpanel"

  if (plot == TRUE) {
    if(identical(list(),get_indicators(out))){
      print(plot(out, zero_line = FALSE))
    } else {
      print(cowplot::plot_grid(plot(out, zero_line = FALSE),
                               plot_grid(out),
                               nrow = 2))
    }
  }


  return(out)


} ###ispan function ends


