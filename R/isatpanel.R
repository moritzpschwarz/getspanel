############ Panel estimation function within R-package "gets"

########### panel isat function

#' Panel isat function
#'
#' @param y Deprecated. The dependent variable. Can be used when data, index, and formula are not specified.
#' @param id Deprecated. Can be used when data, index, and formula are not specified. Must be a vector of the grouping variable as a character or factor
#' @param time Deprecated. Can be used when data, index, and formula are not specified. Must be a vector of the time variable as an integer or numeric.
#' @param mxreg The co-variates matrix
#' @param effect Fixed Effect specification. Possible arguments: "twoways", "individual", "time", or "none".
#' @param na.remove remove NAs
#' @param engine Function to use
#' @param user.estimator Use a user.estimator
#' @param cluster cluster Standard Errors at this level. Default is "none". Possible values are: "indvidiual", "time", or "twoways".
#' @param plm_model Type of PLM model (only if engine = "PLM")
#' @param ar Autoregressive Term to be included. default is 0.
#' @param iis use Impulse Indicator Saturation
#' @param jiis use Joint Impulse Indicator Saturation (Outliers are common across all units). This is essentially just a time fixed effect, but this allows selection of FE.
#' @param jsis use Join Step Indicator Saturation (steps are common across all units)
#' @param fesis Fixed Effect Step Indicator Saturation. Constructed by multiplying a constant (1) with group Fixed Effects. Default is FALSE.
#' @param csis Coefficient Step Indicator Saturation. Constructed by Default is FALSE.
#' @param cfesis Coefficient-Fixed Effect Indicator Saturation. Default is FALSE.
#' @param ... Further arguments to gets::isat()
#' @param data The input data.frame object.
#' @param formula Please specify a formula argument. The dependent variable will be the left-most element, separated by a ~ symbol from the remaining regressors. Note the intercept will always be removed, if effect is not "none" - this means that if any fixed effects are specified, the intercept will always be removed.
#' @param index Specify the name of the group and time column in the format c("id", "time").
#' @param csis_var The csis method can be conducted for all variables or just a subset of them. If you want to use a subset, please specify the column names of the variable in a character vector.
#' @param cfesis_var The cfesis method can be conducted for all variables or just a subset of them. If you want to use a subset, please specify the column names of the variable in a character vector.
#' @param plot Logical. Should the final object be plotted? Default is FALSE.
#'
#' @return
#' @export
#'
#' @examples
#' data <- pandata_simulated
#' isatpanel(data = data, gdp ~ temp, index = c("country","year"),
#' effect="twoways",iis=FALSE,fesis=TRUE,t.pval=0.01,engine = "fixest",cluster = "individual")

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
  jiis = FALSE,
  jsis = FALSE,
  fesis = FALSE,
  csis = FALSE,
  cfesis = FALSE,
  csis_var = colnames(mxreg),
  fesis_id = NULL,
  cfesis_var = colnames(mxreg),
  cfesis_id = NULL,

  plot = FALSE,
  plm_model = "within",

  y=NULL,
  id=NULL,
  time=NULL,
  mxreg=NULL,
  ...
)
{

  # Error checks
  if(! effect %in% c("twoways", "individual", "time","none")){stop("Error in Fixed Effect Specification (effect). Possible values for effect are: 'twoways', 'individual', 'time', or 'none'.")}


  if((effect == "both" | effect == "time") & jiis == TRUE){stop("You cannot use time fixed effects and jiis = TRUE. These would be perfectly collinear. Either set jiis = FALSE or use effect = 'individual'.")}

  if(!is.numeric(ar)){stop("The ar argument must be numeric.")}
  if(ar<0){stop("The ar argument must be greater than or equal to 0.")}
  if(!ar%%1==0){stop("The ar argument must be an integer.")} # check if the numeric value of ar is an integer - the is.integer checks the type

  # Transformations (to do: time, country and mxbreak as grepl character vectors)
  if(is.data.frame(mxreg)){mxreg <- as.matrix(mxreg)}

  # Formula, Index and Data arguments
  if((!is.null(y) | !is.null(mxreg) | !is.null(time) | !is.null(id)) & (!is.null(formula) | !is.null(data))){
    stop("Either specify your model using the data, formula, and index arguments or through y, id, time, and mxreg. Specifying both is not allowed.")
  }

  if(is.null(engine) & cluster != "none"){stop("Cluster specifications are currently only implemented for engine = 'fixest'. Either select cluster = 'none' or engine = 'fixest'.")}


  # csis and cfesis
  if(csis == FALSE & (!missing(csis_var))){stop("You cannot specify csis_var when csis = FALSE.")}
  if(cfesis == FALSE & (!missing(cfesis_var) | !missing(cfesis_id))){stop("You cannot specify cfesis_id or cfesis_var when cfesis = FALSE.")}
  if(fesis == FALSE & (!missing(fesis_id))){stop("You cannot specify fesis_id when fesis = FALSE.")}

  if(is.null(y) & is.null(mxreg) & is.null(time) & is.null(id) & (!is.null(formula) & !is.null(data) & !is.null(index))){
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())

    mt <- attr(mf, "terms")

    attr(mt,"intercept") <- 0 # This forces no intercept!!!

    y <- model.response(mf, "numeric")

    x <- model.matrix(mt, mf)

    id <- factor(data[,index[1]])
    time <- data[,index[2]]
    mxreg <- x

    if(missing(csis_var)){csis_var <- colnames(mxreg)}
    if(missing(cfesis_var)){csis_var <- colnames(mxreg)}

  }


  # Set up Infrastructure
  out <- list()
  out$inputdata <- data.frame(id,time,y,mxreg)

  # Remove any spaces in the id variables (e.g. United Kingdom becomes UnitedKingdom)
  id <- gsub(" ","",id)


  # Some more checks
  if(csis & !is.vector(csis_var)){stop("Specify csis_var as a vector of names that correspond to columns names in mxreg.")}
  if(cfesis & !is.vector(cfesis_var)){stop("Specify cfesis_var as a vector of names that correspond to columns names in mxreg.")}

  # If the coefficient based methods are not specified for a group subset, they will be applied to all id's.
  # An example would be: test for separate coefficient estimates for post-soviet countries
  if(is.null(fesis_id)){fesis_id <- unique(id)}
  if(is.null(cfesis_id)){cfesis_id <- unique(id)}

  mxnames <- colnames(mxreg)
  if (!is.null(mxreg)){
    mxreg <- as.matrix(mxreg)

    # if the mxreg does not have column names, they get x1, x2, etc. below
    if (is.null(mxnames)) {
      mxnames <- paste("x", seq_len(NCOL(mxreg)), sep="")
    }
  } else {
    mxnames <- NULL
  }

  ############# Get sample length and id out
  Tsample  <- length(unique(time))
  N <- length(unique(id))


  df <- data.frame(id,time)

  # Autoregressive Term
  if(ar>0){
    ar_df <- cbind(cbind(df,y),mxreg)
    ar_df_processed <- by(ar_df,INDICES = ar_df$id,FUN = function(x){
      y = x$y
      gets::regressorsMean(y=y, mxreg = mxreg,ar = c(1:ar),return.as.zoo = FALSE)
    })
    ar_df_processed <- as.data.frame(do.call("rbind",as.list(ar_df_processed)))

    # get data back out
    y <- ar_df_processed$y
    mxreg <- ar_df_processed[,!names(ar_df_processed) %in% c("y")]
    Tsample <- Tsample-ar

    # Adjust the time and id vectors
    # The code below removes the minimum time period for each group as many times as the AR term sets out
    for(a in 1:max(ar)){
      df <- data.frame(do.call("rbind",by(df,df$id,FUN = function(x){x[!x$time == min(x$time),]})),row.names = NULL)
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
  if(jsis){
    jsis_df <- as.data.frame(cbind(time = unique(time),gets::sim(Tsample)))

    # merge with df to ensure order is correct
    current <- merge(df,jsis_df, by="time", all.x = TRUE,sort=FALSE)
    # delete any columns that are 0 (can be included if panel not balanced)
    current <- current[, colSums(current != 0) > 0]
    # remove any duplicate columns
    drop <- union(which(duplicated(as.list(current),fromLast = TRUE)),which(duplicated(as.list(current),fromLast = FALSE)))
    if(!identical(drop,integer(0))){
      current <- current[,-drop]
    }
    # Add to Breaklist (uis list)
    BreakList <- c(BreakList,list(jsis=as.matrix(current[,!names(current) %in% c("id","time")])))
  }

  #jiis = TRUE - This is just like a time FE
  if(jiis){
    jiis_df <- as.data.frame(cbind(time = unique(time),gets::iim(Tsample)))

    # merge with df to ensure order is correct
    current <- merge(df,jiis_df, by="time", all.x = TRUE,sort=FALSE)
    # delete any columns that are 0 (can be included if panel not balanced)
    current <- current[, colSums(current != 0) > 0]
    # remove any duplicate columns
    drop <- union(which(duplicated(as.list(current),fromLast = TRUE)),which(duplicated(as.list(current),fromLast = FALSE)))
    if(!identical(drop,integer(0))){
      current <- current[,-drop]
    }
    # Add to Breaklist (uis list)
    BreakList <- c(BreakList,list(jiis=as.matrix(current[,!names(current) %in% c("id","time")])))
  }

  # fesis = TRUE
  if(fesis){
    # Create a balanced data.frame
    df_balanced <- data.frame(id = rep(unique(id),each = Tsample),time = rep(unique(time),N))
    # Extract the names for the balanced data.frame
    fesis_names <- paste0("fesis",df_balanced$id[!df$time == min(df$time)],".",df_balanced$time[!df$time == min(df$time)])

    sistlist <- do.call("list", rep(list(as.matrix(gets::sim(Tsample))), N))
    fesis_df <- as.data.frame(as.matrix(Matrix::bdiag(sistlist)))
    names(fesis_df) <- fesis_names

    fesis_df <- cbind(df_balanced,fesis_df)

    # Set all indicators to 0 for ids which are not in fesis_id
    fesis_df[!fesis_df$id %in% fesis_id,!names(fesis_df) %in% c("id","time")] <- 0

    # merge with df to ensure order is correct
    current <- merge(df,fesis_df,by = c("id","time"),all.x=TRUE,sort=FALSE)
    # delete any columns that are 0 (can be included if panel not balanced)
    current <- current[, colSums(current != 0) > 0]
    # remove any duplicate columns
    drop <- union(which(duplicated(as.list(current),fromLast = TRUE)),which(duplicated(as.list(current),fromLast = FALSE)))
    if(!identical(drop,integer(0))){
      current <- current[,-drop]
    }


    # Add to Breaklist (uis list)
    BreakList <- c(BreakList,list(fesis=as.matrix(current[,!names(current) %in% c("id","time")])))
  }

  # csis = TRUE
  if(csis){
    csis_init <- merge(df_base,as.data.frame(cbind(time = unique(time),gets::sim(Tsample))),by="time",sort=FALSE)
    csis_df <- csis_init[,c("id","time")]
    for(i in csis_var){
      csis_intermed <- csis_init[,!names(csis_init) %in% c("id","time")] * mxreg[,i]
      names(csis_intermed) <- paste0(i,".",names(csis_intermed))

      csis_df <- cbind(csis_df,csis_intermed)
    }


    # merge with df to ensure order is correct
    current <- merge(df,csis_df,by = c("id","time"),all.x=TRUE,sort=FALSE)
    # delete any columns that are 0 (can be included if panel not balanced)
    current <- current[, colSums(current != 0) > 0]
    # remove any duplicate columns
    drop <- union(which(duplicated(as.list(current),fromLast = TRUE)),which(duplicated(as.list(current),fromLast = FALSE)))
    if(!identical(drop,integer(0))){
      current <- current[,-drop]
    }
    # Add to Breaklist (uis list)
    BreakList <- c(BreakList,list(csis=as.matrix(current[,!names(current) %in% c("id","time")])))
  }

  # cfesis=TRUE
  if(cfesis){
    # Create a balanced data.frame
    df_balanced <- data.frame(id = rep(unique(id),each = Tsample),time = rep(unique(time),N))
    # Extract the names for the balanced data.frame
    cfesis_names <- paste0("cfesis",df_balanced$id[!df$time == min(df$time)],".",df_balanced$time[!df$time == min(df$time)])

    sistlist <- do.call("list", rep(list(as.matrix(gets::sim(Tsample))), N))
    cfesis_df <- as.data.frame(as.matrix(Matrix::bdiag(sistlist)))
    names(cfesis_df) <- cfesis_names

    cfesis_df <- cbind(df_balanced,cfesis_df)
    cfesis_out <- df_balanced

    for(i in cfesis_var){
      cfesis_intermed <- cfesis_df[,!names(cfesis_df) %in% c("id","time")]*mxreg[,i]
      names(cfesis_intermed) <- paste0(i,".",names(cfesis_intermed))
      cfesis_out <- cbind(cfesis_out,cfesis_intermed)
    }

    # Set all indicators to 0 for ids which are not in cfesis_id
    cfesis_out[!cfesis_out$id %in% cfesis_id,!names(cfesis_df) %in% c("id","time")] <- 0


    # merge with df to ensure order is correct
    current <- merge(df,cfesis_out,by = c("id","time"),all.x=TRUE,sort=FALSE)
    # delete any columns that are 0 (can be included if panel not balanced)
    current <- current[, colSums(current != 0) > 0]
    # remove any duplicate columns
    drop <- union(which(duplicated(as.list(current),fromLast = TRUE)),which(duplicated(as.list(current),fromLast = FALSE)))
    if(!identical(drop,integer(0))){
      current <- current[,-drop]
    }
    # Add to Breaklist (uis list)
    BreakList <- c(BreakList,list(cfesis=as.matrix(current[,!names(current) %in% c("id","time")])))
  }


  if(any(fesis,jsis,jiis,csis,cfesis)){
    sispanx <- BreakList
  } else {
    sispanx = FALSE
  }



  if(is.null(engine) && effect != "none"){

    ####
    # Generating Fixed effects ------------------------------------------------
    ####
    FE <- vector()
    if(effect %in% c("individual", "twoways")){FE <- append(FE,"id")}
    if(effect %in% c("time", "twoways")){FE <- append(FE,"time")}

    dummies <- fastDummies::dummy_cols(df,select_columns = FE,remove_first_dummy = FALSE,remove_selected_columns = FALSE)
    dummies <- dummies[,!names(dummies)%in%c("id","time")]
    names(dummies) <- gsub("_","",names(dummies))

    if(effect == "twoways"){
      mx <- cbind(mxreg,dummies[-1])
    } else if(effect=="individual"|effect=="time"){
      mx <- cbind(mxreg,dummies)
    }


  } else {
    # If an engine is selected, we don't create fixed effects
    mx <- mxreg
    colnames(mx) <- mxnames
  }


  #################################
  ########## Remove NA observations
  #################################

  if (na.remove)  {

    if (!is.null(mxreg))
    {
      rel.xy <- cbind(y, mx)
    } else {
      rel.xy <- y
    }

    if (any(!complete.cases(rel.xy)))  {

      remove <- which(!complete.cases(rel.xy)==TRUE)

      y <- y[-remove]
      mx <- mx[-remove,]
    }
  }


  #############################
  ####### Set Engine
  ###############################
  if(is.null(user.estimator) && !is.null(engine)){
    if(!engine %in% c("felm","fixest","plm")){
      stop("Specified engine not available. Choose either 'felm' or 'fixest'.")
    }
    if(engine == "felm"){
      user.estimator <- list(
        name = getspanel:::felmFun,
        time = time,
        id = id,
        effect = effect,
        cluster = cluster
      )
      mc = FALSE
    }
    if(engine == "fixest"){
      user.estimator <- list(
        name = getspanel:::fixestFun,
        time = time,
        id = id,
        effect = effect,
        cluster = cluster,
        envir = environment()
      )
      mc = FALSE
    }
    if(engine == "plm"){
      user.estimator <- list(
        name = getspanel:::plmFun,
        time = time,
        id = id,
        effect = effect,
        cluster = cluster,
        model = plm_model
      )
      mc = FALSE
    }

  }


  if(is.null(engine)){
    user.estimator <- NULL
  }
  estimateddata <- data.frame(id,time,y)
  if(is.null(engine)){
    # if FE are manually created we only use mxreg (which is with the id and time column but not the FE)
    estimateddata <- cbind(estimateddata,mxreg)
  } else {
    # if FE are not manually created, i.e. an engine is used, we use mx
    estimateddata <- cbind(estimateddata,mx)
  }

  out$estimateddata <- estimateddata

  # add a manual intercept if no FE selected
  if(effect == "none"){mx <- cbind(mconst = 1, mx)}

  #############################
  ####### Estimate
  ###############################
  #ispan <- gets::isat(y, mxreg = mx, iis=iis, sis=FALSE, uis=sispanx, user.estimator = user.estimator, mc=TRUE, ...)
  ispan <- isat.short(y, mxreg = mx, iis=iis, sis=FALSE, uis=sispanx, user.estimator = user.estimator, mc=FALSE, ...)

  ###############################
  ############## Return output
  ############################


  out$isatpanel.result <- ispan


  # Create a final data object
  indicators <- out$isatpanel.result$aux$mX
  if(is.null(engine)){
    indicators <- indicators[,!colnames(indicators) %in% c(names(estimateddata),if(exists("dummies")){names(dummies)}else{NULL})]
  } else {
    indicators <- indicators[,!colnames(indicators) %in% names(estimateddata)]
  }


  out$finaldata <- data.frame(estimateddata, indicators)

  try(
    if(plot == TRUE){
      plot.isatpanel(ispan)
    }
  )


  class(out) <- "isatpanel"

  return(out)


} ###ispan function ends


