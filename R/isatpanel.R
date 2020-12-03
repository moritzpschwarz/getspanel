############ Panel estimation function within R-package "gets"

########### panel isat function

#' Panel isat function
#'
#' @param y Year
#' @param id Individual
#' @param time Time
#' @param mxreg The co-variates matrix
#' @param effect Fixed Effect specification
#' @param na.remove remove NAs
#' @param engine Function to use
#' @param user.estimator Use a user.estimator
#' @param cluster cluster Standard Errors at this level
#' @param plm_model Type of PLM model (only if engine = "PLM")
#' @param ar Autoregressive Term to be included. default is 0.
#' @param iis use Impulse Indicator Saturation
#' @param jiis use Joint Impulse Indicator Saturation (Outliers are common across all units). This is essentially just a time fixed effect, but this allows selection of FE.
#' @param jsis use Join Step Indicator Saturation (steps are common across all units)
#' @param fesis Fixed Effect Step Indicator Saturation. Constructed by multiplying a constant (1) with group Fixed Effects. Default is FALSE.
#' @param csis Coefficient Step Indicator Saturation. Constructed by Default is FALSE.
#' @param cfesis Coefficient-Fixed Effect Indicator Saturation. Default is FALSE.
#' @param ... Further arguments to gets::isat
#'
#' @return
#' @export
#'
#' @examples isatpanel(y)

isatpanel <- function(
  y,
  id,
  time,
  mxreg,
  #mxbreak,
  #break.method = c("individual"),
  effect = c("individual"),

  na.remove = TRUE,
  engine = NULL,
  user.estimator = NULL,
  cluster = "individual",
  plm_model=NULL,

  iis = FALSE,
  jiis = FALSE,
  jsis = FALSE,
  fesis = FALSE,
  csis = FALSE,
  cfesis = FALSE,
  csis_var = colnames(mxreg),
  cfesis_var = colnames(mxreg),

  ar=0,
  ...
)
{
  # Error checks
  if(!is.vector(csis_var)){stop("Specify csis_var as a vector of names that correspond to columns names in mxreg.")}
  if(!is.vector(cfesis_var)){stop("Specify cfesis_var as a vector of names that correspond to columns names in mxreg.")}

  if(effect == "both" | effect == "time" & jiis == TRUE){stop("You cannot use time fixed effects and jiis = TRUE. These would be perfectly collinear. Either set jiis = FALSE or use effect = 'individual'.")}

  if(!is.numeric(ar)){stop("The ar argument must be numeric.")}
  if(ar<0){stop("The ar argument must be greater than or equal to 0.")}
  if(!ar%%1==0){stop("The ar argument must be an integer.")} # check if the numeric value of ar is an integer - the is.integer checks the type

  # Transformations (to do: time, country and mxbreak as grepl character vectors)
  if(is.data.frame(mxreg)){mxreg <- as.matrix(mxreg)}

  # Set up Infrastructure
  out <- list()
  out$inputdata <- data.frame(id,time,y,mxreg)

  # Remove any spaces in the id variables (e.g. United Kingdom becomes UnitedKingdom)
  id <- gsub(" ","",id)


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
    ar_df <- group_by(ar_df,id)

    ar_df_processed <- by(ar_df,INDICES = ar_df$id,FUN = function(x){
      y = x$y
      gets::regressorsMean(y=y, mxreg = mxreg,ar = ar,return.as.zoo = FALSE)
    })
    ar_df_processed <- as.data.frame(do.call("rbind",as.list(ar_df_processed)))

    # get data back out
    y <- ar_df_processed$y
    mxreg <- ar_df_processed[,!names(ar_df_processed) %in% c("y")]
    Tsample <- Tsample-ar

    # Adjust the time and id vectors
    df <- data.frame(do.call("rbind",by(df,df$id,FUN = function(x){x[!x$time == min(x$time),]})),row.names = NULL)
    time <- df$time
    id <- df$id

    # Adjust mxnames
    mxnames <- append(paste0("ar",1:ar),mxnames)
  }

  df_base <- df



  # Break Methods -----------------------------------------------------------


  # jsis = TRUE
  if(jsis){
    jsis_df <- as.data.frame(cbind(time = unique(time),gets::sim(Tsample)))
    df <- merge(df,jsis_df, by="time", all.x = TRUE,sort=FALSE)
  }

  #jiis = TRUE - This is just like a time FE
  if(jiis){
    jiis_df <- as.data.frame(cbind(time = unique(time),gets::iim(Tsample)))
    df <- merge(df,jiis_df, by="time", all.x = TRUE,sort=FALSE)
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

    df <- merge(df,fesis_df,by = c("id","time"),all.x=TRUE,sort=FALSE)
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
    df <- merge(df,csis_df,by = c("id","time"),all.x=TRUE,sort=FALSE)
  }

  # cfesis=TRUE
  if(cfesis){
    # Create a balanced data.frame
    df_balanced <- data.frame(id = rep(unique(id),each = Tsample),time = rep(unique(time),N))
    # Extract the names for the balanced data.frame
    fesis_names <- paste0("fesis",df_balanced$id[!df$time == min(df$time)],".",df_balanced$time[!df$time == min(df$time)])

    sistlist <- do.call("list", rep(list(as.matrix(gets::sim(Tsample))), N))
    fesis_df <- as.data.frame(as.matrix(Matrix::bdiag(sistlist)))
    names(fesis_df) <- fesis_names

    cfesis_df <- cbind(df_balanced,fesis_df)

    for(i in cfesis_var){
      cfesis_intermed <- cfesis_df[,!names(cfesis_df) %in% c("id","time")]*mxreg[,i]
      names(cfesis_intermed) <- paste0(i,".",names(cfesis_intermed))
      cfesis_df <- cbind(cfesis_df,cfesis_intermed)
    }

    df <- merge(df,cfesis_df,by = c("id","time"),all.x=TRUE,sort=FALSE)
  }

  # delete any columns that are 0 (can be included if panel not balanced)
  df <- df[, colSums(df != 0) > 0]


  if(any(fesis,jsis,jiis,csis,cfesis)){
    sispanx <- as.matrix(df[,!names(df) %in% c("id","time")])
  } else {
    sispanx = FALSE
  }



  if(is.null(engine)){
    ###########################################
    ####generating Fixed effects
    ##########################################

    ###if individual FEs needed
    if (effect %in% c("individual", "twoways")){
      iddum <- fastDummies::dummy_cols(data.frame(id=id),select_columns = "id",remove_first_dummy = FALSE,remove_selected_columns = TRUE)
      idnames <- paste("id", unique(id), sep="")

      ##### Individual FEs
      iddum_r <- as.matrix(iddum)
      idnames_r <- idnames

    }

    ###if time FEs needed
    if (effect %in% c("time", "twoways")){
      timedum <- fastDummies::dummy_cols(data.frame(time=time),select_columns = "time",remove_selected_columns = TRUE,remove_first_dummy = FALSE)
      timenames <- paste("time", unique(time), sep="")

      ####time FEs
      timedum_r <- as.matrix(timedum)
      timenames_r <- timenames

    }

    if (effect == "individual"){

      if (!is.null(mxreg))
      {
        mx <- cbind(mxreg, iddum)
        colnames(mx) <- c(mxnames, idnames_r)
      } else {
        mx <- iddum
        colnames(mx) <- (idnames_r)
      }


    } #if indiv. closed

    if(effect == "time"){

      #  NROW(timedum)

      if (!is.null(mxreg))
      {
        mx <- cbind(mxreg, timedum)
        colnames(mx) <- c(mxnames, timenames_r)
      } else {
        mx <- timedum
        colnames(mx) <- timenames_r
      }

    } #if time close

    if(effect == "twoways"){

      if (!is.null(mxreg))
      {
        mx <- cbind(mxreg, iddum, timedum)
        colnames(mx) <- c(mxnames, idnames_r, timenames_r)
      } else {
        mx <- cbind(iddum, timedum)
        colnames(mx) <- c(idnames_r, timenames_r)
      }

    } #if twoways closed

    if(effect == "none"){

      if (!is.null(mxreg))
      {
        mx <- mxreg

        if (NCOL(mx)>1){
          colnames(mx) <-  mxnames
        } else {
          names(mx) <- mxnames
        }
      } else { #there are no fixed effects and no regressors
        mx <- NULL
      }
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
        name = "felmFun",
        time = time,
        id = id,
        effect = effect,
        cluster = cluster
      )
      mc = FALSE
    }
    if(engine == "fixest"){
      user.estimator <- list(
        name = "fixestFun",
        time = time,
        id = id,
        effect = effect,
        cluster = cluster
      )
      mc = FALSE
    }
    if(engine == "plm"){
      user.estimator <- list(
        name = "plmFun",
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
    mc = TRUE
  }

  out$estimateddata <- data.frame(id,time,y,mx)

  #############################
  ####### Estimate
  ###############################
  #ispan <- gets::isat(y, mxreg = mx, iis=iis, sis=FALSE, uis=sispanx, user.estimator = user.estimator, mc=TRUE, ...)
  ispan <- isat.short(y, mxreg = mx, iis=iis, sis=FALSE, uis=sispanx, user.estimator = user.estimator, mc=FALSE, ...)

  ###############################
  ############## Return output
  ############################


  out$isatpanel.result <- ispan
  #out$finaldata <- cbind(out$inputdata,ispan$aux$mX[,!colnames(ispan$aux$mX) %in% names(out$inputdata)])


  class(out) <- "isatpanel"

  return(out)


} ###ispan function ends


