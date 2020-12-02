############ Panel estimation function within R-package "gets"

########### panel isat function

#' Panel isat function
#'
#' @param y Year
#' @param id Individual
#' @param time Time
#' @param mxreg The co-variates matrix
#' @param mxbreak The break matrix
#' @param break.method Break Method, one of "both" or "individual"
#' @param effect Fixed Effect specification
#' @param iis use Impulse Indicator Saturation
#' @param na.remove remove NAs
#' @param engine Function to use
#' @param user.estimator Use a user.estimator
#' @param cluster cluster Standard Errors at this level
#' @param plm_model Type of PLM model (only if engine = "PLM")
#' @param ar Autoregressive Term to be inlucded. default is 0.
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
  mxbreak,
  break.method = c("individual"),
  effect = c("individual"),
  iis = FALSE,
  na.remove = TRUE,
  engine = NULL,
  user.estimator = NULL,
  cluster = "individual",
  plm_model=NULL,
  ar=0,
  ...
)
{
  # Set up Infrastructure
  out <- list()
  out$inputdata <- data.frame(id,time,y,mxreg,mxbreak)

  # Remove any spaces in the id variables (e.g. United Kingdom becomes UnitedKingdom)
  id <- gsub(" ","",id)
  # stats::lag()



  mxnames <- colnames(mxreg)
  if (!is.null(mxreg)){
    mxreg <- as.matrix(mxreg)
    if (is.null(mxnames)) {
      mxnames <- paste("x", seq_len(NCOL(mxreg)), sep="")
    }
  } else {
    mxnames <- NULL
  }

  ############# Get sample length and id out
  Tsample  <- length(unique(time))
  N <- length(unique(id))
  breakvar <- mxbreak

  #if a breakvariable is specified:
  #### determine if there are more than one variables allowed to break
  if (!is.null(mxbreak)) {
    if (NCOL(mxbreak) > 1 ){
      multibreak <- TRUE
      nbreaks <- NCOL(mxbreak)
    } else {
      multibreak <- FALSE
      nbreaks <- 1
    }
    #### list where break matrices are stored
    sispanxlist <- list()

    ###############################
    ###### Loop over number of variables allowed to break and create break matrix
    ################################

    for (n in seq_len(NCOL(mxbreak)))
    {
      if (multibreak){

        mxbreak <- breakvar[,n]
        mxbreakname <- colnames(mxbreak)
        if (is.null(mxbreakname)){
          mxbreakname <- paste("mxbreak", n,  sep="")
        }

      } else {

        breakvar <- breakvar
        mxbreakname <- colnames(mxbreak)
        if (is.null(mxbreakname)){
          mxbreakname <- "mxbreak1"
        }
      }

      if (var(mxbreak, na.rm = TRUE)!=0){ #if mxbreak is not a constant, then don't drop the intercept
        sis1 <- as.matrix(rep(1,Tsample))
        colnames(sis1) <- "sis1"

        sism <- cbind(sis1, gets::sim(Tsample))

        colnames(sism) <- paste(mxbreakname, "t", seq_len(NCOL(sism)), sep="")

      } else { #if it is a constant, then drop the intercept, then break model (2)

        sism <- gets::sim(Tsample)
        colnames(sism) <- paste(mxbreakname, "t", (seq_len(ncol(sism))+1), sep="")

      }

      sist <- as.matrix(sism)

      ##############################
      ############## create break matrix depending on method
      #############################

      if (break.method=="time"){    #if we are forcing breaks to be common over i

        ###drop the first column
        sist <- sist[,-1]
        sispan <- do.call(rbind, replicate(N, sist, simplify=FALSE))

      }

      if (break.method=="individual"){

        IN <- diag(N)
        rT <- rep(1, Tsample)
        ###- first column because one id has to be the base
        sispan <- kronecker(IN[,-1], rT)

      }

      if (break.method=="both")
      {

        sistlist <- do.call("list", rep(list(sist), N))
        sispan <- as.matrix(Matrix::bdiag(sistlist))

      }

      ####### multiply break matrix by break variable
      sispanx <- mxbreak*sispan

      #########################
      ######## name the break matrix
      ##############################

      if (break.method=="individual"){
        colnames(sispanx) <- paste(mxbreakname, "id", seq(from=2, to=N, by=1), sep="")
      }


      if (break.method=="both"){
        cn <- colnames(sist)
        ids <- unique(id)
        index <- Tsample
        if (var(mxbreak, na.rm = TRUE)==0){
          index <- Tsample-1
        }
        idsn <- matrix(t(matrix(ids,length(ids), (index) )))
        cnn <- rep(cn, N)
        length(cnn)
        cnnp <- paste(cnn, "id", idsn, sep="")
        colnames(sispanx) <- cnnp

      } #if both closed

      ######################
      ########### remove rows from the break matrix that are missing in the sample
      ######################

      if (na.remove) ###drop the rows to match the NROWs of y which will be dropped later
      {

        if (!is.null(mxreg))
        {
          rel.xy <- cbind(y, mxreg)
        } else {
          rel.xy <- y
        }

        if (any(!complete.cases(rel.xy)))  {
          remove <- which(!complete.cases(rel.xy)==TRUE)

          if (!is.null(mxbreak)){
            sispanx <- sispanx[-remove,]
          }

        }
      } #na.remove closed


      sispanxlist[[n]] <- sispanx

    } ####looping over breaks n ends

    #####################
    #### combine all the break matrices
    ###################

    sispanx <- do.call(cbind, sispanxlist)

  }  else { ## if is null mxbreak, ie. not a break variable given

    sispanx <- FALSE

  } ##is null mxbreak closed

  if (!is.null(mxreg))
  {
    if (NCOL(mxreg)>1){
      colnames(mxreg) <-  mxnames
    } else {
      names(mxreg) <- mxnames
    }
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
      iddum_r <- as.matrix(iddum[,-1])
      idnames_r <- idnames[-1]

    }

    ###if time FEs needed
    if (effect %in% c("time", "twoways")){
      timedum <- fastDummies::dummy_cols(data.frame(time=time),select_columns = "time",remove_selected_columns = TRUE,remove_first_dummy = FALSE)
      timenames <- paste("time", unique(time), sep="")

      ####time FEs
      timedum_r <- as.matrix(timedum[,-1])
      timenames_r <- timenames[-1]

    }

    if (effect == "individual"){

      if (!is.null(mxreg))
      {
        mx <- cbind(mxreg, iddum[,-1])
        colnames(mx) <- c(mxnames, idnames_r)
      } else {
        mx <- iddum[,-1]
        colnames(mx) <- (idnames_r)
      }


    } #if indiv. closed

    if(effect == "time"){

      #  NROW(timedum)

      if (!is.null(mxreg))
      {
        mx <- cbind(mxreg, timedum[,-1])
        colnames(mx) <- c(mxnames, timenames_r)
      } else {
        mx <- timedum[,-1]
        colnames(mx) <- timenames_r
      }

    } #if time close

    if(effect == "twoways"){

      if (!is.null(mxreg))
      {
        mx <- cbind(mxreg, iddum[,-1], timedum[,-1])
        colnames(mx) <- c(mxnames, idnames_r, timenames_r)
      } else {
        mx <- cbind(iddum[,-1], timedum[,-1])
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

  if (na.remove)
  {

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

  #############################
  ####### Estimate
  ###############################

  #ispan <- gets::isat(y, mxreg = mx, iis=iis, sis=FALSE, uis=sispanx, user.estimator = user.estimator, mc=TRUE, ...)
  ispan <- isat.short(y, mxreg = mx, iis=iis, sis=FALSE, uis=sispanx, user.estimator = user.estimator, mc=mc, ...)

  ###############################
  ############## Return output
  ############################

  out$isatpanel.result <- ispan
  #out$finaldata <- cbind(out$inputdata,ispan$aux$mX[,!colnames(ispan$aux$mX) %in% names(out$inputdata)])


  class(out) <- "isatpanel"

  return(out)


} ###ispan function ends


