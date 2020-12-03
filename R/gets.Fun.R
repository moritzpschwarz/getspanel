##do gets fast and with full flexibility (for advanced users)
gets.Fun <- function(
  y,
  x,
  untransformed.residuals = NULL,
  user.estimator = list(name = "ols"),
  gum.result = NULL,
  t.pval = 0.05,
  wald.pval = t.pval,
  do.pet = TRUE,
  ar.LjungB = NULL,
  arch.LjungB = NULL,
  normality.JarqueB = NULL,
  user.diagnostics = NULL,
  gof.function = list(name = "infocrit"),
  gof.method = c("min", "max"),
  keep = NULL,
  include.gum = FALSE,
  include.1cut = FALSE,
  include.empty = FALSE,
  max.paths = NULL,
  turbo = FALSE,
  tol = 1e-07,
  LAPACK = FALSE,
  max.regs = NULL,
  print.searchinfo = TRUE,
  alarm = FALSE
)
{
  ## DO NOT:
  ## - introduce a check of the type NROW(y)==NCOL(x), since this will
  ##   invalidate situations where the x's contain coefficients rather
  ##   than regressors (e.g. when models are non-linear in parameters)
  ## TO DO:
  ## - introduce check for is.vector(y)==TRUE?
  ## - introduce check for is.matrix(x)==TRUE?
  ## - let out$specific.spec be equal to the GUM in the case where
  ##   all regressors are significant in the GUM?
  ## - if gof.function is not default, e.g. adjusted R-squared, then
  ##   it seems the value of logl is added to terminals.results
  ##   unnecessarily. Look into?
  ## - turbo: replace length(regsDeleteList) with regsDeleteList.n?
  ## - turbo: redefine regsFun function (careful!: setequal is delicate)
  ## - envir argument in user.estimator: change default behaviour?

  ## contents:
  ## 1 arguments
  ## 2 initialise
  ## 3 gum
  ## 4 1-cut model
  ## 5 empty model
  ## 6 multi-path search
  ## 7 find the best model
  ## 8 output


  ##-----------------------
  ## 1 arguments
  ##-----------------------

  gof.method <- match.arg(gof.method)

  ##y, x, make auxiliary list:
  if( is.null(x) || NCOL(x)==0 ){ stop("GUM regressor matrix is empty") }
  x <- cbind(x) #ensure x is a matrix
  aux <- list()
  aux$y.n <- NROW(y)
  aux$xNCOL <- NCOL(x)

  ##make user-estimator argument:
  userEstArg <- user.estimator
  userEstArg$name <- NULL
  userEstArg$envir <- NULL
  if( length(userEstArg)==0 ){ userEstArg <- NULL }

  ##make gof.function argument:
  if( gof.function$name=="infocrit" && is.null(gof.function$method) ){
    gof.function$method <- "sc"
  }
  gofFunArg <- gof.function
  gofFunArg$name <- NULL
  gofFunArg$envir <- NULL
  if( length(gofFunArg)==0 ){ gofFunArg <- NULL }

  ##max.paths argument:
  if( !is.null(max.paths) && max.paths < 1 ){
    stop("'max.paths' cannot be smaller than 1")
  }

  ##do diagnostics?:
  if( !is.null(ar.LjungB) || !is.null(arch.LjungB)
      || !is.null(normality.JarqueB) || !is.null(user.diagnostics) ){
    doDiagnostics <- TRUE
  }else{ doDiagnostics <- FALSE }

  ## max.regs:
  if(is.null(max.regs)){ max.regs <- 10*aux$y.n }


  ##-----------------------
  ## 2 initialise
  ##-----------------------

  ##add to auxiliary list:
  aux$mR <- matrix(0, aux$xNCOL, aux$xNCOL)
  diag(aux$mR) <- 1 #restriction matrix for PETs

  ##make out list, add to out list:
  out <- list()
  out$time.started <- date()
  out$time.finished <- NA
  out$call <- sys.call()
  out$no.of.estimations <- 0
  out$messages <- NULL
  out$paths <- list() #the paths
  out$terminals <- list() #terminal specifications
  out$terminals.results <- NULL #matrix w/terminals results
  row.labels <- NULL #row labels for out$terminals.results matrix

  ##deletable, non-deletable regressors, re-organise:
  keep <- as.integer(keep) #do not change to as.numeric(NULL) nor as.vector(NULL), since this may affect setequal/!anyNA...etc. inside the turbo
  keep.n <- length(keep)
  gum <- 1:aux$xNCOL
  delete <- setdiff(gum, keep) #integer(0) if empty
  delete.n <- length(delete)

  ##if all regressors in keep, add gum to terminals:
  if( delete.n==0 && include.gum==FALSE ){
    include.gum <- TRUE
    out$messages <- paste(out$messages,
                          "- All regressors in 'keep', GUM added to terminals", sep="")
  }

  ##-----------------------
  ## 3 gum
  ##-----------------------

  ##estimate GUM:
  if( is.null(gum.result) ){
    if( is.null(user.estimator$envir) ){
      est <- do.call(user.estimator$name, c(list(y,x), userEstArg))
    }else{
      est <- do.call(user.estimator$name,
                     c(list(y,x), userEstArg), envir=user.estimator$envir)
    }
    out$no.of.estimations <- out$no.of.estimations + 1
  }else{ est <- gum.result }

  ##do diagnostics:
  if( doDiagnostics ){
    gumDiagnosticsOK <- diagnostics(est, ar.LjungB=ar.LjungB,
                                    arch.LjungB=arch.LjungB, normality.JarqueB=normality.JarqueB,
                                    verbose=FALSE, user.fun=user.diagnostics)
  }else{ gumDiagnosticsOK <- TRUE }

  ## if GUM passes diagnostic checks:
  if( gumDiagnosticsOK ){

    ##record data for Wald-tests (pets) against gum:
    gum.regs <- gum
    gum.coefs <- est$coefficients
    gum.varcovmat <- est$vcov

    ##compute stderrs, t-stats, p-vals:
    stderrs <- sqrt(diag(est$vcov))
    gum.tstat <- est$coefficients/stderrs
    gum.pval <- pt(abs(gum.tstat), est$df, lower.tail=FALSE)*2

    ##these two lines are repeated later under 1-cut, and - if
    ##max.paths < n.paths - adjusted in the multi-path search:
    insig.regs <- setdiff( which(gum.pval > t.pval), keep)
    n.paths <- length(insig.regs)

    ##if all regressors significant, ensure gum is a terminal:
    if( n.paths==0 ){ include.gum <- TRUE }

    if( include.gum ){

      out$terminals[[1]]  <- gum #add gum to list of terminal specs

      ##specification results
      if( is.null(gof.function$envir) ){
        gofValue <- do.call(gof.function$name, c(list(est),gofFunArg))
      }else{
        gofValue <- do.call(gof.function$name,
                            c(list(est),gofFunArg), envir=gof.function$envir)
      }
      out$terminals.results <- rbind(out$terminals.results,
                                     c(gofValue, est$logl, est$n, est$k))
      row.labels <- c(row.labels, "spec 1 (gum):")

    } #end if(include.gum)

  }else{
    out$messages <- paste(out$messages,
                          "- GUM does not pass one or more diagnostic checks", sep="")
  }


  ##-----------------------
  ## 4 1-cut model
  ##-----------------------

  if( gumDiagnosticsOK && delete.n>0 && include.1cut ){

    ##all non-keep regressors significant:
    if( n.paths==0 ){
      out$messages <- paste(out$messages,
                            "- 1-CUT not included (all non-keep regressors are significant)",
                            sep="")
    }

    ##one or more non-keep regressor insignificant:
    if( n.paths>0 ){

      ##estimate 1cut:
      mXadj <- cbind(x[,-insig.regs])
      if( is.null(user.estimator$envir) ){
        est <- do.call(user.estimator$name, c(list(y,mXadj), userEstArg))
      }else{
        est <- do.call(user.estimator$name, c(list(y,mXadj), userEstArg),
                       envir=user.estimator$envir)
      }
      out$no.of.estimations <- out$no.of.estimations + 1

      ##do diagnostics:
      if( doDiagnostics ){
        diagnosticsOK <- diagnostics(est, ar.LjungB=ar.LjungB,
                                     arch.LjungB=arch.LjungB, normality.JarqueB=normality.JarqueB,
                                     verbose=FALSE, user.fun=user.diagnostics)
      }else{ diagnosticsOK <- TRUE }

      ## if 1cut passes diagnostic checks:
      if( diagnosticsOK ){

        ## do pet (i.e. wald-test):
        if( do.pet ){
          mR <- rbind(aux$mR[insig.regs,])
          mRestq <- mR %*% cbind(gum.coefs)
          wald.stat <- t(mRestq)%*%qr.solve(mR%*%gum.varcovmat%*%t(mR), tol=tol) %*% mRestq
          petOK <- as.logical(wald.pval < pchisq(wald.stat, n.paths, lower.tail = FALSE))
        }else{ petOK <- TRUE }

        ##add 1-cut to terminals?:
        if( petOK ){

          #add 1cut to list of terminal specs:
          spec.1cut <- setdiff(gum, insig.regs)
          out$terminals[[ length(out$terminals)+1 ]] <- spec.1cut

          ##specification results
          if( is.null(gof.function$envir) ){
            gofValue <- do.call(gof.function$name, c(list(est),
                                                     gofFunArg))
          }else{
            gofValue <- do.call(gof.function$name, c(list(est),
                                                     gofFunArg), envir=gof.function$envir)
          }
          out$terminals.results <- rbind(out$terminals.results,
                                         c(gofValue, est$logl, est$n, est$k))
          row.labels <- c(row.labels,
                          paste("spec ", length(out$terminals), " (1-cut):", sep=""))

        } #end if(petOK)

      } ##end if(diagnosticsOK)

    } ###end if(n.paths > 0)

  } ####end if(1-cut model)


  ##-----------------------
  ## 5 empty model
  ##-----------------------

  if( gumDiagnosticsOK && delete.n>0 && include.empty ){

    ##Here: do NOT do pet in order to enable reality check!

    ##check if empty = 1-cut:
    if( include.1cut && exists("spec.1cut") ){
      emptyEqualTo1cut <- identical(keep, spec.1cut)
    }else{ emptyEqualTo1cut <- FALSE }

    ##empty equal to 1cut?:
    if( emptyEqualTo1cut ){
      out$messages <- paste0(out$messages,
                             "- The empty model is equal to the 1-cut model")
    }else{

      ## estimate model:
      mXadj <- cbind(x[,keep])
      if( is.null(user.estimator$envir) ){
        est <- do.call(user.estimator$name, c(list(y,mXadj), userEstArg))
      }else{
        est <- do.call(user.estimator$name, c(list(y,mXadj), userEstArg),
                       envir=user.estimator$envir)
      }
      out$no.of.estimations <- out$no.of.estimations + 1

      ##do diagnostics:
      if(doDiagnostics){
        diagnosticsOK <- diagnostics(est, ar.LjungB=ar.LjungB,
                                     arch.LjungB=arch.LjungB, normality.JarqueB=normality.JarqueB,
                                     verbose=FALSE, user.fun=user.diagnostics)
      }else{ diagnosticsOK <- TRUE }

      ## if diagnostics are OK:
      if(diagnosticsOK){

        out$terminals[[ length(out$terminals)+1 ]] <- keep #note: integer(0) if keep=NULL

        ##specification results
        if( is.null(gof.function$envir) ){
          gofValue <- do.call(gof.function$name, c(list(est),
                                                   gofFunArg))
        }else{
          gofValue <- do.call(gof.function$name, c(list(est),
                                                   gofFunArg), envir=gof.function$envir)
        }
        out$terminals.results <- rbind(out$terminals.results,
                                       c(gofValue, est$logl, est$n, est$k))
        row.labels <- c(row.labels,
                        paste("spec ", length(out$terminals), " (empty):", sep=""))

      }else{

        out$messages <- paste0(out$messages,
                               "- Empty model not included (it does not pass one or more diagnostics)")

      } #end if(empty passes diagnostics==TRUE){..}else{..}

    } ##end if( emptyEqualTo1cut )else(...)

  } ###end if(include empty model==TRUE)


  ##-----------------------
  ## 6 multi-path search
  ##-----------------------

  #OLD (already set under 'gum'):
  #insig.regs <- NULL
  pathsTerminals <- list()
  if( gumDiagnosticsOK && delete.n>0 ){

    ##number of paths:
    #OLD:
    #  insig.regs <- setdiff( which(gum.pval > t.pval), keep)
    if( !is.null(max.paths) ){
      if(max.paths < length(insig.regs)){
        pvalRanksInv <- rank( 1-gum.pval[insig.regs] )
        insig.regs <- insig.regs[ pvalRanksInv <= max.paths ]
      }
    }
    n.paths <- length(insig.regs) #re-define n.paths

    ## if paths = 0:
    if(n.paths == 0){
      out$messages <- paste(out$messages,
                            "- All non-keep regressors significant in GUM", sep="")
    }

    ## if paths > 0:
    if(n.paths > 0){

      if(print.searchinfo){
        message(n.paths, " path(s) to search")
        message("Searching: ", appendLF=FALSE)
      }

      ##initiate bookkeeping of paths:
      #add if(turbo){...}?
      regsDeleteList <- list()
      regsKeepList <- list()
      regsMat <- NULL
      #browser()
      ## paths:
      for(i in 1:n.paths){

        ## print searchinfo:
        if(print.searchinfo){
          newLine <- ifelse(i==n.paths, TRUE, FALSE)
          message(i, " ", appendLF=newLine)
        }

        ## prepare single-path search:
        path <- insig.regs[i]
        delete.adj <- setdiff(delete, insig.regs[i])
        keep.adj <- keep

        ## single-path search of path i:
        for(j in 1:max.regs){

          ##begin turbo:
          if(turbo && j>1){

            ##bookkeeping of paths:
            regsDeleteList.n <- length(regsDeleteList)
            if( regsDeleteList.n==0 || i==1 ){
              #          if( length(regsDeleteList)==0 || i==1 ){

              counter <- regsDeleteList.n + 1
              #            counter <- length(regsDeleteList)+1
              regsDeleteList[[ counter ]] <- delete.adj
              regsKeepList[[ counter ]] <- keep.adj
              regsMat <- rbind(regsMat, c(i,length(path)))

            }else{

              ##delete list:
              whichOnesInDelete <- which( sapply(regsDeleteList,
                                                 setequal, delete.adj) )
              #OLD:
              #regsFun <- function(x){ setequal(x,delete.adj) }
              #whichOnesInDelete <- which( sapply(regsDeleteList, regsFun) )
              if( length(whichOnesInDelete)==0 ){
                counter <- regsDeleteList.n + 1
                #              counter <- length(regsDeleteList)+1
                regsDeleteList[[ counter ]] <- delete.adj
                regsKeepList[[ counter ]] <- keep.adj
                regsMat <- rbind(regsMat, c(i,length(path)))
                regsDeleteAlreadyDone <- FALSE
              }else{
                regsDeleteAlreadyDone <- TRUE
              }

              ##keep list:
              if( regsDeleteAlreadyDone ){

                ##keep already done?
                whichOnesInKeep <- which( sapply(regsKeepList,
                                                 setequal, keep.adj) )
                #OLD:
                #regsFun <- function(x){ setequal(x, keep.adj) }
                #whichOnesInKeep <- which( sapply(regsKeepList, regsFun) )
                whichOne <- intersect(whichOnesInDelete, whichOnesInKeep)
                #faster version of intersect:
                #y[match(as.vector(x), y, 0L)]
                if( length(whichOne) == 1 ){
                  regsKeepAlreadyDone <- TRUE
                }else{
                  counter <- regsDeleteList.n + 1
                  #                counter <- length(regsDeleteList)+1
                  regsDeleteList[[ counter ]] <- delete.adj
                  regsKeepList[[ counter ]] <- keep.adj
                  regsMat <- rbind(regsMat, c(i,length(path)))
                  regsKeepAlreadyDone <- FALSE
                }

                ##both delete and keep already done:
                if( regsKeepAlreadyDone ){
                  spec.adj <- pathsTerminals[[ regsMat[whichOne,1] ]]
                  pathtmp <- out$paths[[ regsMat[whichOne,1] ]]
                  pathtmp <- pathtmp[ -c(1:regsMat[whichOne,2]) ]
                  path <- c(path, pathtmp)
                  break # stop single path search
                }

              } #end regsDeleteAlreadyDone

            } ##end bookkeeping of paths

          } ### end turbo

          ## estimate model:
          #regsAdj <- union(delete.adj, keep.adj)
          mXadj <- cbind(x[, union(delete.adj,keep.adj) ])
          if( is.null(user.estimator$envir) ){
            est <- do.call(user.estimator$name, c(list(y,mXadj),
                                                  userEstArg))
          }else{
            est <- do.call(user.estimator$name, c(list(y,mXadj),
                                                  userEstArg), envir=user.estimator$envir)
          }
          out$no.of.estimations <- out$no.of.estimations + 1

          ##do diagnostics:
          if(doDiagnostics){
            diagnosticsOK <- diagnostics(est, ar.LjungB=ar.LjungB,
                                         arch.LjungB=arch.LjungB, normality.JarqueB=normality.JarqueB,
                                         verbose=FALSE, user.fun=user.diagnostics)
          }else{ diagnosticsOK <- TRUE }

          ## move regressor to keep.adj?:
          if( !diagnosticsOK ){
            path.n <- length(path)
            keep.adj <- union(path[path.n], keep.adj)
            path <- union(path, path[path.n]*c(-1))
            next #next j
          }

          ## if empty model passes diagnostic checks:
          if( diagnosticsOK ){

            ## stop if no more deletable regressors:
            if( length(delete.adj)==0 ){
              spec.adj <- sort(keep.adj)
              break
            } #end if(length(..)==0)

            #for the future?:
            #if( is.null(est$vcov) ){
            #  est$vcov <- vcovFun(est, method="ordinary")
            #}
            #this will speed up estimation whenever diagnosticsOK
            #turns out to be FALSE. Also, it will provide the user
            #with more flexibility in choosing the covariance matrix

            ##compute stderrs, t-stats, p-vals:
            stderrs <- sqrt(diag(est$vcov))
            t.stat <- est$coefficients/stderrs
            p.val <- pt(abs(t.stat), est$df, lower.tail=FALSE)*2

            ## try deleting a regressor:
            if( any( p.val[1:c(length(delete.adj))] > t.pval) > 0 ){

              reg.no <- which.max( p.val[1:c(length(delete.adj))] )

              ## do pet test (i.e. wald-test):
              if(do.pet){
                deleted <- setdiff(delete, delete.adj[-reg.no])
                deleted <- sort(deleted) #sort() needed for correct restrictions
                n.deleted <- length(deleted)
                mR <- rbind(aux$mR[deleted,])
                mRestq <- mR %*% cbind(gum.coefs)
                wald.stat <- t(mRestq)%*%qr.solve(mR%*%gum.varcovmat%*%t(mR), tol=tol) %*% mRestq
                petOK <- as.logical(wald.pval < pchisq(wald.stat, n.deleted, lower.tail = FALSE))
              }else{
                petOK <- TRUE
              } #end if(do.pet)else..

              ## delete regressor if(petOK), else move to keep:
              if( petOK ){
                path <- union(path, delete.adj[reg.no])
                delete.adj <- delete.adj[-reg.no]
              }else{
                path <- union(path, delete.adj[reg.no]*c(-1))
                keep.adj <- union(delete.adj[reg.no], keep.adj)
                delete.adj <- delete.adj[-reg.no]
              } #end if( petOK )else{..}

            }else{
              spec.adj <- sort(union(delete.adj, keep.adj))
              break
            } #end if( any p-value > t.pval )else(..)

          } ##end if diagnostics are ok

        } ### end single-path search: for(j in..


        #add path to the paths list:
        counter <- length(out$paths)+1
        out$paths[[ counter ]] <- path
        pathsTerminals[[ counter ]] <- spec.adj

        ##check if spec.adj (terminal) is already in out$terminals:
        if( length(out$terminals)==0 ){
          chk.spec <- FALSE
        }else{
          for(l in 1:length(out$terminals)){
            chk.spec <- setequal(spec.adj, out$terminals[[l]])
            if(chk.spec==TRUE){break} #stop for(l in..)
          }
        } #end check

        ##if spec.adj not in out$terminals (among terminals):
        if(chk.spec==FALSE){

          #add spec.adj to out$terminals:
          out$terminals[[ length(out$terminals)+1 ]] <- spec.adj
          if( is.null(gof.function$envir) ){
            gofValue <- do.call(gof.function$name, c(list(est),
                                                     gofFunArg))
          }else{
            gofValue <- do.call(gof.function$name, c(list(est),
                                                     gofFunArg), envir=gof.function$envir)
          }
          out$terminals.results <- rbind(out$terminals.results,
                                         c(gofValue, est$logl, est$n, est$k))
          row.labels <- c(row.labels, paste("spec ", length(out$terminals), ":", sep=""))

        } #end if(chk.spec==FALSE)

      } ##end multi-path search: for(i in 1:n.paths) loop

    } ###end if paths > 0

  } #####end if( gumDiagnosticsOK && delete.n>0 )


  ##-----------------------
  ## 7 find the best model
  ##-----------------------

  if( !is.null(out$terminals.results) ){

    ##which is the best model(s):
    if( gof.method=="min" ){
      out$best.terminal <- which.min(out$terminals.results[,1])
    }else{
      out$best.terminal <- which.max(out$terminals.results[,1])
    }

    ##check for several minimums:
    if( length(out$best.terminal)>1 ){
      out$messages <- paste(out$messages,
                            "- Several 'best' terminals, the first selected", sep="")
    }
    out$best.terminal <- out$best.terminal[1]
    out$specific.spec <- out$terminals[[ out$best.terminal ]] #the winner

    ##'prettify' out$specific.spec:
    if( length(out$specific.spec)==0 ){
      out$specific.spec <- NULL
    }else{
      out$specific.spec <- sort(out$specific.spec)
      names(out$specific.spec) <- colnames(x)[ out$specific.spec ]
    }

    ##'prettify' out$terminals.results and out$paths:
    if( gof.function$name=="infocrit" ){
      col.labels <- c(paste("info(", gofFunArg$method, ")", sep=""),
                      "logl", "n", "k")
    }else{
      col.labels <- c("gof-value", "logl", "n", "k")
    }
    if( NCOL(out$terminals.results) != length(col.labels) ){
      col.labels <- c(col.labels[1], rep(NA,NCOL(out$terminals.results)-1))
    }
    colnames(out$terminals.results) <- col.labels
    rownames(out$terminals.results) <- row.labels
    if( length(out$paths)==0 ){ out$paths <- NULL }

  } #end if( !is.null(out$terminals.results) )


  ##-----------------------
  ## 8 output
  ##-----------------------

  out$time.finished <- date()
  if(alarm){ alarm() }
  return(out)

} #close getsFun function
