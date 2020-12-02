isat.panel <- function (y, mc = TRUE, ar = NULL, ewma = NULL, mxreg = NULL,
          iis = FALSE, sis = TRUE, tis = FALSE, uis = FALSE, blocks = NULL,
          ratio.threshold = 0.8, max.block.size = 30, t.pval = 0.001,
          wald.pval = t.pval, vcov.type = c("ordinary", "white", "newey-west"),
          do.pet = FALSE, ar.LjungB = NULL, arch.LjungB = NULL, normality.JarqueB = NULL,
          info.method = c("sc", "aic", "hq"), user.diagnostics = NULL,
          user.estimator = NULL, gof.function = NULL, gof.method = c("min",
                                                                     "max"), include.gum = NULL, include.1cut = FALSE, include.empty = FALSE,
          max.paths = NULL, parallel.options = NULL, turbo = FALSE,
          tol = 1e-07, LAPACK = FALSE, max.regs = NULL, print.searchinfo = TRUE,
          plot = NULL, alarm = FALSE, time, id, cluster)
{
  isat.call <- sys.call()
  vcov.type <- match.arg(vcov.type)
  info.method <- match.arg(info.method)
  gof.method <- match.arg(gof.method)
  if (sis == FALSE && iis == FALSE && tis == FALSE && identical(uis,
                                                                FALSE)) {
    stop("No Indicator Selection Method was selected. Either set iis, sis or tis as TRUE or specify uis.")
  }
  y.name <- deparse(substitute(y))
  if (y.name[1] == "") {
    y.name <- "y"
  }
  if (is.null(ar)) {
    qstat.options <- c(1, 1)
  }
  else {
    qstat.options <- c(max(ar), 1)
  }
  if (!is.null(include.gum)) {
    warning("The 'include.gum' argument is ignored (temporarily deprecated in isat)")
  }
  include.gum <- TRUE
  if (is.null(user.estimator)) {
    olsMethod <- switch(vcov.type, ordinary = 3, white = 4,
                        `newey-west` = 5)
    userEstArg <- list(name = "ols.clustered", tol = tol, LAPACK = LAPACK, id=id, time = time, cluster = cluster,
                       method = olsMethod)
    userEstArgArx <- NULL
  }
  else {
    userEstArg <- user.estimator
    userEstArgArx <- user.estimator
  }
  if (is.null(gof.function)) {
    gofFunArg <- list(name = "infocrit", method = info.method)
  }
  else {
    gofFunArg <- gof.function
  }
  if (!is.null(max.paths) && max.paths < 1) {
    stop("'max.paths' cannot be smaller than 1")
  }
  if (!is.null(parallel.options)) {
    if (is.numeric(parallel.options)) {
      clusterSpec <- parallel.options
      OScores <- detectCores()
      if (parallel.options > OScores) {
        stop("parallel.options > number of cores/threads")
      }
    }
    if (is.list(parallel.options)) {
      clusterVarlist <- parallel.options$varlist
    }
    else {
      clusterVarlist <- NULL
    }
    clusterVarlist <- c(clusterVarlist, "dropvar", "getsFun",
                        "ols", "infocrit", "diagnostics")
    if (!is.null(user.diagnostics)) {
      clusterVarlist <- c(clusterVarlist, user.diagnostics$name)
    }
    if (!is.null(user.estimator)) {
      clusterVarlist <- c(clusterVarlist, user.estimator$name)
    }
    if (!is.null(gof.function)) {
      clusterVarlist <- c(clusterVarlist, gof.function$name)
    }
  }
  mX <- gets::regressorsMean(y, mc = mc, ar = ar, ewma = ewma, mxreg = mxreg,
                       return.regressand = TRUE, return.as.zoo = TRUE, na.trim = TRUE)
  y.n <- NROW(mX)
  y.index <- zoo::index(mX)
  y.index.as.char <- as.character(y.index)
  y <- zoo::coredata(mX[, 1])
  if (NCOL(mX) == 1) {
    mX <- NULL
    mXnames <- NULL
    mXncol <- 0
    mxkeep <- NULL
  }
  else {
    mXnames <- colnames(mX)[-1]
    mX <- as.matrix(zoo::coredata(mX[, -1]))
    colnames(mX) <- mXnames
    mXncol <- NCOL(mX)
    mxkeep <- 1:mXncol
  }
  arLjungB <- NULL
  if (!is.null(ar.LjungB)) {
    arLjungB <- c(NA, ar.LjungB$pval)
    if (is.null(ar.LjungB$lag)) {
      arLjungB[1] <- qstat.options[1]
    }
    else {
      arLjungB[1] <- ar.LjungB$lag
    }
  }
  archLjungB <- NULL
  if (!is.null(arch.LjungB)) {
    archLjungB <- c(NA, arch.LjungB$pval)
    if (is.null(arch.LjungB$lag)) {
      archLjungB[1] <- qstat.options[2]
    }
    else {
      archLjungB[1] <- arch.LjungB$lag
    }
  }
  ISmatrices <- list()
  if (iis) {
    mIIS <- matrix(0, y.n, y.n)
    diag(mIIS) <- 1
    colnames(mIIS) <- paste0("iis", y.index.as.char)
    ISmatrices <- c(ISmatrices, list(IIS = mIIS))
  }
  if (sis) {
    mSIS <- matrix(0, y.n, y.n)
    loop.indx <- 1:y.n
    tmp <- function(i) {
      mSIS[i, 1:i] <<- 1
    }
    tmp <- sapply(loop.indx, tmp)
    colnames(mSIS) <- paste0("sis", y.index.as.char)
    mSIS <- mSIS[, -1]
    ISmatrices <- c(ISmatrices, list(SIS = mSIS))
  }
  if (tis) {
    mTIS <- matrix(0, y.n, y.n)
    v1n <- seq(1, y.n)
    loop.indx <- 1:y.n
    tmp <- function(i) {
      mTIS[c(i:y.n), i] <<- v1n[1:c(y.n - i + 1)]
    }
    tmp <- sapply(loop.indx, tmp)
    colnames(mTIS) <- paste0("tis", y.index.as.char)
    mTIS <- mTIS[, -1]
    ISmatrices <- c(ISmatrices, list(TIS = mTIS))
  }
  if (!is.list(uis) && !identical(as.numeric(uis), 0)) {
    uis <- zoo::as.zoo(cbind(uis))
    uis.names <- colnames(uis)
    if (is.null(uis.names)) {
      uis.names <- paste0("uisxreg", 1:NCOL(uis))
    }
    if (any(uis.names == "")) {
      missing.colnames <- which(uis.names == "")
      for (i in 1:length(missing.colnames)) {
        uis.names[missing.colnames[i]] <- paste0("uisxreg",
                                                 missing.colnames[i])
      }
    }
    uis <- zoo::na.trim(uis, sides = "both", is.na = "any")
    uis.index.as.char <- as.character(zoo::index(uis))
    t1 <- which(uis.index.as.char == y.index.as.char[1])
    t2 <- which(uis.index.as.char == y.index.as.char[length(y.index.as.char)])
    uis <- zoo::coredata(uis)
    uis <- window(uis, start = t1, end = t2)
    uis <- cbind(zoo::coredata(zoo::as.zoo(uis)))
    colnames(uis) <- uis.names
    if (nrow(uis) != y.n)
      stop("nrow(uis) is unequal to no. of observations")
    ISmatrices <- c(ISmatrices, list(UIS = uis))
  }
  if (is.list(uis)) {
    for (i in 1:length(uis)) {
      uis[[i]] <- as.matrix(zoo::coredata(zoo::as.zoo(uis[[i]])))
      if (nrow(uis[[i]]) != y.n) {
        stop(paste("nrow(uis[[", i, "]]) is unequal to no. of observations",
                   sep = ""))
      }
    }
    uis.names <- paste0("UIS", 1:length(uis))
    if (is.null(names(uis))) {
      names(uis) <- uis.names
    }
    else {
      for (i in 1:length(uis)) {
        if (names(uis)[i] == "") {
          names(uis)[i] <- uis.names[i]
        }
        else {
          names(uis)[i] <- paste0(uis.names[i], ".",
                                  names(uis)[i])
        }
      }
    }
    ISmatrices <- c(ISmatrices, uis)
  }
  if (is.list(blocks)) {
    if (length(ISmatrices) != length(blocks)) {
      stop("No. of IS matrices is unequal to length(blocks)")
    }
    blocks.is.list <- TRUE
    ISblocks <- blocks
  }
  else {
    blocks.is.list <- FALSE
    ISblocks <- list()
  }
  ISfinalmodels <- list()
  for (i in 1:length(ISmatrices)) {
    if (!blocks.is.list) {
      ncol.adj <- NCOL(ISmatrices[[i]])
      if (is.null(blocks)) {
        blockratio.value <- ncol.adj/(ratio.threshold *
                                        ncol.adj - mXncol)
        blocksize.value <- ncol.adj/min(y.n * ratio.threshold,
                                        max.block.size)
        no.of.blocks <- max(2, blockratio.value, blocksize.value)
        no.of.blocks <- ceiling(no.of.blocks)
        no.of.blocks <- min(ncol.adj, no.of.blocks)
      }
      else {
        no.of.blocks <- blocks
      }
      blocksize <- ceiling(ncol.adj/no.of.blocks)
      partitions.t2 <- blocksize
      for (j in 1:no.of.blocks) {
        if (blocksize * j <= ncol.adj) {
          partitions.t2[j] <- blocksize * j
        }
      }
      if (partitions.t2[length(partitions.t2)] < ncol.adj) {
        partitions.t2 <- c(partitions.t2, ncol.adj)
      }
      blocksadj <- length(partitions.t2)
      partitions.t1 <- partitions.t2 + 1
      partitions.t1 <- c(1, partitions.t1[-blocksadj])
      tmp <- list()
      for (j in 1:blocksadj) {
        tmp[[j]] <- partitions.t1[j]:partitions.t2[j]
      }
      ISblocks[[i]] <- tmp
    }
    ISblocksFun <- function(j, i, ISmatrices, ISblocks,
                            mX, parallel.options, y, userEstArg, t.pval, wald.pval,
                            do.pet, arLjungB, archLjungB, normality.JarqueB,
                            user.diagnostics, gofFunArg, gof.method, mxkeep,
                            include.gum, include.1cut, include.empty, max.paths,
                            turbo, tol, LAPACK, max.regs, print.searchinfo) {
      if (length(ISblocks[[i]][[j]]) == 1) {
        tmp <- colnames(ISmatrices[[i]])[ISblocks[[i]][[j]]]
        mXis <- cbind(ISmatrices[[i]][, ISblocks[[i]][[j]]])
        colnames(mXis) <- tmp
        mXis <- cbind(mX, mXis)
      }
      else {
        mXis <- cbind(mX, ISmatrices[[i]][, ISblocks[[i]][[j]]])
      }
      mXis <- gets::dropvar(mXis, tol = tol, LAPACK = LAPACK,
                      silent = print.searchinfo)
      if (is.null(parallel.options)) {
        if (print.searchinfo) {
          message("\n", appendLF = FALSE)
          message(names(ISmatrices)[i], " block ", j,
                  " of ", length(ISblocks[[i]]), ":", appendLF = TRUE)
        }
      }
      getsis <- gets::getsFun(y, mXis, untransformed.residuals = NULL,
                        user.estimator = userEstArg, gum.result = NULL,
                        t.pval = t.pval, wald.pval = wald.pval, do.pet = do.pet,
                        ar.LjungB = arLjungB, arch.LjungB = archLjungB,
                        normality.JarqueB = normality.JarqueB, user.diagnostics = user.diagnostics,
                        gof.function = gofFunArg, gof.method = gof.method,
                        keep = mxkeep, include.gum = include.gum, include.1cut = include.1cut,
                        include.empty = include.empty, max.paths = max.paths,
                        turbo = turbo, tol = tol, LAPACK = LAPACK, max.regs = max.regs,
                        print.searchinfo = print.searchinfo, alarm = FALSE)
      if (is.null(getsis$specific.spec)) {
        ISspecific.models <- NULL
      }
      else {
        ISspecific.models <- names(getsis$specific.spec)
      }
      return(ISspecific.models)
    }
    if (is.null(parallel.options)) {
      ISspecific.models <- lapply(1:length(ISblocks[[i]]),
                                  ISblocksFun, i, ISmatrices, ISblocks, mX, parallel.options,
                                  y, userEstArg, t.pval, wald.pval, do.pet, arLjungB,
                                  archLjungB, normality.JarqueB, user.diagnostics,
                                  gofFunArg, gof.method, mxkeep, include.gum,
                                  include.1cut, include.empty, max.paths, turbo,
                                  tol, LAPACK, max.regs, print.searchinfo)
    }
    if (!is.null(parallel.options)) {
      if (print.searchinfo) {
        message("\n", appendLF = FALSE)
        message("Preparing parallel computing...", appendLF = TRUE)
        message(names(ISmatrices)[i], " blocks to search in parallel: ",
                length(ISblocks[[i]]), appendLF = TRUE)
        message("Searching...", appendLF = TRUE)
      }
      blocksClust <- makeCluster(clusterSpec, outfile = "")
      clusterExport(blocksClust, clusterVarlist, envir = .GlobalEnv)
      ISspecific.models <- parLapply(blocksClust, 1:length(ISblocks[[i]]),
                                     ISblocksFun, i, ISmatrices, ISblocks, mX, parallel.options,
                                     y, userEstArg, t.pval, wald.pval, do.pet, arLjungB,
                                     archLjungB, normality.JarqueB, user.diagnostics,
                                     gofFunArg, gof.method, mxkeep, include.gum,
                                     include.1cut, include.empty, max.paths, turbo,
                                     tol, LAPACK, max.regs, print.searchinfo)
      stopCluster(blocksClust)
    }
    if (print.searchinfo) {
      message("\n", appendLF = FALSE)
      message("GETS of union of retained ", names(ISmatrices)[i],
              " variables... ", appendLF = TRUE)
    }
    if (length(ISspecific.models) == 0) {
      isNames <- NULL
      ISfinalmodels[[i]] <- NULL
    }
    if (length(ISspecific.models) > 0) {
      isNames <- NULL
      for (j in 1:length(ISspecific.models)) {
        if (!is.null(ISspecific.models[[j]])) {
          isNames <- union(isNames, ISspecific.models[[j]])
        }
      }
      isNames <- setdiff(isNames, mXnames)
      if (length(isNames) == 0) {
        ISfinalmodels[[i]] <- mXnames
      }
      else {
        mXisNames <- c(mXnames, isNames)
        mXis <- cbind(mX, ISmatrices[[i]][, isNames])
        colnames(mXis) <- mXisNames
        mXis <- gets::dropvar(mXis, tol = tol, LAPACK = LAPACK,
                        silent = print.searchinfo)
        getsis <- gets::getsFun(y, mXis, untransformed.residuals = NULL,
                          user.estimator = userEstArg, gum.result = NULL,
                          t.pval = t.pval, wald.pval = wald.pval, do.pet = do.pet,
                          ar.LjungB = arLjungB, arch.LjungB = archLjungB,
                          normality.JarqueB = normality.JarqueB, user.diagnostics = user.diagnostics,
                          gof.function = gofFunArg, gof.method = gof.method,
                          keep = mxkeep, include.gum = include.gum,
                          include.1cut = include.1cut, include.empty = include.empty,
                          max.paths = max.paths, turbo = turbo, tol = tol,
                          LAPACK = LAPACK, max.regs = max.regs, print.searchinfo = print.searchinfo,
                          alarm = FALSE)
        ISfinalmodels[[i]] <- names(getsis$specific.spec)
      }
    }
  }
  names(ISblocks) <- names(ISmatrices)
  if (print.searchinfo) {
    message("\n", appendLF = FALSE)
    message("GETS of union of ALL retained variables...",
            appendLF = TRUE)
  }
  if (length(ISfinalmodels) > 0) {
    mIS <- NULL
    for (i in 1:length(ISfinalmodels)) {
      isNames <- NULL
      if (!is.null(ISfinalmodels[[i]])) {
        isNames <- setdiff(ISfinalmodels[[i]], mXnames)
      }
      if (length(isNames) > 0) {
        tmp <- cbind(ISmatrices[[i]][, isNames])
        colnames(tmp) <- isNames
        mIS <- cbind(mIS, tmp)
      }
    }
    mXis <- gets::dropvar(cbind(mX, mIS), tol = tol, LAPACK = LAPACK,
                    silent = print.searchinfo)
  }
  browser()
  if (length(ISfinalmodels) == 0) {
    ISfinalmodels <- NULL
    if (is.null(mX)) {
      mXis <- NULL
    }
    else {
      mXis <- cbind(mX)
      colnames(mXis) <- mXnames
    }
  }
  getsis <- gets::getsFun(y, mXis, untransformed.residuals = NULL,
                    user.estimator = userEstArg, gum.result = NULL, t.pval = t.pval,
                    wald.pval = wald.pval, do.pet = do.pet, ar.LjungB = arLjungB,
                    arch.LjungB = archLjungB, normality.JarqueB = normality.JarqueB,
                    user.diagnostics = user.diagnostics, gof.function = gofFunArg,
                    gof.method = gof.method, keep = mxkeep, include.gum = include.gum,
                    include.1cut = include.1cut, include.empty = include.empty,
                    max.paths = max.paths, turbo = turbo, tol = tol, LAPACK = LAPACK,
                    max.regs = max.regs, print.searchinfo = print.searchinfo,
                    alarm = FALSE)
  if (print.searchinfo && !is.null(getsis$messages)) {
    message(getsis$messages)
  }
  y <- zoo(y, order.by = y.index)
  if (is.null(getsis$specific.spec)) {
    mXisNames <- NULL
    mXis <- NULL
  }
  else {
    mXisNames <- colnames(mXis)[getsis$specific.spec]
    mXis <- cbind(mXis[, getsis$specific.spec])
    colnames(mXis) <- mXisNames
    mXis <- zoo(mXis, order.by = y.index)
  }
  if (is.null(normality.JarqueB)) {
    normalityArg <- FALSE
  }
  else {
    normalityArg <- as.numeric(normality.JarqueB)
  }
  mod <- arx.panel(y, mxreg = mXis, vcov.type = vcov.type, qstat.options = qstat.options,
             normality.JarqueB = normalityArg, user.estimator = userEstArgArx,
             user.diagnostics = user.diagnostics, tol = tol, LAPACK = LAPACK, time, id, cluster,
             plot = FALSE)
  mod$call <- NULL
  ISnames <- setdiff(mXisNames, mXnames)
  if (length(ISnames) == 0) {
    ISnames <- NULL
  }
  colnames(mod$aux$mX) <- mod$aux$mXnames
  getsis$gets.type <- "isat"
  getsis$call <- isat.call
  getsis <- c(list(ISfinalmodels = ISfinalmodels, ISnames = ISnames),
              getsis, mod)
  getsis$aux$t.pval <- t.pval
  class(getsis) <- "isat"
  if (alarm) {
    alarm()
  }
  if (is.null(plot)) {
    plot <- getOption("plot")
    if (is.null(plot)) {
      plot <- FALSE
    }
  }
  if (plot) {
    plot.isat(getsis, coef.path = TRUE)
  }
  return(getsis)
}
