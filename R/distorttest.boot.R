#' Bootstrap Distortion Test Function
#'
#' @param x isat object (I think)
#' @param nboot Number of bootstraps
#' @param clean.sample logical
#' @param parametric logical. Currently: if parametric then clean.sample = TRUE
#' @param scale.t.pval numeric. Default = 1.
#' @param parallel logical. Default = FALSE
#' @param ncore to be used with parallel
#' @param ... Further arguments to be passed to ??
#'
#' @return
#' @examples
#' #' # A quick example with 10 replications
#' data(Nile)
#' Nile[10] <- Nile[10]+1000 # to ensure there is an indicator retained
#' nile <- isat(Nile, sis=FALSE, iis=TRUE, plot=TRUE, t.pval=0.005)
#'
#' distorttest.boot(nile, nboot = 10)
#'
#' # Another example with co-variates
#' dat <- hpdata[,c("GD", "GNPQ", "FSDJ")]
#' Y <- ts(dat$GD,start = 1959, frequency = 4)
#' mxreg <- ts(dat[,c("GNPQ","FSDJ")],start = 1959, frequency = 4)
#' m1 <- isat(y = Y, mc = TRUE, sis = FALSE, iis = TRUE)
#' m2 <- isat(y = Y, mc = TRUE, sis = FALSE, iis = TRUE, ar = 1)
#' m3 <- isat(y = Y, mxreg = mxreg, mc = TRUE, sis = FALSE, iis = TRUE)
#' m4 <- isat(y = Y, mxreg = mxreg, mc = TRUE, sis = FALSE, iis = TRUE, ar = 1)
#'
#' distorttest(m1)
#' distorttest(m2)
#' distorttest(m3)
#' distorttest(m4)
#'
#' # bootstrap
#' db1 <- distorttest.boot(m1, nboot = 10)
#' db2 <- distorttest.boot(m2, nboot = 10)
#' db3 <- distorttest.boot(m3, nboot = 10)
#' db4 <- distorttest.boot(m4, nboot = 10)
#'
#' db4.clean <- distorttest.boot(m4, nboot = 10, clean.sample = TRUE)
#' @export
#'
distorttest.boot <- function(
  x,
  nboot = 199,
  clean.sample = TRUE,
  parametric = FALSE,
  scale.t.pval = 1,
  parallel = FALSE,
  ncore = detectCores()[1] - 1,
  ...
){


  # x <- is1
  # nboot=199
  #
  ####compute distortion on full model
  dist.full <- distorttest(x)

  #names(dist.full$coef.diff)

  out.full <- outliertest(x)

  is0.date <- isatdates(x)$iis$index
  N <- x$aux$y.n
  # define the set over which to sample the bootstraps from as all observations apart from those where IIS identified outliers
  n.null <- seq(1:N)[!(seq(1:N) %in% is0.date)]

  boot.samples <- list()

  if (parametric){
    clean.sample <- TRUE
  }

  for (i in 1:nboot){
    if (clean.sample == TRUE){
      boot.samp <- sample(n.null, N, replace=TRUE)
    } else {
      boot.samp <- sample(seq(1:N), N, replace=TRUE)
    }
    boot.samples[[i]] <- boot.samp

  } #i closed

  # MORITZ: Can I move this lower to be called only when parallel = TRUE?
  require(foreach)
  require(doParallel)

  boot.tpval <- x$aux$t.pval*scale.t.pval #bootstrap level of significance of selection



  ### function used in parallel loop
  dist.boot.temp <- function(y.boot, x.boot, boot.tpval, ...){
    is.boot <- isat(y.boot, mxreg=x.boot, mc=FALSE, t.pval=boot.tpval, iis=TRUE, sis=FALSE,  print.searchinfo=FALSE, ...)
    dist.boot <- distorttest(is.boot)
    out.boot <- outliertest(is.boot)
    dist.res <- c(dist.boot$coef.diff, dist.boot$statistic,  out.boot$proportion$estimate,  out.boot$proportion$statistic, out.boot$count$estimate, out.boot$count$statistic)
    names(dist.res) <- c(names(dist.boot$coef.diff), "dist", "prop", "prop.test", "count", "count.test")
    return( dist.res)
    }


  if (parallel){
    #ncore=7
    #  boot.tpval <- 0.05
    cl <- makeCluster(ncore) #not to overload your computer
    registerDoParallel(cl)
    coefdist.sample <- foreach(i=1:nboot, .combine=rbind) %dopar% {
      require(gets)
      boot.samp <-  boot.samples[[i]]


      if (parametric==TRUE){ #parametric bootstrap (residual resampling)
        x.boot <- x$aux$mX[, !(colnames(x$aux$mX) %in% x$ISnames)]
        res.boot <- as.vector(x$residuals)[boot.samp]
        y.boot <-   x.boot %*% coefficients(x)[!(colnames(x$aux$mX) %in% x$ISnames)] + res.boot

      } else { #nonparametric
        y.boot <- x$aux$y[boot.samp]
        x.boot <- x$aux$mX[boot.samp, !(colnames(x$aux$mX) %in% x$ISnames)]
      }

      tempMatrix =   dist.boot.temp(y.boot, x.boot, boot.tpval)

    }
    #stop cluster
    stopCluster(cl)

  } else { #if not using parallel
    coefdist.sample <-foreach(i=1:nboot,.combine=rbind) %do% {
      boot.samp <-  boot.samples[[i]]


      if (parametric==TRUE){ #parametric bootstrap (residual resampling)
        x.boot <- x$aux$mX[, !(colnames(x$aux$mX) %in% x$ISnames)]
        res.boot <- as.vector(x$residuals)[boot.samp]
        y.boot <-   x.boot %*% coefficients(x)[!(colnames(x$aux$mX) %in% x$ISnames)] + res.boot

      } else { #nonparametric
        y.boot <- x$aux$y[boot.samp]
        x.boot <- x$aux$mX[boot.samp, !(colnames(x$aux$mX) %in% x$ISnames)]
      }
      #  y.boot <- x$aux$y[boot.samp]
      # x.boot <- x$aux$mX[boot.samp, !(colnames(x$aux$mX) %in% x$ISnames)]
      #
      dist.boot.temp(y.boot, x.boot, boot.tpval)
    }
  } #parallel if closed

  # end.time <- Sys.time()
  # time.diff <-  end.time - start.time
  # print(paste("Boot Complete in", sep=""))
  # print(time.diff)

  # start.time <- Sys.time()

  coefdist.sample <- as.data.frame(coefdist.sample)
  coefdist.res <- data.frame(matrix(NA, nrow=nboot, ncol=1))
  names(coefdist.res) <- "boot"
  coefdist.res$boot <- seq(1:nboot)
  if (length(names(dist.full$coef.diff)) > 1){
    coefdist.res$L2 <- apply(coefdist.sample[,names(dist.full$coef.diff)], 1, function(x)  sqrt(sum(x^2)))
    coefdist.res$L1 <- apply(coefdist.sample[,names(dist.full$coef.diff)], 1, function(x)  sum(abs(x)))
  } else {
    coefdist.res$L2 <- sqrt(coefdist.sample[,1]^2)
    coefdist.res$L1 <- abs(coefdist.sample[,1])
  }


  coefdist.res$dist <- coefdist.sample$dist

  coefdist.res$prop <- coefdist.sample$prop
  coefdist.res$count <- coefdist.sample$count
  coefdist.res$prop.test <- coefdist.sample$prop.test
  coefdist.res$count.test <- coefdist.sample$count.test


  L2.full <- sqrt(sum(dist.full$coef.diff^2))
  L1.full <- sum(abs(dist.full$coef.diff))
  dist.full.stat <- dist.full$statistic

  prop.full <- out.full$proportion$estimate
  prop.full.stat <- out.full$proportion$statistic

  count.full <- out.full$count$estimate
  count.full.stat <- out.full$count$statistic

  boot.q.prop <- quantile(coefdist.res$prop, probs = c(0.9, 0.95, 0.975, 0.99, 0.995))
  boot.q.count <- quantile(coefdist.res$count, probs = c(0.9, 0.95, 0.975, 0.99, 0.995))
  boot.q.prop.test <- quantile(coefdist.res$prop.test, probs = c(0.9, 0.95, 0.975, 0.99, 0.995))
  boot.q.count.test <- quantile(coefdist.res$count.test, probs = c(0.9, 0.95, 0.975, 0.99, 0.995))


  boot.q.L2 <- quantile(coefdist.res$L2, probs = c(0.9, 0.95, 0.975, 0.99, 0.995))
  boot.q.L1 <- quantile(coefdist.res$L1, probs = c(0.9, 0.95, 0.975, 0.99, 0.995))
  boot.q.dist <- quantile(coefdist.res$dist, probs = c(0.9, 0.95, 0.975, 0.99, 0.995))

  boot.p.L2 <- sum(coefdist.res$L2 > L2.full)/nboot
  boot.p.L1 <- sum(coefdist.res$L1 > L1.full)/nboot
  boot.p.dist <- sum(coefdist.res$dist > dist.full.stat)/nboot

  boot.p.prop <- 2*min(sum(coefdist.res$prop > prop.full)/nboot,  sum(coefdist.res$prop <= prop.full)/nboot) #check the p-values here.
  boot.p.count <- 2*min(sum(coefdist.res$count > count.full)/nboot, sum(coefdist.res$count <= count.full)/nboot)

  boot.p.prop.test <- sum(abs(coefdist.res$prop.test) > abs(prop.full.stat))/(nboot)
  boot.p.count.test <- sum(abs(coefdist.res$count.test) > abs(count.full.stat))/(nboot)


  out <- list()

  out$coefdist.res <- coefdist.res

  out$prop.full <- prop.full
  out$boot.q.prop <- boot.q.prop
  out$boot.p.prop <- boot.p.prop

  out$prop.full.stat <- prop.full.stat
  out$boot.q.prop.stat <- boot.q.prop.test
  out$boot.p.prop.stat <- boot.p.prop.test

  out$count.full <- count.full
  out$boot.q.count <- boot.q.count
  out$boot.p.count <- boot.p.count

  out$count.full.stat <- count.full.stat
  out$boot.q.count.stat <- boot.q.count.test
  out$boot.p.count.stat <- boot.p.count.test


  out$L2.full <- L2.full
  out$boot.q.L2 <- boot.q.L2
  out$boot.p.L2 <- boot.p.L2
  out$L1.full <- L1.full
  out$boot.q.L1 <- boot.q.L1
  out$boot.p.L1 <- boot.p.L1
  out$boot.q.dist <- boot.q.dist
  out$boot.p.dist <- boot.p.dist


  out$dist.full <- dist.full

  class(out) <- "distorttest.boot"

  return(out)

}
