################ Bootstrap Distortion Test Function ######




#########

require(gets)
source("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/code/distorttest_v5.R") ###change directory here


distorttest.boot <- function(x, nboot=199, clean.sample = TRUE, parametric=FALSE, scale.t.pval = 1, parallel=FALSE, ncore=detectCores()[1]-1, ...){

  # x <- is1
  # nboot=199
  # 
  ####compute distortion on full model
  dist.full <- distorttest(x)

  #names(dist.full$coef.diff)
  
  out.full <- outliertest(x)
  
  is0.date <- isatdates(x)$iis$index
  N <- x$aux$y.n
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
  
  require(foreach)
  require(doParallel)

  boot.tpval <- x$aux$t.pval*scale.t.pval #bootstrap level of significance of selection
  
  ### function used in parallel loop
  dist.boot.temp <- function(y.boot, x.boot, boot.tpval, ...){
    source("C:/Users/Felix/OneDrive/Documents/Projects/IS beta testing/code/distorttest_v5.R") #change directory here
    
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
 if (NCOL(names(dist.full$coef.diff)) > 1){
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
 
 return(out)
 
} #function closed
  
#distorttest(is1)

# library(gets)
# # # cores=detectCores()[1]-1
# # cores[1]-1
#
########
# set.seed(129403)
# mu <- 0
# 
# N <- 100
# # eps <- rnorm(N, 0, 1)
# #  eps <- runif(N, 0, 1)
#  eps <- rt(N, df=2, ncp=0) #works well with new boot! even with 50 observations
# # eps <- rgamma(N, shape=1) #works well with new boot!
# # eps <- rcauchy(N, 0, 1)
#  x1 <- rnorm(N, 0, 1)
#  x2 <- rnorm(N, 0, 1)
# # hist(eps)
# #y <- mu + 0.5*x1 + 0.5*x2 + eps
#  y <- mu + 0.5*x1 + 0.5*x2 + eps
# 
#  #y[c(2, 10, 15, 27, 30, 55, 76, 85, 99, 90)] <- y[c(2, 10, 15, 27, 30, 55, 76, 85, 99, 90)] + 6
# 
# start.time <- Sys.time()
# 
# is1 <- isat(y, mxreg=cbind(x1, x2), t.pval=0.05, sis=FALSE, iis=TRUE, max.block.size=2)
# is1
# 
# is1 <- isat(y, mxreg=NULL, t.pval=0.05, sis=FALSE, iis=TRUE, max.block.size=2)
# is1
# # 
# # outliertest(is1)
# # end.time <- Sys.time()
# # time.diff <-  end.time - start.time
# # print(paste("Boot Complete in", sep=""))
# # print(time.diff)
# # #
# # distorttest(is1)
# # 
# # start.time <- Sys.time()
# # 
# test1 <- distorttest.boot(x=is1, nboot=200, clean.sample = TRUE, scale.t.pval = 1, parametric = FALSE, parallel=TRUE, ncore=7, max.block.size=2, turbo=FALSE)
# test1
# # 
# # hist(test1$coefdist.res$prop)
# # abline(v=test1$prop.full, col="orange")
# 
# #outliertest(is1)
# 
# #sum(test1$coefdist.res$prop > test1$prop.full)/100
# 
# end.time <- Sys.time()
# # time.diff <-  end.time - start.time
# # print(paste("Boot Complete in", sep=""))
# # print(time.diff)

# x <- is1
# clean.sample <- FALSE
# scale.t.pval <- 1
# nboot <- 50
# parallel <- TRUE
