#distorttest(is1)
devtools::load_all()
library(gets)
# cores=detectCores()[1]-1
# cores[1]-1

########
set.seed(129403)
mu <- 0

N <- 100
# eps <- rnorm(N, 0, 1)
#  eps <- runif(N, 0, 1)
 eps <- rt(N, df=2, ncp=0) #works well with new boot! even with 50 observations
# eps <- rgamma(N, shape=1) #works well with new boot!
# eps <- rcauchy(N, 0, 1)
 x1 <- rnorm(N, 0, 1)
 x2 <- rnorm(N, 0, 1)
# hist(eps)
#y <- mu + 0.5*x1 + 0.5*x2 + eps
 y <- mu + 0.5*x1 + 0.5*x2 + eps
#
y[c(2, 10, 15, 27, 30, 55, 76, 85, 99, 90)] <- y[c(2, 10, 15, 27, 30, 55, 76, 85, 99, 90)] + 6

start.time <- Sys.time()

is1 <- isat(y, mxreg=cbind(x1, x2), t.pval=0.05, sis=FALSE, iis=TRUE, max.block.size=2)
is1

is1 <- isat(y, mxreg=NULL, t.pval=0.05, sis=FALSE, iis=TRUE, max.block.size=2)
is1

outliertest(is1)
end.time <- Sys.time()
time.diff <-  end.time - start.time
print(paste("Boot Complete in", sep=""))
print(time.diff)
#
distorttest(is1)

start.time <- Sys.time()


test1 <- distorttest.boot(
  x = is1,
  nboot = 5,
  parallel = TRUE
)


test1 <- distorttest.boot(
  x = is1,
  nboot = 200,
  clean.sample = TRUE,
  scale.t.pval = 1,
  parametric = FALSE,
  parallel = FALSE,
  ncore = 2,
  max.block.size = 2,
  turbo = FALSE
)
test1

hist(test1$coefdist.res$prop)
abline(v=test1$prop.full, col="orange")

outliertest(is1)

sum(test1$coefdist.res$prop > test1$prop.full)/100

end.time <- Sys.time()
time.diff <-  end.time - start.time
print(paste("Boot Complete in", sep=""))
print(time.diff)

x <- is1
clean.sample <- FALSE
scale.t.pval <- 1
nboot <- 50
parallel <- TRUE
