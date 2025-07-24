#' Estimate Breakdate Uncertainty
#'
#' @param x An object produced by the isatpanel function
#' @param m Maximum range of interval (default is 15 time periods).
#' @param interval Approximate level of interval. CI level will be at least > interval. Default 0.99 is a 99% CI, so the time interval will always be the integer that results in at least > 99% coverage.
#'
#' @return A data.frame that indicates the uncertainty for each FESIS break. The time interval is given by the estimated date in the 'time' column with a confidence interval of +/- the interval in the tci column.
#' @export
#'
#' @importFrom mvtnorm pmvnorm
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
#'
#' break_uncertainty(result)
#'}

break_uncertainty <- function(x, m = 15, interval = 0.99){

  ### First define all relevant functions

  ### Function for rotation
  rotate <- function(x) t(apply(x, 2, rev))

  ### Function to extract matrices
  mextrc <- function(x, p=0, k=0){ #k=2*(plus minus), i.e. total range, and p is where it starts relative to the center, so k is the size, and p is the starting point

    mid <- NROW(x)/2
    out <- 2*k
    rotate <- function(x) t(apply(x, 2, rev))
    subx <- x[(mid+p-k):(mid+p-k+out+1),(mid+p-k):(mid+p-k+out+1)]
    subx <- rotate(rotate(rotate(x)))[(mid+p-k):(mid+p-k+out+1),(mid+p-k):(mid+p-k+out+1)]
    output <- rotate(rotate(rotate(subx)))
    return(output)
  }

  #####################################################

  ####### Function to compute approximate SIS uncertainty

  isattime <- function(m, delta, plot=FALSE){

    #######################################

    lim <- 2*m
    sigma <- diag(2*m)
    a_lim <- m-1

    ########-----------Constructing general matrix

    up0 <- (matrix(NA, nrow=2*m, ncol=1))
    up <- (matrix(NA, nrow=2*m, ncol=(m)))
    upg.a <- seq(1:lim)
    upg <- c(rev(upg.a*(-1)), upg.a)
    upg[(lim+1):(lim+m-1)]


    ###loop to generate
    for (j in 1:m){

      #T0 values
      up0[j, 1] <- delta/2*((m+1)-j)
      up0[j+m, 1] <- delta/2*j

      #Tp+m values
      ##loop over rows
      for (l in 1:(m+1))
      {
        #j is the column
        up[l,j] <- delta/2 *(((m+1)-l)-j)
        up[l+m-1, j] <- delta/2 * upg[(lim-1-j+l)]
      }

    }


    up.tot <- cbind(up0, up)

    #######------Constructing the general A Matrix

    A <- matrix(0, nrow=lim, ncol=lim)
    for (i in 1:lim){

      A[i, 1:i] <- 1

    }
    A
    Ar <- rotate(A)
    Arm <- (-1)*rotate(rotate(rotate(A)))
    Az <- matrix(0, nrow=lim, ncol=lim)
    Ag <- rbind(cbind(Az, Ar), cbind(Arm, Az))
    Ag
    Agb <- Ag*(-1)

    ###A0
    A0 <- mextrc(Agb, 0, a_lim )*(-1)
    A0gsigma <- A0%*%sigma%*%t(A0)
    p0 <- pmvnorm(mean=rep(0, NROW(A0gsigma)), sigma=A0gsigma, lower=rep(-Inf, NROW(A0gsigma)), upper=up.tot[,1] )[1]
    p0

    p_m <- matrix(NA, m, 1)

    ### up to m
    for (i in 1:m){
      Ai <- mextrc(Agb, i, a_lim )
      Aisigma <- Ai%*%sigma%*%t(Ai)
      pi <- pmvnorm(mean=rep(0, NROW(Aisigma)), sigma=Aisigma, lower=rep(-Inf, NROW(Aisigma)), upper=up.tot[,(i+1)] )[1]
      p_m[i,1] <- pi
    }

    p.tot <- as.matrix(c(rev(p_m), p0, p_m))
    p.tot
    colSums(p.tot)

    x <- seq(from = -m, to=m, by=1)

    if (plot)
    {
      plot(x, p.tot, type='hist')
    }
    return(p.tot)
  }

  df <- x$estimateddata
  indicators <- x$isatpanel.result$aux$mX
  indicators <- indicators[,!colnames(indicators) %in% names(df)]
  df <- cbind(df,indicators)

  df_identified_coef <- get_indicators(x)$fesis

  if(is.null(df_identified_coef)){stop("Function currently only works on FESIS variables - No FESIS Indicators found in this isatpanel object.")}

  dat <- df_identified_coef$coef/sqrt(x$isatpanel.result$sigma2)
  prob <- matrix(NA, NROW(dat), 2*m+1)
  ci <- matrix(NA, NROW(dat), 4)

  for (i in 1:NROW(dat)){
    #i <- 1
    time <- isattime(m, abs(dat[i])) #external function to compute approx. CI

    prob[i,] <- t(time)
    cu.p <- matrix(NA, (length(time)+1)/2, 1)
    for (j in 1:((length(time)+1)/2)){
      start <-  (length(time)+1)/2
      cu.p[j,1] <- sum(time[((start-j+1):(start+j-1))])
    }

    thresh <- interval
    ci.v <- min(which(abs(cu.p)>thresh))
    ci.d <- ci.v - 1
    cu.p[ci.v]

    ci[i,1] <- ci.d #interval is given by estimated date + - ci.d
    ci[i,2] <- cu.p[ci.v] #coverage probability
    ci[i,3] <- i #which break number is considered (corresponding to elements in "dat")
    ci[i,4] <- df_identified_coef$coef[i] #break magnitude
  }
  colnames(ci) <- c("half_range", "coverage", "break_number", "break_magnitude")

  df_identified_coef$tci <- ci[,"half_range"]
  return(df_identified_coef)
}


