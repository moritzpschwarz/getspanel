
set.seed(1234)
reps <- 1 #number of replications, keep = 1 for simple demonstration
N <- 4 #cross-sectional dimension
T <- 100 #time dimension

escale <- 1 #error term is divided by this to scale the noise

############### Generate Artificial Data: hypothetical DGP where dependent variable depends on "Temp" and coefficient changes at time t=35

####### Breaks in Temperature Coefficient
breakval_temp = 35 #time when temperature coefficient changes
btemp <- 2 #original temperature coefficient
btemp_break <- 2 #change in temperature coefficient in response to break


#########################
iotatemp <- matrix(0, T, N)
tempbreaks <- matrix(0, N, 1)
tempbreaks[1:2] <- breakval_temp   #two countries have changing temp coefficients
iotatemp[breakval_temp:T,which(tempbreaks!=0)] <- 1

########country-specific effects (i.e. intercepts)
a <- matrix(NA, N, 1)
a[1:floor(N/2)] <- 3
a[(floor(N/2)+1):N] <- 5

####### generate regressors
temp <- matrix(rnorm(N*T, 2, 1), T, N) #generate random temperature series (independent variable)
year <- seq(1:T)+1900 # time index
num <- rep(1:N)
colnames(temp) <- paste( "temp", num, sep="")

gdp <- matrix(NA, T, N)
colnames(gdp) <- paste("gdp", num, sep="")


########data generation loop:
pandata <- replicate(reps, data.frame())

for (j in 1: reps){

  print("---------------------------------")
  print("------ Generating New Country:")
  print(j)
  print("-------")
  eps <- matrix(rnorm(N*T, 0, 1), T, N)

  for (i in 1:N){
    gdp[,i] <-  a[i] + btemp*temp[,i] + btemp_break*iotatemp[,i]*temp[,i] + eps[,i]/escale
  }

  #########Reshape the data into panel format
  data <- cbind(year, gdp, temp)
  data.pan <- matrix(NA, nrow=(N*T), ncol=2+2)
  data.pan[,1] <- rep(year, N)

  for (i in 1:N)
  {
    index_start <- (i-1)*T+1
    index_end <- index_start+T-1
    data.pan[index_start:index_end,2] <- i
    data.pan[index_start:index_end,3] <- gdp[,i]
    data.pan[index_start:index_end,4] <- temp[,i]
  }

  data.pan <- data.frame(data.pan)
  names(data.pan) <- c("year", "country", "gdp", "temp")
  head(data.pan)

  data.pan <- fastDummies::dummy_cols(.data = data.pan,select_columns = "country")
  data.pan$const <- 1
  dat <- data.pan
  pandata[[j]] <- dat

  print("--------Country Complete------")

} #data generation loop ends


##########################
######### load data
###########################

pandata_simulated <-  pandata[[1]]



usethis::use_data(pandata_simulated, overwrite = TRUE)
