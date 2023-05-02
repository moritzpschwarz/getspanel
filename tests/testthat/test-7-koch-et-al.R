# Prepare Data
set.seed(1230)
#data <- read.csv("CO2DriversEU_dataset.csv")
data("EU_emissions_road")
data <- EU_emissions_road
data$lgdp_sq <- data$lgdp^2

data$transport.emissions_pc <- data$transport.emissions/data$pop
data$ltransport.emissions_pc <- log(data$transport.emissions_pc)

data$L1.ltransport.emissions_pc <- NA
# For each country, shift the values of 'ltransport.emissions_pc' by one position
for (i in unique(data$country)) {
  # Extract the 'ltransport.emissions_pc' values for the current country
  current_country_values <- data$ltransport.emissions_pc[data$country == i]

  # Shift the values by one position and insert an NA value at the beginning
  shifted_values <- c(NA, current_country_values[-length(current_country_values)])

  # Assign the shifted values to the corresponding rows in 'L1.ltransport.emissions_pc'
  data$L1.ltransport.emissions_pc[data$country == i] <- shifted_values
}

# data %>%
#   dplyr::group_by(country) %>%
#   dplyr::mutate(test = dplyr::lag(ltransport.emissions_pc)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(istrue = test == L1.ltransport.emissions_pc) %>%
#   dplyr::distinct(istrue)


# Group specification
EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg",
          "Netherlands", "Greece", "Portugal", "Sweden")
# EU16 <- c("Croatia", "Bulgaria", "Cyprus","Czech Republic", "Estonia",
#           "Hungary", "Lithuania", "Latvia", "Malta", "Poland", "Romania",
#           "Slovak Republic", "Slovenia", "Switzerland", "Iceland",
#           "Norway")
# EU31 <- c(EU15, EU16)



###### Analysis:

# Prepare sample and data
sample <- EU15
dat <- data[data$country %in% sample & data$year>=1995,] 



# Break analysis:
is1 <- isatpanel(
  data = dat,
  formula = ltransport.emissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T,
  t.pval=.05,
  print.searchinfo = FALSE
)


# Break analysis:
is2 <- isatpanel(
  data = dat,
  formula = ltransport.emissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T,
  t.pval=.01,
  print.searchinfo = FALSE
)



# Break analysis:
is3 <- isatpanel(
  data = dat,
  formula = ltransport.emissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = T,
  fesis = T,
  t.pval=.001,
  print.searchinfo = FALSE
)

get_countries <- function(x){
  df <- x$estimateddata
  indicators <- x$isatpanel.result$aux$mX
  indicators <- indicators[,!colnames(indicators) %in% names(df)]
  df <- cbind(df,indicators)
  identify_indicator_timings(df)
}

is1_coef <- coef(is1$isatpanel.result)[get_countries(is1)$fesis$name]
is2_coef <- coef(is2$isatpanel.result)[get_countries(is2)$fesis$name]
is3_coef <- coef(is3$isatpanel.result)[get_countries(is3)$fesis$name]

is1_coef <- is1_coef[order(names(is1_coef))]
is2_coef <- is2_coef[order(names(is2_coef))]
is3_coef <- is3_coef[order(names(is3_coef))]

is1_tib <- data.frame(name = c("fesisFinland.2000",
                                  "fesisGermany.2002",
                                  "fesisIreland.2011",
                                  "fesisIreland.2015",
                                  "fesisLuxembourg.2007",
                                  "fesisSweden.2001"),
                         coef = c(-0.103,
                                  -0.105,
                                  -0.087,
                                  -0.148,
                                  -0.136,
                                  -0.095))


is2_tib <- data.frame(name = c("fesisFinland.2000",
                                  "fesisGermany.2002",
                                  "fesisIreland.2015",
                                  "fesisSweden.2001"),
                         coef = c(-0.123,
                                  -0.131,
                                  -0.192,
                                  -0.103))


is3_tib <- data.frame(name = c("fesisFinland.2000",
                                  "fesisGermany.2002",
                                  "fesisIreland.2011",
                                  "fesisLuxembourg.2015",
                                  "fesisSweden.2001"),
                         coef = c(-0.128,
                                  -0.108,
                                  -0.127,
                                  -0.214,
                                  -0.110))

test_that("Equal Breaks as in Koch et al are identified",{

  is1_tib_estimated <- data.frame(name = names(is1_coef[is1_coef < 0]),
                                     coef = as.numeric(round(is1_coef[is1_coef < 0], 3)))

  expect_equal(is1_tib, is1_tib_estimated)


  is2_tib_estimated <- data.frame(name = names(is2_coef[is2_coef < 0]),
                                     coef = as.numeric(round(is2_coef[is2_coef < 0], 3)))

  expect_equal(is2_tib, is2_tib_estimated)

  is3_tib_estimated <- data.frame(name = names(is3_coef[is3_coef < 0]),
                                     coef = as.numeric(round(is3_coef[is3_coef < 0], 3)))

  expect_equal(is3_tib, is3_tib_estimated)

})



test_that("Equal Break Uncertainties as in Koch et al are identified",{

  break_is1 <- break_uncertainty( is1, interval = 0.99)
  break_is2 <- break_uncertainty( is2, interval = 0.99)
  break_is3 <- break_uncertainty( is3, interval = 0.99)

  break_is1 <- dplyr::filter( break_is1, coef < 0 )
  break_is2 <- dplyr::filter( break_is2, coef < 0 )
  break_is3 <- dplyr::filter( break_is3, coef < 0 )



  is1_tib_break <- data.frame(name = c("fesisFinland.2000",
                                          "fesisGermany.2002",
                                          "fesisIreland.2011",
                                          "fesisIreland.2015",
                                          "fesisLuxembourg.2007",
                                          "fesisSweden.2001"),
                                 tci = c(2,
                                         2,
                                         3,
                                         1,
                                         1,
                                         2))


  is2_tib_break <- data.frame(name = c("fesisFinland.2000",
                                          "fesisGermany.2002",
                                          "fesisIreland.2015",
                                          "fesisSweden.2001"),
                                 tci = c(2,
                                         1,
                                         1,
                                         2))


  is3_tib_break <- data.frame(name = c("fesisFinland.2000",
                                          "fesisGermany.2002",
                                          "fesisIreland.2011",
                                          "fesisLuxembourg.2015",
                                          "fesisSweden.2001"),
                                 tci = c(2,
                                         3,
                                         2,
                                         1,
                                         3))




  expect_equal(is1_tib_break, as.data.frame(break_is1[,c("name", "tci")]))
  expect_equal(is2_tib_break, as.data.frame(break_is2[,c("name", "tci")]))
  expect_equal(is3_tib_break, as.data.frame(break_is3[,c("name", "tci")]))

})




# ##### uncertainty for break dates
#
# ####This file compiles the uncertainty functions needed for sis
#
#
# library(mvtnorm)
#
# ### Function for rotation
# rotate <- function(x) t(apply(x, 2, rev))
#
# ### Function to extract matrices
# mextrc <- function(x, p=0, k=0){
#   # k=2*(plus minus), i.e. total range, and p is where it starts relative to the center, so k is the size, and p is the starting point
#
#   mid <- NROW(x)/2
#   out <- 2*k
#   rotate <- function(x) t(apply(x, 2, rev))
#   subx <- x[(mid+p-k):(mid+p-k+out+1),(mid+p-k):(mid+p-k+out+1)]
#   subx <- rotate(rotate(rotate(x)))[(mid+p-k):(mid+p-k+out+1),(mid+p-k):(mid+p-k+out+1)]
#   output <- rotate(rotate(rotate(subx)))
#   return(output)
# }
#
# #####################################################
#
# ####### Function to compute approximate SIS uncertainty
#
# isattime <- function(m, delta, plot=FALSE){
#
#   #######################################
#
#   lim <- 2*m
#   sigma <- diag(2*m)
#   a_lim <- m-1
#
#   ########-----------Constructing general matrix
#
#   up0 <- (matrix(NA, nrow=2*m, ncol=1))
#   up <- (matrix(NA, nrow=2*m, ncol=(m)))
#   upg.a <- seq(1:lim)
#   upg <- c(rev(upg.a*(-1)), upg.a)
#   upg[(lim+1):(lim+m-1)]
#
#
#   ###loop to generate
#   for (j in 1:m){
#
#     #T0 values
#     up0[j, 1] <- delta/2*((m+1)-j)
#     up0[j+m, 1] <- delta/2*j
#
#     #Tp+m values
#     ##loop over rows
#     for (l in 1:(m+1))
#     {
#       #j is the column
#       up[l,j] <- delta/2 *(((m+1)-l)-j)
#       up[l+m-1, j] <- delta/2 * upg[(lim-1-j+l)]
#     }
#   }
#
#
#   up.tot <- cbind(up0, up)
#
#   #######------Constructing the general A Matrix
#
#   A <- matrix(0, nrow=lim, ncol=lim)
#   for (i in 1:lim){
#     A[i, 1:i] <- 1
#   }
#   A
#   Ar <- rotate(A)
#   Arm <- (-1)*rotate(rotate(rotate(A)))
#   Az <- matrix(0, nrow=lim, ncol=lim)
#   Ag <- rbind(cbind(Az, Ar), cbind(Arm, Az))
#   Ag
#   Agb <- Ag*(-1)
#
#   ###A0
#   A0 <- mextrc(Agb, 0, a_lim )*(-1)
#   A0gsigma <- A0 %*% sigma %*% t(A0)
#   p0 <- pmvnorm(mean=rep(0, NROW(A0gsigma)), sigma=A0gsigma, lower=rep(-Inf, NROW(A0gsigma)), upper=up.tot[,1] )[1]
#   p0
#
#   p_m <- matrix(NA, m, 1)
#
#   ### up to m
#   for (i in 1:m){
#     Ai <- mextrc(Agb, i, a_lim )
#     Aisigma <- Ai %*% sigma %*% t(Ai)
#     pi <- pmvnorm(mean=rep(0, NROW(Aisigma)), sigma=Aisigma, lower=rep(-Inf, NROW(Aisigma)), upper=up.tot[,(i+1)] )[1]
#     p_m[i,1] <- pi
#   }
#
#   p.tot <- as.matrix(c(rev(p_m), p0, p_m))
#   p.tot
#   colSums(p.tot)
#
#   x <- seq(from = -m, to=m, by=1)
#
#   if (plot){
#     plot(x, p.tot, type='hist')
#   }
#   return(p.tot)
# }
#
# #######################################################
# #################################################
#
#
#
# ############# Input
#
# model.names <- c("EU15_levelpc_p0.05", "EU15_levelpc_p0.01", "EU15_levelpc_p0.001")
#
# dat.input <- list() #break magnitudes (estimated coefficients)
# breaknames <- list() #break title for overview
# se <- c() #s.e. of regression (i.e. sqrt of estimated error variance)
#
# # Extract coeffs and se from model .Rds:
# for(k in 1:length(model.names)){
#   #mod <- model.names[k]
#
#   #raw <- readRDS(paste0("../../A Break Detection/Analysis4a (level)/Analysis4a/", mod, ".Rds"))
#   raw <- list(is1, is2, is3)[k][[1]]
#
#   finalmodel <- raw$isatpanel.result$mean.results
#
#   coeffs <- finalmodel$coef[stringr::str_detect(row.names(finalmodel), "fesis")]
#   breaks <- row.names(finalmodel)[stringr::str_detect(row.names(finalmodel), "fesis")] %>%
#     stringr::str_remove("fesis")
#
#   sigm <- sqrt(raw$isatpanel.result$sigma2)
#
#
#   dat.input[[k]] <- coeffs
#   se[k] <- sigm
#   breaknames[[k]] <- breaks
# }
#
#
# ############ Calibration parameters
# interval <- 0.99 #approx. level of interval. CI level will be at least > interval. So 0.99 is a 99% CI, and interval will be the integer that gets at least > 99% coverage
# m <- 15 #maximum range of interval (can leave at 15 for default).
#
# set.seed(1234)
#
# #######################################
# #################
#
# for(k in 1:length(model.names)){
#   dat <- dat.input[[k]]/se[k] #standardizing break magnitudes
#   prob <- matrix(NA, NROW(dat), 2*m+1)
#   ci <- matrix(NA, NROW(dat), 4)
#
#   for (i in 1:NROW(dat)){
#     #i <- 1
#     time <- isattime(m, abs(dat[i])) #external function to compute approx. CI
#
#     prob[i,] <- t(time)
#     cu.p <- matrix(NA, (length(time)+1)/2, 1)
#     for (j in 1:((length(time)+1)/2))
#     {
#       start <-  (length(time)+1)/2
#       cu.p[j,1] <- sum(time[((start-j+1):(start+j-1))])
#     }
#
#     thresh <- interval
#     ci.v <- min(which(abs(cu.p)>thresh))
#     ci.d <- ci.v - 1
#     cu.p[ci.v]
#
#     ci[i,1] <- ci.d #interval is given by estimated date + - ci.d
#     ci[i,2] <- cu.p[ci.v] #coverage probability
#     ci[i,3] <- breaknames[[k]][i] #which break is considered
#     ci[i,4] <- dat.input[[k]][i] #break magnitude
#
#   }
#
#   colnames(ci) <- c("half_range", "coverage", "break_title", "break_magnitude")
#
#   cat(paste0("\n Model ", k, "/", length(model.names), " done."))
# }




