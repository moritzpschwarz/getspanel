
rm(list = ls())  #clear the workspace

setwd("C:/Users/Felix/OneDrive/Documents/Projects/Causal Breaks/code")
#setwd("C:/Users/EMoD/Dropbox/Paleo Future/Future_Simulation/Future_Simulation")

#install.packages("Rtools")

#devtools::install_github("moritzpschwarz/getspanel")
library(getspanel)
#install.packages("processx")
#install.packages("zoo")
#install.packages("utf")
#devtools::install_github("moritzpschwarz/getspanel")
library(tidyverse) # needed for the plots
#install.packages("tidyverse")


################# Basque data

library(Synth)
#install.packages("Synth")
data(basque)
str(basque)


length(unique(basque$regionname))

###Full Sample Treatment Model
basque$treat <- 0
basque$treat[basque$year > 1975 & basque$regionno==17] <- 1
m1 <- lm(gdpcap ~ treat + as.factor(regionno) + as.factor(year), data=basque)
summary(m1)

### Match with PLM
library(plm)
m1.p <- plm(gdpcap ~ treat, index=c("regionno", "year"), data=basque, model="within", effect="twoways")
summary(m1.p)


basqrel <- basque[,c("gdpcap",   "invest",  "regionname", "year")]
basqrel.comp <- na.omit(basqrel)


######### Construct Lags
lg <- function(x)c(NA, x[1:(length(x)-1)])
library(plyr)

basqrel.comp <- ddply(basqrel.comp, ~regionname, transform, L1.gdpcap = lg(gdpcap))
basqrel.comp <- ddply(basqrel.comp, ~regionname, transform, L1.invest = lg(invest))

basqrel.comp$lgdpcap <- log(basqrel.comp$gdpcap)
basqrel.comp$linvest <- log(basqrel.comp$invest)

basqrel.comp$L1.lgdpcap <- log(basqrel.comp$L1.gdpcap)
basqrel.comp$L1.linvest <- log(basqrel.comp$L1.invest)

basqrel.comp$dlgdpcap <- basqrel.comp$lgdpcap - basqrel.comp$L1.lgdpcap
basqrel.comp$dlinvest <- basqrel.comp$linvest - basqrel.comp$L1.linvest

basqrel.comp <- na.omit(basqrel.comp)



##########################################
####################### Example 1: One treated unit (Basque), one control unit. Treatment Unknown
# control_reg <- "Principado De Asturias"
# control_reg <- "Cataluna"
# control_reg <- "Navarra (Comunidad Foral De)"
# control_reg <- "Principado De Asturias" #not bad finds two
# control_reg <- "Galicia" #no
# control_reg <- "Castilla-La Mancha" #no
# control_reg <- "Rioja (La)" #no
# control_reg <- "Castilla Y Leon" #no

###### These work well:
control_reg <- "Madrid (Comunidad De)" #works really well
#control_reg <- "Cantabria" #good! Finds a 1970 and 1978 for looser


unique(basqrel.comp$regionname)
####Things that work:
# control: "Cantabria", no IIS, t.pval=0.00001: finds exactly one. Seems to also work reasonably well with the LASSO. With looser significance finds another in 1970.
# control "Principado De Asturias", t.pval=0.01: finds two, first is great. Second is positive in 1986. What happened? which may be problematic?
# control "Navarra (Comunidad Foral De)", t.pval=0.001: finds two, early 80s, which may be problematic?
# control "Madrid (Comunidad De)", t.pval=0.001 finds one break for ETA



#### Subset:
basqrel.comp.reg <- basqrel.comp[basqrel.comp$regionname %in% c("Basque Country (Pais Vasco)",  control_reg)
                                 & basqrel.comp$year > 1964 & basqrel.comp$year < 1996 ,]
#basqrel.comp <- basqrel.comp[basqrel.comp$regionname %in% c("Basque Country (Pais Vasco)", "Madrid (Comunidad De)" ),]



years <- unique(basqrel.comp.reg$year)


############## Time Series Approach, no control units

library(gets)
basq_lgdp <- zooreg(basqrel.comp.reg[basqrel.comp.reg$regionname %in% "Basque Country (Pais Vasco)","lgdpcap"], min(basqrel.comp.reg$year))
basq_linv <-  zooreg(basqrel.comp.reg[basqrel.comp.reg$regionname %in% "Basque Country (Pais Vasco)","linvest"], min(basqrel.comp.reg$year))
# is1.bas.noctr <- isat(y= basq_lgdp, mxreg=basq_linv , sis=TRUE, iis=FALSE)

is1.bas.noctr <- isatpanel(data = basqrel.comp.reg[basqrel.comp.reg$regionname %in% "Basque Country (Pais Vasco)",],
                           formula = lgdpcap ~   linvest,
                           index = c("regionname","year"),
                           effect = "none",
                           fesis = TRUE, iis=FALSE, t.pval=0.001)

is1.bas.noctr_iis <- isatpanel(data = basqrel.comp.reg[basqrel.comp.reg$regionname %in% "Basque Country (Pais Vasco)",],
                               formula = lgdpcap ~   linvest,
                               index = c("regionname","year"),
                               effect = "none",
                               fesis = FALSE, iis=TRUE, t.pval=0.01)

#dat_res <- basqrel.comp.reg[basqrel.comp.reg$regionname %in% "Basque Country (Pais Vasco)",]
is1.bas.noctr_iis <- isat(basq_lgdp, mxreg=basq_linv, iis=TRUE, mc=TRUE, t.pval=0.05, sis=FALSE, max.block.size = 15)
is1.bas.noctr_iis
plot(is1.bas.noctr_iis)


pdf("./generated figures/time_series_v1.pdf", height=6, width=5)
plot(is1.bas.noctr)
dev.off()


isnames <- gsub('[.]', '', is1.bas.noctr$isatpanel.result$ISnames)
isnames_form <- paste("`", isnames, "`", sep="")
reldat <- is1.bas.noctr$finaldata
names(reldat)[which(names(reldat)=="y")] <- "lgdpcap"
names(reldat)[6:NCOL(reldat)] <- isnames
fixform <- as.formula( paste("lgdpcap ~ linvest +",  paste(isnames_form, collapse=" + ")))

####re-estimate final model using fixest
library(fixest)
fx1.bas.noctr <- feols(fixform, data= reldat)
summary(fx1.bas.noctr)

##### re-estimate second time series model using fixest




basqrel.comp.reg$const <- 1
#############################
###with control units
is1.bas <- isatpanel(data = basqrel.comp.reg,
                     formula = lgdpcap ~   linvest,
                     index = c("regionname","year"),
                     effect = "twoways",
                     fesis = TRUE, iis=FALSE, t.pval=0.001, max.block.size=15)
plot(is1.bas)

is1.bas_01 <- isatpanel(data = basqrel.comp.reg,
                        formula = lgdpcap ~   linvest,
                        index = c("regionname","year"),
                        effect = "twoways",
                        fesis = TRUE, iis=FALSE, t.pval=0.01, max.block.size=15)
plot(is1.bas_01)



pdf("./generated figures/two_unit_sis_v1.pdf", height=6, width=8)
plot(is1.bas)
dev.off()

isatdates(is1.bas$isatpanel.result)

#########################################################
####try it with time-varying treatment effects (i.e. IIS)

is1.bas_iis <- isatpanel(data = basqrel.comp.reg,
                         formula = lgdpcap ~   linvest + const,
                         index = c("regionname","year"),
                         effect = "twoways",
                         fesis = FALSE, iis=TRUE, t.pval=0.05, max.block.size=15)
plot(is1.bas_iis)


is1.bas_iis_025 <- isatpanel(data = basqrel.comp.reg,
                             formula = lgdpcap ~   linvest + const,
                             index = c("regionname","year"),
                             effect = "twoways",
                             fesis = FALSE, iis=TRUE, t.pval=0.025, max.block.size=15)
plot(is1.bas_iis_025)

is1.bas_iis_01 <- isatpanel(data = basqrel.comp.reg,
                            formula = lgdpcap ~   linvest + const,
                            index = c("regionname","year"),
                            effect = "twoways",
                            fesis = FALSE, iis=TRUE, t.pval=0.01, max.block.size=15)
plot(is1.bas_iis_01)



pdf("./generated figures/two_unit_iis_v1.pdf", height=6, width=8)
plot(is1.bas_iis)
dev.off()

iis_dates <- isatdates(is1.bas_iis$isatpanel.result)
iis_years <- basqrel.comp.reg$year[iis_dates$iis$index]

iis_coef <- is1.bas_iis$isatpanel.result$coefficients[is1.bas_iis$isatpanel.result$ISnames]
iis_att <- mean(iis_coef[3:length(iis_coef)])
iis_att_se <- sqrt(sum(iis_dates$iis$coef.se^2)/(length(iis_coef[3:length(iis_coef)])))

is1.bas_iis$isatpanel.result$vcov.mean[is1.bas_iis$isatpanel.result$ISnames, is1.bas_iis$isatpanel.result$ISnames]

x1 <- as.vector(is1.bas_iis$finaldata$iis16)
x2 <- as.vector(is1.bas_iis$finaldata$iis17)
t(x1) %*% x2


####### Map All IIS Models ################
####### Map to Latex using Fixest
isnames <- gsub('[.]', '', is1.bas_iis$isatpanel.result$ISnames)

reldat <- is1.bas_iis$finaldata
names(reldat)[which(names(reldat)=="y")] <- "lgdpcap"

isnames_re <- paste("bas_", years[as.numeric(substring(isnames, 4))], sep="")
names(reldat)[6:NCOL(reldat)] <- isnames_re
isnames_form <- paste("`", isnames_re, "`", sep="")
fixform_iis <- as.formula( paste("lgdpcap ~ linvest +",  paste(isnames_form, collapse=" + "), " | id + time" ))

####re-estimate final model using fixest
library(fixest)
fx1.bas_iis <- feols(fixform_iis, data= reldat)
summary(fx1.bas_iis, se="standard")

############### 025
isnames <- gsub('[.]', '', is1.bas_iis_025$isatpanel.result$ISnames)

reldat <- is1.bas_iis_025$finaldata
names(reldat)[which(names(reldat)=="y")] <- "lgdpcap"

isnames_re <- paste("bas_", years[as.numeric(substring(isnames, 4))], sep="")
names(reldat)[6:NCOL(reldat)] <- isnames_re
isnames_form <- paste("`", isnames_re, "`", sep="")
fixform_iis <- as.formula( paste("lgdpcap ~ linvest +",  paste(isnames_form, collapse=" + "), " | id + time" ))

####re-estimate final model using fixest
library(fixest)
fx1.bas_iis_025 <- feols(fixform_iis, data= reldat)
summary(fx1.bas_iis_025, se="standard")


##############01
isnames <- gsub('[.]', '', is1.bas_iis_01$isatpanel.result$ISnames)

reldat <- is1.bas_iis_01$finaldata
names(reldat)[which(names(reldat)=="y")] <- "lgdpcap"

isnames_re <- paste("bas_", years[as.numeric(substring(isnames, 4))], sep="")
names(reldat)[6:NCOL(reldat)] <- isnames_re
isnames_form <- paste("`", isnames_re, "`", sep="")
fixform_iis <- as.formula( paste("lgdpcap ~ linvest +",  paste(isnames_form, collapse=" + "), " | id + time" ))

####re-estimate final model using fixest
library(fixest)
fx1.bas_iis_01 <- feols(fixform_iis, data= reldat)
summary(fx1.bas_iis_01, se="standard")




###compare to twfe:
library(fixest)
#basqrel.comp.reg$year <- basqrel.comp.reg$time
#install.packages("fixest")
basqrel.comp.reg$eta1979 <- 0
basqrel.comp.reg$eta1979[basqrel.comp.reg$year > 1978 & basqrel.comp.reg$regionname=="Basque Country (Pais Vasco)"] <- 1

basqrel.comp.reg_backup <- basqrel.comp.reg

names(basqrel.comp.reg)[names(basqrel.comp.reg)=="year"] <- "time"
names(basqrel.comp.reg)[names(basqrel.comp.reg)=="regionname"] <- "id"

fx1_known.bas <- feols(lgdpcap ~ linvest + eta1979 | id + time , data= basqrel.comp.reg)
summary(fx1_known.bas, se="standard")


#plm1.bas <- plm(lgdpcap ~  eta +  linvest, data=basqrel.comp.reg, effect="twoways", model="within", index=c("regionname","year"))
#summary(plm1.bas)

#basqrel.comp.reg <- basqrel.comp.reg_backup
###with time-varying effects
library(dummies)

eta1979_dums <- dummy(basqrel.comp.reg$eta1979 * basqrel.comp.reg$time)[,-1]

dums_names <- paste("bas_", seq(1979, 1995, 1), sep="")
colnames(eta1979_dums) <- dums_names

basqrel.comp.reg <- cbind(basqrel.comp.reg,eta1979_dums)



#paste("bas_", years[as.numeric(substring(isnames, 4))], sep="")

# basqrel.comp.reg$eta1979 <- 0
# basqrel.comp.reg$eta1979[basqrel.comp.reg$year > 1978 & basqrel.comp.reg$regionname=="Basque Country (Pais Vasco)"] <- 1

# basqrel.comp.reg_backup <- basqrel.comp.reg

names(basqrel.comp.reg)[names(basqrel.comp.reg)=="year"] <- "time"
names(basqrel.comp.reg)[names(basqrel.comp.reg)=="regionname"] <- "id"


dums_form <- paste("`", dums_names, "`", sep="")
fixform_dums <- as.formula( paste("lgdpcap ~ linvest +",  paste(dums_names, collapse=" + "), " | id + time" ))



fx1_known.bas_dums <- feols(fixform_dums, data= basqrel.comp.reg)
summary(fx1_known.bas_dums, se="standard")

sum_dums <- summary(fx1_known.bas_dums, se="standard")

### compute ATT
#relnames <- paste("bas_", colnames(basqrel.comp.reg$eta1979_dums), sep="")
att_dums <- mean(sum_dums$coefficients[dums_names])
dums_att_se <- sqrt(sum(sum_dums$se[dums_names]^2)/(length(sum_dums$se[dums_names])))

#compute ATT over selected subset


att_dums_sub <- mean(sum_dums$coefficients[2:13])
dums_att_se_sub <- sqrt(sum(sum_dums$se[2:13]^2)/(length(sum_dums$se[2:13])))




#plm1.bas <- plm(lgdpcap ~  eta +  linvest, data=basqrel.comp.reg, effect="twoways", model="within", index=c("regionname","year"))
#summary(fx1_known.bas_dums)





#toLatex(is1.bas$isatpanel.result$)


############################### Map SIS Models to Fixest
####### Map to Latex using Fixest
isnames <- gsub('[.]', '', is1.bas$isatpanel.result$ISnames)
isnames_form <- paste("`", isnames, "`", sep="")
reldat <- is1.bas$finaldata
names(reldat)[which(names(reldat)=="y")] <- "lgdpcap"
names(reldat)[5:NCOL(reldat)] <- isnames
fixform <- as.formula( paste("lgdpcap ~ linvest +",  paste(isnames_form, collapse=" + "), " | id + time" ))

####re-estimate final model using fixest
library(fixest)
fx1.bas <- feols(fixform, data= reldat)
summary(fx1.bas)

###### second SIS model
isnames <- gsub('[.]', '', is1.bas_01$isatpanel.result$ISnames)
isnames_form <- paste("`", isnames, "`", sep="")
reldat <- is1.bas_01$finaldata
names(reldat)[which(names(reldat)=="y")] <- "lgdpcap"
names(reldat)[5:NCOL(reldat)] <- isnames
fixform <- as.formula( paste("lgdpcap ~ linvest +",  paste(isnames_form, collapse=" + "), " | id + time" ))

####re-estimate final model using fixest
library(fixest)
fx1.bas_01 <- feols(fixform, data= reldat)
summary(fx1.bas_01)


#etable(fx1.bas.noctr, fx1.bas, fx1_known.bas, se="standard",  tex = TRUE)


#
# is1.bas$finaldata
#
# toLatex.gets
# toLatex.arx

## can we plot the counter-factual for a few periods?

#Basque in 1986? What happened

#Basque in 1970: early onset of ETA?
#break in 1978: identifies lagged impact of terrorism, effect size of ~10% consistent with Abadie et al.
#







###### Compare to LASSO Implementation
#### list where break matrices are stored
dat.sub <- basqrel.comp.reg_backup

sispanxlist <- list()
nbreaks <- 1
Tsample <-length(unique(dat.sub$year))
N <- length(unique(dat.sub$regionname))
id <- unique(dat.sub$regionname)
###############################
###### Loop over number of variables allowed to break and create break matrix
################################

library(Matrix)

for (n in 1: nbreaks)
{
  #n <- 1

  breakvar <- 1
  mxbreakname <- "mxbreak1"

  sism <- gets::sim(Tsample)
  colnames(sism) <- paste("break", "t", 2:(NCOL(sism)+1)+min(dat.sub$year)-1, sep="")

  sist <- as.matrix(sism)

  ##############################
  ############## create break matrix depending on method
  #############################

  sistlist <- do.call("list", rep(list(sist), N))
  sispan <- as.matrix(bdiag(sistlist))

  ####### multiply break matrix by break variable

  sispanx <- 1*sispan

  #########################
  ######## name the break matrix
  ##############################

  cn <- colnames(sist)
  ids <- unique(id)
  index <- Tsample
  #if (var(mxbreak, na.rm = TRUE)==0){
  index <- Tsample-1
  # }
  idsn <- matrix(t(matrix(ids,length(ids), (index) )))
  cnn <- rep(cn, N)
  length(cnn)
  cnnp <- paste(cnn, "id", idsn, sep="")
  #NCOL(sispanx)
  colnames(sispanx) <- cnnp
  sispanxlist[[n]] <- sispanx


} ####looping over breaks n ends


############ merge here:
library(dummies)
#library(fastDummies)
regdat <- data.frame(cbind(dat.sub$lgdpcap, dat.sub$linvest))
names(regdat) <- c("lgdp", "linvest")
regdat <- cbind(regdat, dummy(dat.sub$regionname), dummy(dat.sub$year))

regdat <- data.frame(cbind(dat.sub$regionname, dat.sub$year, dat.sub$lgdpcap, dat.sub$linvest))
names(regdat) <- c("regionname","year","lgdp", "linvest")
regdat <- fastDummies::dummy_cols(regdat, select_columns = c("regionname","year"))
regdat <- regdat[,!names(regdat) %in% c("regionname","year")]

NCOL(regdat)
NCOL(sispanxlist[[1]])

dat.sub.m <- cbind(regdat, as.matrix( sispanxlist[[1]]))
dat.sub.reg <- dat.sub.m
dat.sub.reg$lgdp <- as.numeric(dat.sub.reg$lgdp)
dat.sub.reg_mx <- dat.sub.reg[,-(which(colnames(dat.sub.reg)=="lgdp"))]

library(glmnet)

mod_fit = glmnet(x=as.matrix(dat.sub.reg_mx), intercept=FALSE, y=as.matrix(dat.sub.reg$lgdp), family='gaussian', alpha=1,
                 penalty.factor=c(rep(0, ncol(regdat)), rep(1, ncol(dat.sub.reg_mx)-ncol(regdat))))
#mod_fit = glmnet(x=as.matrix(dat.sub.reg_mx), y=as.matrix(dat.sub.reg$lgdp), family='gaussian', alpha=0.5, penalty.factor=c(rep(0, 35), rep(1, 60)))


mod_cv <- cv.glmnet(x=as.matrix(dat.sub.reg_mx), intercept=FALSE, nfolds=10, y=as.matrix(dat.sub.reg$lgdp), family='gaussian', alpha=1,
                    penalty.factor=c(rep(0, ncol(regdat)), rep(1, ncol(dat.sub.reg_mx)-ncol(regdat))))
#mod_cv_elass <- cv.glmnet(x=as.matrix(dat.sub.reg_mx), nfolds=10, y=as.matrix(dat.sub.reg$lgdp), family='gaussian', alpha=0.5, penalty.factor=c(rep(0, 35), rep(1, 60)))




coef(mod_fit, mod_fit$lambda[2])
coef(mod_fit, mod_fit$lambda[3])
coef(mod_fit, s=0)

plot(mod_fit)


#
# plot(mod_fit)
# plot(mod_fit, xvar = "lambda", label = TRUE)

#plot(mod_cv_elass_nop)

# NCOL(dat.sub.reg)
#
# 77-29
# length(c(rep(0, 29), rep(1, 48)))
#
# as.matrix(dat.sub.reg)[,1:30]

mod_cv$lambda.1se
mod_cv$lambda.min ##minimum CV

#coef(mod_cv, mod_cv$lambda.1se)
coef_lass_cv <- coef(mod_cv, mod_cv$lambda.min)
coef_lass_cv <- coef(mod_cv, mod_cv$lambda.1se)


# coef_lass_cv_elass <- coef(mod_cv, mod_cv_elass$lambda.min)
# coef_lass_cv_elass_nop <- coef(mod_cv, mod_cv_elass$lambda.1se)


########### Adaptive Lasso

set.seed(12345)

# penalty.factor.1 <- rep(1, length(dat.sub.reg_mx)+1) # +1 for intercept
# names(penalty.factor.1) <- c("(Intercept)", names(dat.sub.reg_mx))

penalty.factor.1 <- rep(1, length(dat.sub.reg_mx))
names(penalty.factor.1) <- names(dat.sub.reg_mx)
penalty.factor.1[grepl("linvest|regionname|year_", names(penalty.factor.1))] <- 0

mod_fit_adap1 = glmnet(
  x = as.matrix(dat.sub.reg_mx),
  intercept = TRUE,
  y = as.matrix(dat.sub.reg$lgdp),
  family = 'gaussian',
  alpha = 1,
  penalty.factor = penalty.factor.1
)
mod_cv_adap1 <- cv.glmnet(
  x = as.matrix(dat.sub.reg_mx),
  intercept = TRUE,
  nfolds = 10,
  y = as.matrix(dat.sub.reg$lgdp),
  family = 'gaussian',
  alpha = 1,
  penalty.factor = penalty.factor.1
)

coef(mod_fit_adap1, mod_fit$lambda[1])


best_lass_coef <- as.numeric(coef(mod_cv_adap1, s = mod_cv_adap1$lambda.min))

penalty.factor.2 = 1 / abs(best_lass_coef)
names(penalty.factor.2) <- row.names(coef(mod_cv_adap1, s = mod_cv_adap1$lambda.min))
penalty.factor.2[grepl("(Intercept)|linvest|regionname|year_", names(penalty.factor.2))] <- 0
penalty.factor.2 <- penalty.factor.2[!names(penalty.factor.2) %in% "(Intercept)"]

mod_fit_adap2 = glmnet(
  x = as.matrix(dat.sub.reg_mx),
  y = as.matrix(dat.sub.reg$lgdp),
  family = 'gaussian',
  alpha = 1,
  penalty.factor = penalty.factor.2
)

coef(mod_fit_adap2, mod_fit_adap2$lambda[2])
mod_cv_adap2 <- cv.glmnet(
  x = as.matrix(dat.sub.reg_mx),
  intercept = TRUE,
  nfolds = 10,
  y = as.matrix(dat.sub.reg$lgdp),
  family = 'gaussian',
  alpha = 1,
  penalty.factor = penalty.factor.2
)
rel.coefs.adap2 <- coef(mod_fit_adap2, s = mod_cv_adap1$lambda.min)

####estimate with fixest
str(rel.coefs.adap2)

rownames(rel.coefs.adap2)   #[rel.coefs.adap2>0]

xnames <- names(dat.sub.reg_mx)

ret_cv <- which(rel.coefs.adap2!=0)
xret <- xnames[ret_cv-1]
xret.is <- xret[xret %in% cnnp]
xret.is.m <- dat.sub.reg_mx[,xret.is]

#unlist(xret.is)

xret.is_comma <- paste("`", as.vector(xret.is), "`", sep="")
xret.is_comma_p <- paste(xret.is_comma, collapse=" + ")
###################
fixform_lass <- as.formula( paste("lgdpcap ~ linvest +",  xret.is_comma_p, " | id + time" , sep=""))



dat.sub.lass <- cbind(dat.sub, dat.sub.m[,xret.is])
names(dat.sub.lass)[names(dat.sub.lass)=="year"] <- "time"
names(dat.sub.lass)[names(dat.sub.lass)=="regionname"] <- "id"

####re-estimate final model using fixest
library(fixest)
fx1.bas_lass <- feols(fixform_lass, data= dat.sub.lass)
summary(fx1.bas_lass)

yrorder <- c(seq(from=1965, 1995, by=1))
yrorder <- c("(Intercept)", "lgdp", "linvest", paste(yrorder), "eta")


# etable( fx1.bas, fx1.bas_lass,fx1_known.bas,
#        se="standard",  tex = TRUE, order=yrorder)


etable(fx1.bas, fx1.bas_01, fx1.bas_lass,  fx1_known.bas,
       se="standard",  tex = TRUE, order=yrorder)

etable(fx1.bas_iis,  fx1.bas_iis_025, fx1.bas_iis_01,  fx1_known.bas_dums,
       se="standard",  tex = TRUE, order=yrorder)


# etable(fx1.bas_iis,   fx1_known.bas_dums,  fx1.bas, fx1.bas_lass, fx1_known.bas,
#        se="standard",  tex = TRUE, order=yrorder)

###ts only:
etable(fx1.bas.noctr, se="standard",  tex = TRUE, order=yrorder)


########################################

###### Selective Inference Example

library(selectiveInference)
#install.packages("selectiveInference")

set.seed(43)
n = 50
p = 10
sigma = 1

x = matrix(rnorm(n*p),n,p)
x = scale(x,TRUE,TRUE)

beta = c(3,2,rep(0,p-2))
y = x%*%beta + sigma*rnorm(n)

# first run glmnet
gfit = glmnet(x,y,standardize=FALSE)

# extract coef for a given lambda; note the 1/n factor!
# (and we don't save the intercept term)
lambda = 10
beta = coef(gfit, x=x, y=y, s=lambda/n, exact=TRUE)[-1]



# compute fixed lambda p-values and selection intervals
out = fixedLassoInf(x,y,beta,lambda,sigma=sigma)
out

###compare to re-estimated
m_ols <- lm(y ~ x[,which(beta!=0)])
summary(m_ols)



#######################################
############# General Case #############



basque$const <- 1

basque$popdens

str(basque)

basqrel <- basque[,c("gdpcap", "const",  "invest",  "regionname", "year")]

length(unique(basqrel$regionname))

basqrel.comp <- na.omit(basqrel)

length(unique(basqrel.comp$regionname))

lg <- function(x)c(NA, x[1:(length(x)-1)])
library(plyr)

basqrel.comp <- ddply(basqrel.comp, ~regionname, transform, L1.gdpcap = lg(gdpcap))
basqrel.comp <- ddply(basqrel.comp, ~regionname, transform, L1.invest = lg(invest))

basqrel.comp$lgdpcap <- log(basqrel.comp$gdpcap)
basqrel.comp$linvest <- log(basqrel.comp$invest)

basqrel.comp$L1.lgdpcap <- log(basqrel.comp$L1.gdpcap)
basqrel.comp$L1.linvest <- log(basqrel.comp$L1.invest)

basqrel.comp$dlgdpcap <- basqrel.comp$lgdpcap - basqrel.comp$L1.lgdpcap
basqrel.comp$dlinvest <- basqrel.comp$linvest - basqrel.comp$L1.linvest

basqrel.comp <- na.omit(basqrel.comp)

####compute mean for each year
unique(basqrel.comp$regionname)

length(unique(basqrel.comp$regionname))
length(unique(basqrel.comp$year))


#unique(basqrel.comp$regionname)

basqrel.comp <- basqrel.comp[!basqrel.comp$regionname %in% c("Baleares (Islas)", "Canarias", "Spain (Espana)" ),]

#basqrel.comp <- basqrel.comp[basqrel.comp$regionname %in% c("Basque Country (Pais Vasco)", "Cataluna" ),]
#basqrel.comp <- basqrel.comp[basqrel.comp$regionname %in% c("Basque Country (Pais Vasco)", "Madrid (Comunidad De)" ),]
library(dummies)

reg_dum <- dummy(basqrel.comp$regionname)*basqrel.comp$year

15*31

length(unique(basqrel.comp$regionname))

idn <- seq(1: length(unique(basqrel.comp$regionname)))
colnames(reg_dum) <- paste("id_trend",idn, sep="")

basqrel.comp <- cbind(basqrel.comp, reg_dum)

yr_names <- colnames(reg_dum)

#basqrel.comp

basqrel.comp$meanlgdp <- NA
years <- unique(basqrel.comp$year)

18*31

for (i in 1:length(years)){
  # i <- 1
  rel.yr <- years[i]
  basqrel.comp$meanlgdp[basqrel.comp$year==rel.yr] <- mean(basqrel.comp$lgdp[basqrel.comp$year==rel.yr])
  basqrel.comp$meanlinvest[basqrel.comp$year==rel.yr] <- mean(basqrel.comp$linvest[basqrel.comp$year==rel.yr])


}
library(dummies)
provdum <- dummy(basqrel.comp$regionname)*basqrel.comp$meanlgdp
provdum_linvest <- dummy(basqrel.comp$regionname)*basqrel.comp$meanlinvest

colnames(provdum) <- paste("id_mean_lgdp",idn, sep="")
cce_names_lgdp <- colnames(provdum)

colnames(provdum_linvest) <- paste("id_mean_linvest",idn, sep="")
cce_names_linvest <- colnames(provdum_linvest)



basqrel.comp <- cbind(basqrel.comp[, c("lgdpcap" , "const" ,  "linvest", "regionname", "year")], provdum, provdum_linvest)



form_trends <- as.formula(paste("lgdpcap ~   linvest + ", paste(yr_names, collapse=" + ")  ,sep=""))
form_cce <- as.formula(paste("lgdpcap ~   linvest + ", paste(cce_names_lgdp, collapse=" + "), "+", paste(cce_names_linvest, collapse=" + ")  ,sep=""))


form <-  as.formula(paste("lgdpcap ~   linvest"))
# form <- form_trends
# form <- form_cce


is1.bas.ar <- isatpanel(data = basqrel.comp,
                        formula = lgdpcap ~  linvest,
                        index = c("regionname","year"), ar=1,
                        effect = "twoways",
                        fesis = TRUE, iis=TRUE, t.pval=0.0001)
plot(is1.bas.ar)

is1.bas_all <- isatpanel(data = basqrel.comp,
                         formula = form,
                         index = c("regionname","year"),
                         effect = "twoways",
                         fesis = TRUE, iis=TRUE, t.pval=0.0001)

plot(is1.bas_all)


pdf("./generated figures/multi_panel_v1.pdf", height=11, width=9)
plot(is1.bas_all)
dev.off()


##################
### just using IIS
is1.bas_all_iis <- isatpanel(data = basqrel.comp,
                             formula = form,
                             index = c("regionname","year"),
                             effect = "twoways",
                             fesis = FALSE, iis=TRUE, t.pval=0.01)

plot(is1.bas_all_iis)

###############


# is1.bas <- isatpanel(data = basqrel.comp,
#                      formula = lgdpcap ~   linvest,
#                      index = c("regionname","year"),
#                      effect = "twoways",
#                      fesis = TRUE, iis=TRUE, t.pval=0.001)
#
# plot(is1.bas)

####### Map to Latex using Fixest
isnames <- gsub('[.]', '', is1.bas_all$isatpanel.result$ISnames)
isnames_form <- paste("`", isnames, "`", sep="")
reldat <- is1.bas_all$finaldata
names(reldat)[which(names(reldat)=="y")] <- "lgdpcap"
names(reldat)[5:NCOL(reldat)] <- isnames
fixform <- as.formula( paste("lgdpcap ~ linvest +",  paste(isnames_form, collapse=" + "), " | id + time" ))

####re-estimate final model using fixest
library(fixest)
fx1.bas_all <- feols(fixform, data= reldat)
summary(fx1.bas_all)

reldat$basque_1979 <- 0
reldat$basque_1979[reldat$id=="BasqueCountry(PaisVasco)" &  reldat$time > 1978] <- 1

fixform_known <- as.formula( paste("lgdpcap ~ linvest + basque_1979 | id + time" ))

fx1_known.bas_all <- feols(fixform_known, data= reldat)
summary(fx1_known.bas_all)



etable(fx1.bas_all, fx1_known.bas_all, se="standard",  tex = TRUE)

reldat[reldat$iis311==1,]

