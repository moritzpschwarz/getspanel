library(here)

########### Analyse Simulation: compare p-values to 0.01 and 0.05 cut-offs

sum_list <- list()
sum_rej_05_list <- list()
sum_rej_01_list <- list()

load(here("data-raw","bootstrap_test", "simulations","spec_list.RData"))
spec_n <- NROW(specs)

missing <- 0

for (l in 1:spec_n){

  # l <- 1
  # res <- list.res[[l]]
  if(file.exists(here("data-raw","bootstrap_test","simulations",paste0(paste0(specs[l,],collapse = "_"),".RData")))){
    load(here("data-raw","bootstrap_test","simulations",paste0(paste0(specs[l,],collapse = "_"),".RData")))
  } else{
    print(paste0("File Specification ",l," skipped - not found."))
    missing <- missing + 1
    next
  }


  res.rej05 <- res[,2:NCOL(res)]
  res.rej05$rej <- NA
  res.rej05$rej.L2.boot <- NA
  res.rej05$rej.L1.boot <- NA
  res.rej05$rej.dist.boot <- NA

  res.rej05$rej.prop.test <- NA
  res.rej05$rej.prop.test.boot <- NA
  res.rej05$rej.prop.boot <- NA



  res.rej05$rej[res.rej05$is.dist1.p > 0.05] <- 999
  res.rej05$rej[res.rej05$is.dist1.p < 0.05] <- 1
  res.rej05$rej[res.rej05$rej > 1] <- 0

  res.rej05$rej.L2.boot[res.rej05$is.dist1.boot.L2.p > 0.05] <- 999
  res.rej05$rej.L2.boot[res.rej05$is.dist1.boot.L2.p < 0.05] <- 1
  res.rej05$rej.L2.boot[res.rej05$rej.L2.boot > 1] <- 0

  res.rej05$rej.L1.boot[res.rej05$is.dist1.boot.L1.p > 0.05] <- 999
  res.rej05$rej.L1.boot[res.rej05$is.dist1.boot.L1.p < 0.05] <- 1
  res.rej05$rej.L1.boot[res.rej05$rej.L1.boot > 1] <- 0

  res.rej05$rej.dist.boot[res.rej05$is.dist1.boot.dist.p > 0.05] <- 999
  res.rej05$rej.dist.boot[res.rej05$is.dist1.boot.dist.p < 0.05] <- 1
  res.rej05$rej.dist.boot[res.rej05$rej.dist.boot > 1] <- 0


  ####proportion test
  res.rej05$rej.prop.test[res.rej05$is.prop.test.p > 0.05] <- 999
  res.rej05$rej.prop.test[res.rej05$is.prop.test.p < 0.05] <- 1
  res.rej05$rej.prop.test[res.rej05$rej.prop.test > 1] <- 0

  res.rej05$rej.prop.test.boot[res.rej05$is.prop.boot.test.p > 0.05] <- 999
  res.rej05$rej.prop.test.boot[res.rej05$is.prop.boot.test.p < 0.05] <- 1
  res.rej05$rej.prop.test.boot[res.rej05$rej.prop.test.boot > 1] <- 0

  res.rej05$rej.prop.boot[res.rej05$is.prop.boot.p > 0.05] <- 999
  res.rej05$rej.prop.boot[res.rej05$is.prop.boot.p < 0.05] <- 1
  res.rej05$rej.prop.boot[res.rej05$rej.prop.boot > 1] <- 0





  res.rej05$id <- res$id
  res.rej05$id.seq <- res$id.seq


  sum_rej_05_list[[l]] <- colMeans(res.rej05, na.rm = TRUE)


  res.rej01 <- res[,2:NCOL(res)]
  res.rej01$rej <- NA
  res.rej01$rej.L2.boot <- NA
  res.rej01$rej.L1.boot <- NA
  res.rej01$rej.dist.boot <- NA

  res.rej01$rej.prop.test <- NA
  res.rej01$rej.prop.test.boot <- NA
  res.rej01$rej.prop.boot <- NA



  res.rej01$rej[res.rej01$is.dist1.p > 0.01] <- 999
  res.rej01$rej[res.rej01$is.dist1.p < 0.01] <- 1
  res.rej01$rej[res.rej01$rej > 1] <- 0

  res.rej01$rej.L2.boot[res.rej01$is.dist1.boot.L2.p > 0.01] <- 999
  res.rej01$rej.L2.boot[res.rej01$is.dist1.boot.L2.p < 0.01] <- 1
  res.rej01$rej.L2.boot[res.rej01$rej.L2.boot > 1] <- 0

  res.rej01$rej.L1.boot[res.rej01$is.dist1.boot.L1.p > 0.01] <- 999
  res.rej01$rej.L1.boot[res.rej01$is.dist1.boot.L1.p < 0.01] <- 1
  res.rej01$rej.L1.boot[res.rej01$rej.L1.boot > 1] <- 0

  res.rej01$rej.dist.boot[res.rej01$is.dist1.boot.dist.p > 0.01] <- 999
  res.rej01$rej.dist.boot[res.rej01$is.dist1.boot.dist.p < 0.01] <- 1
  res.rej01$rej.dist.boot[res.rej01$rej.dist.boot > 1] <- 0

  ###proportion test
  res.rej01$rej.prop.test[res.rej01$is.prop.test.p > 0.01] <- 999
  res.rej01$rej.prop.test[res.rej01$is.prop.test.p < 0.01] <- 1
  res.rej01$rej.prop.test[res.rej01$rej.prop.test > 1] <- 0

  res.rej01$rej.prop.test.boot[res.rej01$is.prop.boot.test.p > 0.01] <- 999
  res.rej01$rej.prop.test.boot[res.rej01$is.prop.boot.test.p < 0.01] <- 1
  res.rej01$rej.prop.test.boot[res.rej01$rej.prop.test.boot > 1] <- 0

  res.rej01$rej.prop.boot[res.rej01$is.prop.boot.p > 0.01] <- 999
  res.rej01$rej.prop.boot[res.rej01$is.prop.boot.p < 0.01] <- 1
  res.rej01$rej.prop.boot[res.rej01$rej.prop.boot > 1] <- 0





  res.rej01$id <- res$id
  res.rej01$id.seq <- res$id.seq

  sum_rej_01_list[[l]] <- colMeans(res.rej01, na.rm = TRUE)


}

print(paste0("Number of files still missing: ",missing))


sum.05 <- do.call(rbind.data.frame, sum_rej_05_list)
names(sum.05) <- names(sum_rej_05_list[[2]])
sum.05$test.lev <- 0.05

sum.01 <- do.call(rbind.data.frame, sum_rej_01_list)
names(sum.01) <- names(sum_rej_01_list[[2]])
sum.01$test.lev <- 0.01

###combine
sum.01.m <- merge(sum.01, specs, by="id")
sum.05.m <- merge(sum.05, specs, by="id")

sum_tab <- rbind(sum.01.m, sum.05.m)

sum_tab



library(tidyverse)

sum_tab %>%
  as_tibble %>%

ggplot(sum_tab, aes(x = sample, y = rej)) +
  geom_line()

