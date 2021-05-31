library(tidyverse)
library(here)



rm(list = ls())
select <- dplyr::select


socio <- c("Mueller")
model<- c(
  "m2",
  "m2.isat",
  "am2",
  "am2.isat",
  "am2.isat_L1",
  "am2_L1",
  NULL
)


for(i in socio){
  for(j in model){
    #i = "Mueller"
    #j = "m2"

    print(paste("full",i,j,sep="_"))
    #print(length(list.files(here("data","temp","projections"),pattern = paste("full",i,j,sep="_"))))

    # Carry out the merging of the files
    indv_files <- list.files(here("data-raw","projections","projfiles"),pattern = paste(i,j,sep="_"),full.names = TRUE)
    indv_files <- indv_files[!grepl("massive",indv_files)]

    if(j == "m2"){indv_files <- indv_files[!grepl("m2.isat",indv_files)]}
    if(j == "m2"){indv_files <- indv_files[!grepl("am2",indv_files)]}
    if(j == "am2"){indv_files <- indv_files[!grepl("am2.isat|am2_L1",indv_files)]}
    if(j == "am2.isat"){indv_files <- indv_files[!grepl("am2.isat_L1",indv_files)]}

    massive_overall <- tibble()
    for(k in seq_along(indv_files)){
      print(k)
      load(indv_files[k])

      done %>%
        filter(year > 2089) %>%

        mutate(diff = gdp_cap_hundred_climate / gdp_cap_hundred) %>%
        #drop_na %>%

        group_by(iso,final_temp,realisation) %>%

        summarise(diff = mean(diff),.groups =  "drop") %>%
        ungroup %>%
        bind_rows(massive_overall,.) -> massive_overall

      rm(done)
    }
    save(massive_overall, file=here("data-raw","projections","projfiles",paste0(i,"_",j,"_massive_EOC.RData")))
  }
}


