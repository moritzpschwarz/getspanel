#' Internal function to identify the timing of selected indicators
#'
#' @param object data.frame
#'
#' @return A list of data.frames
#'
identify_indicator_timings <- function(object){

  varying_vars <- names(object)[!names(object)%in% c("id","time","y","fitted")]

  object_l <- reshape(object,
                  varying = varying_vars,
                  idvar = c("id","time"),
                  v.names = "value",
                  timevar = "name",
                  times = varying_vars,
                  direction = "long")

  # Impulses and Steps
  impulses <- object_l[grepl("iis",object_l$name) & object_l$value == 1,]
  steps <- object_l[grepl("sis",object_l$name) & object_l$value == 1 & !grepl("fesis", object_l$name) & !grepl("csis", object_l$name),]

  # FESIS
  if(any(grepl("^fesis",names(object)))){
    fesis_wide <- object[,grepl("^fesis", names(object)), drop = FALSE]
    fesis_l <- reshape(fesis_wide,
                       direction = "long",
                       varying = names(fesis_wide),
                       times = names(fesis_wide),
                       v.names = "value",
                       timevar = "name")


    split_list <- strsplit(x = fesis_l$name, split = "\\.")

    fesis_l$id <- unlist(lapply(split_list, `[[`, 1))
    fesis_l$id <- gsub("fesis","",fesis_l$id)
    fesis_l$time <- unlist(lapply(split_list, `[[`, 2))
    fesis_l$time <- as.numeric(fesis_l$time)

    fesis_l <- fesis_l[c("id","time","name")]

    fesis <- fesis_l[!duplicated(fesis_l),]

  } else {fesis <- NULL}

  # CFESIS
  if(any(grepl("cfesis",names(object)))){

    cfesis_wide <- object[,grepl("cfesis", names(object)), drop = FALSE]
    cfesis_l <- reshape(cfesis_wide,
                        direction = "long",
                        varying = names(cfesis_wide),
                        times = names(cfesis_wide),
                        v.names = "value",
                        timevar = "name")


    split_list <- strsplit(x = cfesis_l$name, split = "\\.")

    cfesis_l$name <- unlist(lapply(split_list, `[[`, 1))

    cfesis_l$id <- unlist(lapply(split_list, `[[`, 2))
    cfesis_l$id <- gsub("cfesis","",cfesis_l$id)

    cfesis_l$time <- unlist(lapply(split_list, `[[`, 3))
    cfesis_l$time <- as.numeric(cfesis_l$time)

    cfesis_l <- cfesis_l[c("id","time","name")]

    cfesis <- cfesis_l[!duplicated(cfesis_l),]

    # object %>%
    #   select(contains("cfesis")) %>%
    #   pivot_longer(cols = everything()) %>%
    #   separate(col = "name",sep = "\\.",into = c("variable","id","time")) %>%
    #   mutate(id = gsub("cfesis","",id),
    #                 time = as.numeric(time)) %>%
    #   select(-"value") %>%
    #   distinct(across(c("variable", "time", "id"))) -> cfesis
  } else {cfesis <- NULL}

  # CSIS

  if(any(grepl("csis",names(object)))){

    csis_wide <- object[,grepl("csis", names(object)), drop = FALSE]
    csis_l <- reshape(csis_wide,
                      direction = "long",
                      varying = names(csis_wide),
                      times = names(csis_wide),
                      v.names = "value",
                      timevar = "name")

    split_list <- strsplit(x = csis_l$name, split = "\\.")

    csis_l$name <- unlist(lapply(split_list, `[[`, 1))
    csis_l$time <- unlist(lapply(split_list, `[[`, 2))
    csis_l$time <- gsub("csis","",csis_l$time)
    csis_l$time <- as.numeric(csis_l$time)

    csis_l <- csis_l[c("time","name")]

    csis <- csis_l[!duplicated(csis_l),]

    # object %>%
    #   select(contains("cfesis")) %>%
    #   pivot_longer(cols = everything()) %>%
    #   separate(col = "name",sep = "\\.",into = c("variable","id","time")) %>%
    #   mutate(id = gsub("cfesis","",id),
    #                 time = as.numeric(time)) %>%
    #   select(-"value") %>%
    #   distinct(across(c("variable", "time", "id"))) -> cfesis
  } else {csis <- NULL}


  output <- list()
  output$impulses <- impulses
  output$steps <- steps
  output$csis <- csis
  output$fesis <- fesis
  output$cfesis <- cfesis

  return(output)

}
