
# test_that("Complicated example making sure that isatpanel and fixest are producing the same results",{
#
#   skip_on_cran()
#   data("EUCO2residential")
#   data <- EUCO2residential
#   data$lgdp_sq <- data$lgdp^2
#
#   data$agg.directem_pc <- data$agg.directem/data$pop
#   data$lagg.directem_pc <- log(data$agg.directem_pc)
#   # data %>%
#   #   group_by(country) %>%
#   #   mutate(L1.lagg.directem_pc = lag(lagg.directem_pc)) %>%
#   #   ungroup -> data
#
#
#
#
#   # Group specification
#   EU15 <- c("Austria", "Germany", "Denmark", "Spain", "Finland", "Belgium",
#             "France", "United Kingdom", "Ireland", "Italy", "Luxembourg",
#             "Netherlands", "Greece", "Portugal", "Sweden")
#   EU16 <- c("Croatia", "Bulgaria", "Cyprus","Czech Republic", "Estonia",
#             "Hungary", "Lithuania", "Latvia", "Poland", "Romania",
#             "Slovenia", "Switzerland", "Slovak Republic", "Malta", #"Iceland",
#             "Norway")
#   EU31 <- c(EU15, EU16)
#
#   # Heterogenous effects preparation
#
#   base.int <- c("lgdp_EU15", "lgdp_EU16", "lgdpsq_EU15", "lgdpsq_EU16",
#                 "lhdd_EU15", "lhdd_EU16", "lcdd_EU15", "lcdd_EU16",
#                 "urban_EU15", "urban_EU16", "av.rate_EU15", "av.rate_EU16")
#
#   #
#   #   data$lgdp_EU15 <- data$lgdp * (data$country %in% EU15)
#   #   data$lgdp_EU16 <- data$lgdp * (data$country %in% EU16)
#   #   data$lgdpsq_EU15 <- data$lgdp_sq * (data$country %in% EU15)
#   #   data$lgdpsq_EU16 <- data$lgdp_sq * (data$country %in% EU16)
#   #   data$lhdd_EU15 <- data$lhdd * (data$country %in% EU15)
#   #   data$lhdd_EU16 <- data$lhdd * (data$country %in% EU16)
#   #   #data$lcdd_EU15 <- data$lcdd * (data$country %in% EU15)
#   #   #data$lcdd_EU16 <- data$lcdd * (data$country %in% EU16)
#   #   data$av.rate_EU15 <- data$av.rate * (data$country %in% EU15)
#   #   data$av.rate_EU16 <- data$av.rate * (data$country %in% EU16)
#   #   data$urban_EU15 <- data$urban * (data$country %in% EU15)
#   #   data$urban_EU16 <- data$urban * (data$country %in% EU16)
#
#
#   dv.name <- "log(agg. direct emissions p.c.)"
#   group <- 1
#
#   # Prepare sample and data
#   sample <- list(EU15, EU31)[[group]]
#   dat <- data[data$country %in% sample & data$year >= 2000,]
#
#
#
#   p.value <- 0.01
#
#   # IIS and FESIS
#   start <- Sys.time()
#   a1 <- isatpanel(
#     data = dat,
#     formula = ifelse(
#       group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban "),
#       paste0(
#         "lagg.directem_pc ~ ",
#         paste(base.int, collapse = " + ")
#       )
#     ) %>% as.formula,
#     index = c("country", "year"),
#     effect = "twoways",
#     iis = TRUE,
#     fesis = TRUE,
#     #engine = "fixest",
#     plot = FALSE,
#     t.pval = p.value,
#     print.searchinfo = FALSE
#   )
#   end <- Sys.time()
#   end - start
#
#
#   start <- Sys.time()
#   a1 <- isatpanel(
#     data = dat,
#     formula = ifelse(
#       group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban "),
#       paste0(
#         "lagg.directem_pc ~ ",
#         paste(base.int, collapse = " + ")
#       )
#     ) %>% as.formula,
#     index = c("country", "year"),
#     effect = "twoways",
#     iis = TRUE,
#     fesis = TRUE,
#     #engine = "fixest",
#     plot = FALSE,
#     t.pval = p.value,
#     turbo = TRUE,
#     print.searchinfo = FALSE
#   )
#   end <- Sys.time()
#   end - start
#
#   b1 <- isatpanel(
#     data = dat,
#     formula = ifelse(
#       group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban"),
#       paste0(
#         "lagg.directem_pc ~ ",
#         paste(base.int, collapse = " + ")
#       )
#     ) %>% as.formula,
#     index = c("country", "year"),
#     effect = "twoways",
#     iis = TRUE,
#     fesis = TRUE,
#     engine = "fixest",
#     plot = FALSE,
#     t.pval = p.value,
#     print.searchinfo = FALSE
#   )
#
#
#   expect_identical(a1$isatpanel.result$ISnames, b1$isatpanel.result$ISnames[!is.na(coef(b1$isatpanel.result)[b1$isatpanel.result$ISnames])])
#
#
#   # IIS and FESIS with perfectly linear term
#
#   a2 <- isatpanel(
#     data = dat,
#     formula = ifelse(
#       group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban + av.rate"),
#       paste0(
#         "lagg.directem_pc ~ ",
#         paste(base.int, collapse = " + ")
#       )
#     ) %>% as.formula,
#     index = c("country", "year"),
#     effect = "twoways",
#     iis = TRUE,
#     fesis = TRUE,
#     #engine = "fixest",
#     plot = FALSE,
#     t.pval = p.value,
#     print.searchinfo = FALSE
#   )
#
#
#   b2 <- isatpanel(
#     data = dat,
#     formula = ifelse(
#       group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban + av.rate"),
#       paste0(
#         "lagg.directem_pc ~ ",
#         paste(base.int, collapse = " + ")
#       )
#     ) %>% as.formula,
#     index = c("country", "year"),
#     effect = "twoways",
#     iis = TRUE,
#     fesis = TRUE,
#     engine = "fixest",
#     plot = FALSE,
#     t.pval=p.value,
#     print.searchinfo = FALSE
#   )
#
#   expect_identical(a2$isatpanel.result$ISnames, b2$isatpanel.result$ISnames[!is.na(coef(b2$isatpanel.result)[b2$isatpanel.result$ISnames])])
#
#
#   # Without IIS
#
#   a3 <- isatpanel(
#     data = dat,
#     formula = ifelse(
#       group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban + av.rate"),
#       paste0(
#         "lagg.directem_pc ~ ",
#         paste(base.int, collapse = " + ")
#       )
#     ) %>% as.formula,
#     index = c("country", "year"),
#     effect = "twoways",
#     iis = FALSE,
#     fesis = TRUE,
#     #engine = "fixest",
#     plot = FALSE,
#     t.pval=p.value,
#     print.searchinfo = FALSE
#   )
#
#
#   b3 <- isatpanel(
#     data = dat,
#     formula = ifelse(
#       group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban + av.rate"),
#       paste0(
#         "lagg.directem_pc ~ ",
#         paste(base.int, collapse = " + ")
#       )
#     ) %>% as.formula,
#     index = c("country", "year"),
#     effect = "twoways",
#     iis = FALSE,
#     fesis = TRUE,
#     engine = "fixest",
#     plot = FALSE,
#     t.pval=p.value,
#     print.searchinfo = FALSE
#   )
#
#   expect_identical(a3$isatpanel.result$ISnames, b3$isatpanel.result$ISnames[!is.na(coef(b3$isatpanel.result)[b3$isatpanel.result$ISnames])])
#
#
#   # #Testing that the names of the breaks are correct
#   # a5 <- isatpanel(
#   #   data = dat %>% slice(1:100),
#   #   formula = ifelse(
#   #     group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban"),
#   #     paste0(
#   #       "lagg.directem_pc ~ ",
#   #       paste(base.int, collapse = " + ")
#   #     )
#   #   ) %>% as.formula,
#   #   index = c("country", "year"),
#   #   effect = "twoways",
#   #   iis = F,
#   #   fesis = TRUE,
#   #   #engine = "fixest",
#   #   plot = TRUE,
#   #   t.pval=p.value
#   # )
#
#   # gsub("fesis","",a$isatpanel.result$ISnames)
#   #
#   # a$finaldata %>%
#   #   select(id, time, contains("fesis")) %>%
#   #   as_tibble() %>%
#   #   mutate(across(.cols = c(contains("fesis")),.fns = ~case_when(. == 0 ~ NA_real_,
#   #                                                                . == 1 ~ 1))) %>%
#   #   filter(across(-c(id,time), ~ all_vars(. == 0)))
#   # filter()
#
#
#
# })

test_that("Check the turbo and parallel options",{

  skip_on_cran()
  library(gets)
  options(print.searchinfo = FALSE)

  data("EUCO2residential")
  data <- EUCO2residential
  data$lgdp_sq <- data$lgdp^2

  data$agg.directem_pc <- data$agg.directem/data$pop
  data$lagg.directem_pc <- log(data$agg.directem_pc)
  # data %>%
  #   group_by(country) %>%
  #   mutate(L1.lagg.directem_pc = lag(lagg.directem_pc)) %>%
  #   ungroup -> data




  # Group specification
  EU15 <- c("Austria", "Germany", "Denmark", "Spain", "Finland", "Belgium",
            "France", "United Kingdom", "Ireland", "Italy", "Luxembourg",
            "Netherlands", "Greece", "Portugal", "Sweden")
  EU16 <- c("Croatia", "Bulgaria", "Cyprus","Czech Republic", "Estonia",
            "Hungary", "Lithuania", "Latvia", "Poland", "Romania",
            "Slovenia", "Switzerland", "Slovak Republic", "Malta", #"Iceland",
            "Norway")
  EU31 <- c(EU15, EU16)

  # Heterogenous effects preparation

  base.int <- c("lgdp_EU15", "lgdp_EU16", "lgdpsq_EU15", "lgdpsq_EU16",
                "lhdd_EU15", "lhdd_EU16", "lcdd_EU15", "lcdd_EU16",
                "urban_EU15", "urban_EU16", "av.rate_EU15", "av.rate_EU16")

  #
  #   data$lgdp_EU15 <- data$lgdp * (data$country %in% EU15)
  #   data$lgdp_EU16 <- data$lgdp * (data$country %in% EU16)
  #   data$lgdpsq_EU15 <- data$lgdp_sq * (data$country %in% EU15)
  #   data$lgdpsq_EU16 <- data$lgdp_sq * (data$country %in% EU16)
  #   data$lhdd_EU15 <- data$lhdd * (data$country %in% EU15)
  #   data$lhdd_EU16 <- data$lhdd * (data$country %in% EU16)
  #   #data$lcdd_EU15 <- data$lcdd * (data$country %in% EU15)
  #   #data$lcdd_EU16 <- data$lcdd * (data$country %in% EU16)
  #   data$av.rate_EU15 <- data$av.rate * (data$country %in% EU15)
  #   data$av.rate_EU16 <- data$av.rate * (data$country %in% EU16)
  #   data$urban_EU15 <- data$urban * (data$country %in% EU15)
  #   data$urban_EU16 <- data$urban * (data$country %in% EU16)


  dv.name <- "log(agg. direct emissions p.c.)"
  group <- 1

  # Prepare sample and data
  sample <- list(EU15, EU31)[[group]]
  dat <- data[data$country %in% sample & data$year >= 2000,]

  p.value <- 0.01


  a3.default <- isatpanel(
    data = dat,
    formula = ifelse(
      group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban + av.rate"),
      paste0(
        "lagg.directem_pc ~ ",
        paste(base.int, collapse = " + ")
      )
    ) %>% as.formula,
    index = c("country", "year"),
    effect = "twoways",
    iis = FALSE,
    fesis = TRUE,
    #engine = "fixest",
    plot = FALSE,
    t.pval=p.value,
    print.searchinfo = FALSE
  )

  a3.turbo <- isatpanel(
    data = dat,
    formula = ifelse(
      group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban + av.rate"),
      paste0(
        "lagg.directem_pc ~ ",
        paste(base.int, collapse = " + ")
      )
    ) %>% as.formula,
    index = c("country", "year"),
    effect = "twoways",
    iis = FALSE,
    fesis = TRUE,
    #engine = "fixest",
    plot = FALSE,
    t.pval=p.value,
    turbo = TRUE,
    print.searchinfo = FALSE
  )

  a3.parallel <- isatpanel(
    data = dat,
    formula = ifelse(
      group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban + av.rate"),
      paste0(
        "lagg.directem_pc ~ ",
        paste(base.int, collapse = " + ")
      )
    ) %>% as.formula,
    index = c("country", "year"),
    effect = "twoways",
    iis = FALSE,
    fesis = TRUE,
    #engine = "fixest",
    plot = FALSE,
    t.pval=p.value,
    parallel.options = 2,
    print.searchinfo = FALSE
  )


  a3.parallel.turbo <- isatpanel(
    data = dat,
    formula = ifelse(
      group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban + av.rate"),
      paste0(
        "lagg.directem_pc ~ ",
        paste(base.int, collapse = " + ")
      )
    ) %>% as.formula,
    index = c("country", "year"),
    effect = "twoways",
    iis = FALSE,
    fesis = TRUE,
    #engine = "fixest",
    plot = FALSE,
    t.pval=p.value,
    parallel.options = 2,
    turbo = TRUE,
    print.searchinfo = FALSE
  )
  expect_identical(a3.default$isatpanel.result$coefficients,
                   a3.turbo$isatpanel.result$coefficients,
                   a3.parallel$isatpanel.result$coefficients,
                   a3.parallel.turbo$isatpanel.result$coefficients)


  # with fixest

  b3.default <- isatpanel(
    data = dat,
    formula = ifelse(
      group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban + av.rate"),
      paste0(
        "lagg.directem_pc ~ ",
        paste(base.int, collapse = " + ")
      )
    ) %>% as.formula,
    index = c("country", "year"),
    effect = "twoways",
    iis = FALSE,
    fesis = TRUE,
    engine = "fixest",
    plot = FALSE,
    t.pval=p.value
  )

  b3.turbo <- isatpanel(
    data = dat,
    formula = ifelse(
      group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban + av.rate"),
      paste0(
        "lagg.directem_pc ~ ",
        paste(base.int, collapse = " + ")
      )
    ) %>% as.formula,
    index = c("country", "year"),
    effect = "twoways",
    iis = FALSE,
    fesis = TRUE,
    engine = "fixest",
    plot = FALSE,
    t.pval=p.value,
    turbo = TRUE,
    print.searchinfo = FALSE
  )

  # b3.parallel <- isatpanel(
  #   data = dat,
  #   formula = ifelse(
  #     group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban + av.rate"),
  #     paste0(
  #       "lagg.directem_pc ~ ",
  #       paste(base.int, collapse = " + ")
  #     )
  #   ) %>% as.formula,
  #   index = c("country", "year"),
  #   effect = "twoways",
  #   iis = FALSE,
  #   fesis = TRUE,
  #   engine = "fixest",
  #   plot = FALSE,
  #   t.pval=p.value,
  #   parallel.options = 2,
  #   print.searchinfo = FALSE
  # )


  # b3.parallel.turbo <- isatpanel(
  #   data = dat,
  #   formula = ifelse(
  #     group == 1, paste0("lagg.directem_pc ~ lgdp + lgdp_sq + lhdd + lcdd + urban + av.rate"),
  #     paste0(
  #       "lagg.directem_pc ~ ",
  #       paste(base.int, collapse = " + ")
  #     )
  #   ) %>% as.formula,
  #   index = c("country", "year"),
  #   effect = "twoways",
  #   iis = FALSE,
  #   fesis = TRUE,
  #   engine = "fixest",
  #   plot = FALSE,
  #   t.pval=p.value,
  #   parallel.options = 2,
  #   turbo = TRUE,
  #   print.searchinfo = FALSE
  # )
  expect_identical(b3.default$isatpanel.result$coefficients,
                   b3.turbo$isatpanel.result$coefficients,
                   b3.parallel$isatpanel.result$coefficients)
  #b3.parallel.turbo$isatpanel.result$coefficients)


})

