
test_that("AR gives exact same result", {

  data(EU_emissions_road)

  # Group specification
  EU15 <- c("Austria", "Germany", "Denmark", "Spain", "Finland", "Belgium",
            "France", "United Kingdom", "Ireland", "Italy", "Luxembourg",
            "Netherlands", "Greece", "Portugal", "Sweden"
  )

  # Prepare sample and data
  EU_emissions_road_short <- EU_emissions_road[
    EU_emissions_road$country %in% EU15 &
      EU_emissions_road$year >= 2000,
  ]


  EU_emissions_road_short %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(ar1 = dplyr::lag(ltransport.emissions)) -> EU_emissions_road_short_ar


  result_builtinAR <- isatpanel(
    data = EU_emissions_road_short,
    formula = ltransport.emissions ~ lgdp + I(lgdp^2) + lpop,
    index = c("country", "year"),
    effect = "twoways",
    ar = 1,
    fesis = TRUE,
    plot = FALSE,
    t.pval = 0.01
  )


  result_presetAR <- isatpanel(
    data = EU_emissions_road_short_ar,
    formula = ltransport.emissions ~ ar1 + lgdp + I(lgdp^2) + lpop,
    index = c("country", "year"),
    effect = "twoways",
    ar = 0,
    fesis = TRUE,
    plot = FALSE,
    t.pval = 0.01
  )


  result_presetAR$isatpanel.result$time.finished <- NULL
  result_presetAR$isatpanel.result$time.started <- NULL
  result_presetAR$isatpanel.result$date <- NULL

  result_builtinAR$isatpanel.result$time.finished <- NULL
  result_builtinAR$isatpanel.result$time.started <- NULL
  result_builtinAR$isatpanel.result$date <- NULL

  expect_equal(result_presetAR$isatpanel.result, result_builtinAR$isatpanel.result)


})
