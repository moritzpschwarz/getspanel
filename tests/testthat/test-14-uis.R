data(EU_emissions_road)

# Group specification
EU15 <- c("Austria", "Germany", "Denmark", "Spain", "Finland", "Belgium",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg",
          "Netherlands", "Greece", "Portugal", "Sweden")

# Prepare sample and data
EU_emissions_road_short <- EU_emissions_road[
EU_emissions_road$country %in% EU15 &
 EU_emissions_road$year >= 2000,
 ]

test_that("UIS works", {
  # Run uis
  set.seed(188)
  ncol <- 20
  weird_uis <- matrix(0,nrow = nrow(EU_emissions_road_short), ncol = ncol) + matrix(rbinom(nrow(EU_emissions_road_short)*ncol, size = 1, prob = 0.1),
                                                                                    nrow = nrow(EU_emissions_road_short))
  test_uis <- weird_uis * EU_emissions_road_short$lgdp

  # make sure some are selected
  EU_emissions_road_short$ltransport.emissions <- EU_emissions_road_short$ltransport.emissions + 0.003 * test_uis[,c(5)]
  EU_emissions_road_short$ltransport.emissions <- EU_emissions_road_short$ltransport.emissions - 0.0006 * test_uis[,c(1)]

  result <- isatpanel(
    data = EU_emissions_road_short,
    formula = ltransport.emissions ~ lgdp + I(lgdp^2) + lpop,
    index = c("country", "year"),
    effect = "twoways",
    fesis = FALSE,
    plot = FALSE,
    t.pval = 0.1,
    uis = test_uis,
    print.searchinfo = FALSE)

  indics <- get_indicators(result)

  expect_identical(indics$uis_breaks$name, c("uis1", "uis1", "uis1", "uis1", "uis1", "uis1", "uis1", "uis1",
                                             "uis1", "uis1", "uis1", "uis1", "uis1", "uis5", "uis5", "uis5",
                                             "uis5", "uis5", "uis5", "uis5", "uis5", "uis5", "uis5", "uis5",
                                             "uis5", "uis5", "uis5"))

  expect_identical(round(indics$uis_breaks$coef,5), c(-0.00089, -0.00089, -0.00089, -0.00089, -0.00089, -0.00089,
                                             -0.00089, -0.00089, -0.00089, -0.00089, -0.00089, -0.00089, -0.00089,
                                             0.00317, 0.00317, 0.00317, 0.00317, 0.00317, 0.00317, 0.00317,
                                             0.00317, 0.00317, 0.00317, 0.00317, 0.00317, 0.00317, 0.00317
  ))

  # plot_indicators(result)
  #
  # plot_grid(result)
  # plot(result)






})


