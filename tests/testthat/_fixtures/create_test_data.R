# Prepare Data
set.seed(1230)
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

# Group specification
EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland", 
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg",
          "Netherlands", "Greece", "Portugal", "Sweden")

# Prepare sample and data
sample <- EU15
dat <- data[data$country %in% sample & data$year >= 1995, ]

# Run
result <- isatpanel(
  data = dat,
  formula = ltransport.emissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = TRUE,
  fesis = TRUE,
  tis = TRUE,
  csis = TRUE,
  cfesis = TRUE,
  t.pval = .05,
  print.searchinfo = FALSE,
  plot = FALSE,
)

saveRDS(result, file = "tests/testthat/_fixtures/isatpanel_with_all_indicators.rds")

dat[dat$country %in% c("Austria"), ]$country <- "iis"
dat[dat$country %in% c("Germany"), ]$country <- "fesis"

# Run
result <- isatpanel(
  data = dat,
  formula = ltransport.emissions_pc ~ lgdp + lgdp_sq + lpop,
  index = c("country", "year"),
  effect = "twoways",
  iis = TRUE,
  fesis = TRUE,
  tis = TRUE,
  csis = TRUE,
  cfesis = TRUE,
  t.pval = .05,
  print.searchinfo = FALSE,
  plot = FALSE,
)

saveRDS(result, file = "tests/testthat/_fixtures/isatpanel_with_problematic_names.rds")
