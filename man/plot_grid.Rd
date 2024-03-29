% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_grid.R
\name{plot_grid}
\alias{plot_grid}
\title{Plotting an isatpanel object}
\usage{
plot_grid(x, title = NULL, regex_exclude_indicators = NULL, ...)
}
\arguments{
\item{x}{An object produced by the isatpanel function}

\item{title}{Plot title. Must be a character vector.}

\item{regex_exclude_indicators}{A regex character vector to exclude the inclusion of certain indicators in the plot. Default = NULL. Use with care, experimental.}

\item{...}{Further arguments to be passed to ggplot2.}
}
\value{
A ggplot2 plot that plots an 'isatpanel' object and shows all indicators as a grid to give a good and quick overview.
}
\description{
Plotting an isatpanel object
}
\examples{
\donttest{
data(EU_emissions_road)

# Group specification
EU15 <- c("Austria", "Germany", "Denmark", "Spain", "Finland", "Belgium",
         "France", "United Kingdom", "Ireland", "Italy", "Luxembourg",
         "Netherlands", "Greece", "Portugal", "Sweden")

# Prepare sample and data
EU_emissions_road_short <- EU_emissions_road[
EU_emissions_road$country \%in\% EU15 &
EU_emissions_road$year >= 2000,
]

# Run
result <- isatpanel(
  data = EU_emissions_road_short,
  formula = ltransport.emissions ~ lgdp + I(lgdp^2) + lpop,
  index = c("country", "year"),
  effect = "twoways",
  fesis = TRUE,
  plot = FALSE,
  t.pval = 0.01
)
plot(result)
plot_grid(result)
}
}
