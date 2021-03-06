% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.isatpanel.R
\name{plot.isatpanel}
\alias{plot.isatpanel}
\title{Plotting an isatpanel object}
\usage{
\method{plot}{isatpanel}(
  isatpanelobject,
  max.id.facet = 16,
  facet.scales = "free",
  title = "Panel Saturation",
  ...
)
}
\arguments{
\item{isatpanelobject}{An object produced by the isatpanel function}

\item{max.id.facet}{The resulting plot will be faceted for each individual in the panel. Beyond a certain number, this might result in unreadable figures. Default set at 16.}

\item{facet.scales}{To be passed to ggplot2::facet_wrap. Default is "free" (i.e. a separate y axis for each panel group/id). Alternatives are: "fixed", "fixed_y", and "fixed_x".}

\item{title}{Plot title. Must be a character vector.}

\item{...}{Further arguments to be passed to ggplot2.}
}
\value{

}
\description{
Plotting an isatpanel object
}
