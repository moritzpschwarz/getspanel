% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/felm_getspanel.R
\name{felmFun}
\alias{felmFun}
\title{Internal lfe/felm Estimation Method}
\usage{
felmFun(y, x, effect, time, id, cluster = "individual", ...)
}
\arguments{
\item{y}{dependent variable}

\item{x}{matrix of regressors}

\item{effect}{Fixed Effect specification}

\item{time}{Character vector of name of the time variable}

\item{id}{Character vector of the name of the group variable}

\item{cluster}{Character vector of the variable(s) to cluster Standard Errors at}

\item{...}{Further arguments to pass to gets::isat}
}
\value{
List to be used by gets::isat
}
\description{
Internal lfe/felm Estimation Method
}
