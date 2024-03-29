% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/within_plm.R
\name{Within_plm}
\alias{Within_plm}
\title{Use the within transformation from the plm package}
\usage{
Within_plm(df, effect = "twoways")
}
\arguments{
\item{df}{A data.frame object}

\item{effect}{The fixed effect specification. Values possible: "twoways" (default), "individual", "time", "nested"}
}
\value{
A data.frame object with the transformation complete
}
\description{
Use the within transformation from the plm package
}
