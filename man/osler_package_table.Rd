% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osler_package_table.R
\name{osler_package_table}
\alias{osler_package_table}
\title{OSLER Package Table}
\usage{
osler_package_table(path = "https://oslerinhealth.org/oslerPackages",
  long = FALSE)
}
\arguments{
\item{path}{Path to the table of package}

\item{long}{Should the data be "long" (with respect to stable/current)}
}
\value{
\code{data.frame} of packages with commit IDs
}
\description{
Returns the table of OSLER packages
}
\note{
Package information is obtained from
\url{"https://oslerinhealth.org/oslerPackages"}
}
\examples{
osler_package_table()
}
