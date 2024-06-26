% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ceda_utils.R
\name{list_flight_data}
\alias{list_flight_data}
\title{List Flight Data}
\usage{
list_flight_data(flight, verbose = TRUE, force = FALSE)
}
\arguments{
\item{flight}{a character vector of FAAM flight numbers.}

\item{verbose}{Default true, should the available files be detailed in the console. When TRUE the data.frame is returned invisibly}

\item{force}{For performance previously queried flights are cached in an environment variable \code{the$ListofFlights}. Set true to force scraping CEDA.}
}
\description{
Checks for the presence of the core_raw, core_processed, non_core and mo_no_core folders, and the flight summary file.
Then lists the files in the folders. Returns information as a data.frame and when verbose == TRUE, prints the information to the console. 
When verbose is TRUE, the data.frame is returned invisibly.
}
\author{
W. S. Drysdale
}
