% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_faam_nitrates.R
\name{read_faam_nitrates}
\alias{read_faam_nitrates}
\title{Read FAAM Nitrates}
\usage{
read_faam_nitrates(
  filepath,
  startDate = NULL,
  endDate = NULL,
  averageNanoString = NULL
)
}
\arguments{
\item{filepath}{path to file}

\item{startDate}{string that can be coerced to a nanotime via: \code{nanotime::nanotime(startDate,format = "\%Y-\%m-\%d \%H:\%M:\%S")}
used to filter \code{tidync::hyper_array()}}

\item{endDate}{string that can be coerced to a nanotime via: \code{nanotime::nanotime(endDate,format = "\%Y-\%m-\%d \%H:\%M:\%S")}
used to filter \code{tidync::hyper_array()}}

\item{averageNanoString}{string to pass to \code{nanotime::nano_floor(date, nanotime::as.nanoduration(averageNanoString))} for resampling date
For example to resample to 10 Hz, argument is "00:00:00.1", for 1 hz it is "00:00:00"}
}
\description{
Reads faam core nitrates file
}
\author{
W. S. Drysdale
}
