% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ceda_utils.R
\name{summarise_by_extension}
\alias{summarise_by_extension}
\title{Summarise Flight Folder by extension}
\usage{
summarise_by_extension(flightFolder)
}
\arguments{
\item{flightFolder}{a subFolder in the flightDataCheck list}
}
\description{
Sometimes the same file is listed several times as different types. This
groups those files and presents their exentsions comma separated after their 
name. USed for tidy printing in print_flight_data
}
