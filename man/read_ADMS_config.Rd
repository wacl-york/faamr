% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_ADMS_config.R
\name{read_ADMS_config}
\alias{read_ADMS_config}
\title{Read ADMS Config}
\usage{
read_ADMS_config(path)
}
\arguments{
\item{path}{path to ADMS config file}
}
\description{
Reads and ADMS config file into a nested list. Level 1 are the headers preceeded 
&, level 2 are the names of the key value pairs. Adapted from a python version 
found \href{https://github.com/irene146/adms6/blob/main/modify\%20_apl_files/modelconfig.py}{here}.
}
