% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_ADMS_config.R
\name{write_ADMS_config}
\alias{write_ADMS_config}
\title{Write ADMS Config}
\usage{
write_ADMS_config(config, path)
}
\arguments{
\item{config}{list that represents the ADMS config to write to disk. Structure can be made by reading a config with \code{read_ADMS_config()}}

\item{path}{path to ADMS config file}
}
\description{
Writes an ADMS config file from a nested list. Level 1 are the headers preceeded 
level 2 are the names of the key value pairs. Adapted from a python version 
found \href{https://github.com/irene146/adms6/blob/main/modify\%20_apl_files/modelconfig.py}{here}.
}
\details{
Best used after editing a working ADMS config file that has been loaded using \code{read_ADMS_config()}
}
