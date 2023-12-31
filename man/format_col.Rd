% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function.R
\name{format_col}
\alias{format_col}
\title{format_col}
\usage{
format_col(line_nb, envir = .GlobalEnv)
}
\arguments{
\item{line_nb}{The line number of the data to be checked for updates on the data_default reference table.}
}
\value{
Does not return anything transform s the data inplace.
}
\description{
Given the line number of the corresponding data in data_default and the environment in which the data must be changed
will set the data types to data types found data_default$var_type. (!!! The data frame to be changed must already be present in the given environment !!!)
Meant to be used for automatic formatting when data is downloaded.
}
