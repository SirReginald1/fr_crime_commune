% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function.R
\name{get_data}
\alias{get_data}
\title{get_data}
\usage{
get_data(url, file_name, API_function = "no", token = "no")
}
\arguments{
\item{url}{The url from which the data is to be downloaded.}

\item{file_name}{The name for the file to be saved as. Will be saved as rds file. Will be placed in "Data" file in current directory.
File name must end in ".rds".}

\item{API_function}{The name of the function used to make the appropriate API request. Function name in string format without parentheses.
The function must exist and return a list of with user message in first place and the data as second place. Defaults to "no".}

\item{token}{The token used to access the API. !!! not implemented for any API other than INSEE API yet !!!}
}
\value{
List containing [[1]] user message indicating download status and [[2]] data.
}
\description{
Downloads the data at the given url and saves it as the given file name to the Data directory in the current working directory.
There must be a Data directory present in the current working directory for the file to be saved.
Saves the file in rds format.
}
