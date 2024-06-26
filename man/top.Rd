% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/top-value.R
\name{top}
\alias{top}
\title{Top values}
\usage{
top(data, varnames = NULL, n = 20, unique = FALSE)
}
\arguments{
\item{data}{The input data frame.}

\item{varnames}{A character vector specifying the names of variables to
consider. If NULL, all numeric variables in the data frame are considered.}

\item{n}{The number of top values to return.}

\item{unique}{Logical indicating whether to consider unique values only.
Default is FALSE.}
}
\value{
A list of data frames, where each data frame contains the top n
largest and smallest values for each numeric variable.
}
\description{
This function returns the top n largest and smallest values for each numeric
variable in the input data frame.
}
\details{
This function returns the top n largest and smallest values for each numeric
variable in the input data frame. The 'varnames' argument specifies the
variables to consider. If 'varnames' is NULL, all numeric variables in the
data frame are considered. The 'unique' argument, if set to TRUE, considers
only unique values when determining the top values.
}
