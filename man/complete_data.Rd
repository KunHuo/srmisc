% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/complete-data.R
\name{complete_data}
\alias{complete_data}
\title{Complete Data}
\usage{
complete_data(data, varnames = NULL, ...)
}
\arguments{
\item{data}{The input data frame.}

\item{varnames}{A character vector specifying the names of variables to
consider. If NULL, all variables in the data frame are considered.}

\item{...}{Additional arguments passed to other functions.}
}
\value{
A data frame with complete cases, i.e., rows with no missing values.
}
\description{
This function removes rows with missing values from the input data frame. It
can be applied to the entire data frame or to specific variables.
}
\details{
This function removes rows with missing values either from the entire data
frame or from specific variables specified by 'varnames'. If 'varnames' is
NULL, all variables in the data frame are considered for the removal of rows
with missing values. If 'varnames' is specified, only the specified variables
are considered.
}
\examples{
data <- data.frame(
  x1 = c(1, 2, NA, 4),
  x2 = c(NA, 2, 3, 4),
  x3 = c(1, NA, 3, 4)
)
complete_data(data)

complete_data(data, varnames = "x1")

complete_data(data, varnames = c("x1", "x2"))

}
