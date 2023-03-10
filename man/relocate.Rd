% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-reshape.R
\name{relocate}
\alias{relocate}
\title{Change column order}
\usage{
relocate(data, variables, before = NULL, after = NULL)
}
\arguments{
\item{data}{a data frame, data frame extension (e.g. a tibble), or a lazy data frame.}

\item{variables}{columns to move. It can be a variable name or an index in the data frame.}

\item{before}{destination of columns selected by variables. Supplying neither
will move columns to the left-hand side.}

\item{after}{destination of columns selected by variables. Supplying neither
will move columns to the right-hand side.}
}
\value{
A data frame.
}
\description{
Change column order
}
\examples{
head(mtcars)

relocate(mtcars, variables = "am", before = "cyl")

relocate(mtcars, variables = "am", before = 2)

relocate(mtcars, variables = "am", after = "mpg")

relocate(mtcars, variables = "am", after = 1)

relocate(mtcars, variables = c("am", "vs"), before = 1)
}
