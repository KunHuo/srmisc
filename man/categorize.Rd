% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-factor.R
\name{categorize}
\alias{categorize}
\title{categorize}
\usage{
categorize(
  data,
  varnames = ".auto",
  exclude = NULL,
  min = 2,
  max = 5,
  na.include = FALSE,
  na.level = "Missing",
  ...
)
}
\arguments{
\item{data}{a data frame.}

\item{varnames}{variable names.}

\item{exclude}{exclude variable names.}

\item{min}{usually taking a min number of distinct values only when varnames = ".auto". Default 2.}

\item{max}{usually taking a max number of distinct values only when varnames = ".auto". Default 5.}

\item{na.include}{whether to include NA values in the table.}

\item{na.level}{level to use for missing values.}

\item{...}{unused.}
}
\value{
a data frame.
}
\description{
categorize
}
