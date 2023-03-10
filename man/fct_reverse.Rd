% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-factor.R
\name{fct_reverse}
\alias{fct_reverse}
\alias{fct_reverse.factor}
\alias{fct_reverse.character}
\alias{fct_reverse.data.frame}
\title{Reverse order of factor levels}
\usage{
fct_reverse(x, ...)

\method{fct_reverse}{factor}(x, ...)

\method{fct_reverse}{character}(x, ...)

\method{fct_reverse}{data.frame}(x, varname = NULL, ...)
}
\arguments{
\item{x}{a facotr (or character vector) or a data frame.}

\item{...}{further arguments.}

\item{varname}{variable name contains in the data frame.}
}
\value{
Return a factor when 'x' is a factor or character, otherwise return a
data frame when 'x' is a data frame.
}
\description{
Reverse order of factor levels
}
\examples{
f <- factor(c("a", "b", "c"))
f
fct_reverse(f)
fd <- data.frame(x = factor(c("a", "b", "c")), y = 1:3)
fd$x
fd <- fct_reverse(fd, varname = "x")
fd$x
}
