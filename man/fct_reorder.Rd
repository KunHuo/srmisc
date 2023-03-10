% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-factor.R
\name{fct_reorder}
\alias{fct_reorder}
\alias{fct_reorder.factor}
\alias{fct_reorder.character}
\alias{fct_reorder.data.frame}
\title{Reorder factor levels}
\usage{
fct_reorder(x, ...)

\method{fct_reorder}{factor}(x, ..., exclude = NA)

\method{fct_reorder}{character}(x, ..., exclude = NA)

\method{fct_reorder}{data.frame}(x, ..., exclude = NA, varname = NULL)
}
\arguments{
\item{x}{a facotr (or character vector) or a data frame.}

\item{...}{the levels of factor.}

\item{exclude}{a vector of values to be excluded when forming the set of levels.
This may be factor with the same level set as x or should be a character.}

\item{varname}{a string of variable name.}
}
\value{
Return a factor when 'x' is a factor or character, otherwise return a
data frame when 'x' is a data frame.
}
\description{
Reorder factor levels
}
\examples{
# for character
f <- c("a", "b", "c", "d")
f
fct_reorder(f, "c", "b")

# for factor
f <- factor(f)
f
fct_reorder(f, "d", "b")

# for data.frame
fd <- data.frame(x = factor(c("a", "b", "c", "d")), y = 1:4)
fd$x
fd <- fct_reorder(fd, varname = "x", "b", "d")
fd$x
}
