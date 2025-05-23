% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global-parameters.R
\name{global_fontsize}
\alias{global_fontsize}
\title{Set or Get Global Font Size}
\usage{
global_fontsize(size)
}
\arguments{
\item{size}{An optional numeric value specifying the font size to set. If
not provided, the current global font size is returned.}
}
\value{
If \code{size} is provided, the function sets the global font size and
returns \code{NULL}. If \code{size} is not provided, the current global font size is
returned. The default value is 8 if no global font size has been set.
}
\description{
This function sets or gets the global font size option.
}
\examples{
global_fontsize(12)  # Set global font size to 12
current_size <- global_fontsize()  # Get current global font size

}
