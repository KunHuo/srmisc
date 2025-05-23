% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{join_elements}
\alias{join_elements}
\title{Join Elements with Commas and "and"}
\usage{
join_elements(vec)
}
\arguments{
\item{vec}{A character vector of elements to be joined.}
}
\value{
A single string with the elements joined according to the rules above.
If the input vector is \code{NULL}, the function returns \code{NULL}.
}
\description{
This function takes a vector of elements and joins them into a single string.
If there is only one element, it is returned as is. If there are two elements,
they are joined with "and". If there are more than two elements, they are
joined with commas, and the last element is prefixed with "and".
}
\examples{
join_elements(c("Apple"))           # Returns "Apple"
join_elements(c("Apple", "Banana")) # Returns "Apple and Banana"
join_elements(c("Apple", "Banana", "Cherry")) # Returns "Apple, Banana, and Cherry"
join_elements(NULL) # Returns NULL

}
