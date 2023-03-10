% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-ggplot2.R
\name{gg_ylab}
\alias{gg_ylab}
\title{Modify y-axis labels}
\usage{
gg_ylab(label)
}
\arguments{
\item{label}{the title of the respective axis.}
}
\value{
An obejct of ggplot2.
}
\description{
Modify y-axis labels
}
\examples{
library(ggplot2)

ggplot(diamonds) +
  geom_density(aes(x = price, fill = color)) +
  gg_ylab("Density")
}
\seealso{
\code{\link[ggplot2:labs]{ggplot2::ylab()}}
}
