% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-ggplot2.R
\name{gg_legend_position}
\alias{gg_legend_position}
\title{Set legend position for ggplot2}
\usage{
gg_legend_position(position = NULL)
}
\arguments{
\item{position}{legend position, NULL for indicates deletion, or a character
of 'top', 'bottom', 'left' and 'right', or numeric vector (between 0 and 1)
of length two (x-axis and y-axis).}
}
\value{
An obejct of ggplot2.
}
\description{
Set legend position for ggplot2
}
\examples{
library(ggplot2)

ggplot(diamonds) +
  geom_density(aes(x = price, fill = color))

ggplot(diamonds) +
  geom_density(aes(x = price, fill = color)) +
  gg_legend_position(NULL)

ggplot(diamonds) +
  geom_density(aes(x = price, fill = color)) +
  gg_legend_position("top")

ggplot(diamonds) +
  geom_density(aes(x = price, fill = color)) +
  gg_legend_position(c(0.9, 0.9))
}
