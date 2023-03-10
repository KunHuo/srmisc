% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-ggplot2.R
\name{gg_bold_tags}
\alias{gg_bold_tags}
\title{Bold tags for ggplot2}
\usage{
gg_bold_tags(bold = TRUE)
}
\arguments{
\item{bold}{a logical indicating whether to bold the tags, default TRUE.}
}
\value{
An obejct of ggplot2.
}
\description{
Bold tags for ggplot2
}
\examples{
library(ggplot2)

ggplot(diamonds) +
  geom_density(aes(x = price, fill = color)) +
  gg_tags("A")

ggplot(diamonds) +
  geom_density(aes(x = price, fill = color)) +
  gg_tags("A") +
  gg_bold_tags()
}
