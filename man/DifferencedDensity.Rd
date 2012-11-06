\name{DifferencedDensity}
\alias{DifferencedDensity}
\title{Differenced 2D Binned Kernel Density Estimates}
\usage{
  DifferencedDensity(data, bandwidth = c(0.1, 0.1),
    gridsize = c(51L, 51L),
    range.x = list(c(-2, 2), c(1, 4)), truncate = TRUE)
}
\arguments{
  \item{data}{PITCHf/x data}
}
\value{
  Returns a data frame with binning values and density
  estimates. Similar to stat_contour?
}
\description{
  Computes differenced 2D Binned Kernel Density Estimates
  using bkde2D{KernSmooth}
}
\details{
  Details go here.
}

