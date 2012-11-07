\name{getDensity}
\alias{getDensity}
\title{2D Binned Kernel Differenced Density Estimates}
\usage{
  getDensity(data, density, bandwidth = c(0.1, 0.1),
    gridsize = c(51L, 51L),
    range.x = list(c(-2, 2), c(1, 4)), truncate = TRUE)
}
\arguments{
  \item{data}{PITCHf/x data}

  \item{density}{formula that describes the (possibly
  differenced) 2D Binned Kernel Density Estimates.}

  \item{bandwidth}{KernSmooth::bkde2D parameter}

  \item{gridsize}{KernSmooth::bkde2D parameter}

  \item{range.x}{KernSmooth::bkde2D parameter}

  \item{truncate}{KernSmooth::bkde2D parameter}
}
\value{
  Returns a data frame with binning values and density
  estimates.
}
\description{
  Computes differenced 2D Binned Kernel Density Estimates
  using KernSmooth::bkde2D
}
\details{
  Computes two densities on the same support and subtracts
  them according to the \code{density} expression. The
  expression should look as follows:
  variable~value1-value2.
}

