\name{diffDensity}
\alias{diffDensity}
\title{Differenced 2D Kernel Density Estimates}
\usage{
  diffDensity(data, density1, density2, limz)
}
\arguments{
  \item{data}{PITCHf/x data}

  \item{density1}{either density1 or density2 passed on
  from \link{strikeFX}.}

  \item{density2}{either density1 or density2 passed on
  from \link{strikeFX}.}

  \item{limz}{MASS::kde2d paramaters passed from
  \link{strikeFX}.}
}
\value{
  Returns a data frame with differenced density estimates
  as column z.
}
\description{
  Computes differenced 2D Kernel Density Estimates using
  MASS::kde2d
}
\details{
  Computes two densities on the same support and subtracts
  them according to the \code{density} expression.
}

