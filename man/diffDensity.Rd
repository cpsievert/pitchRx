\name{diffDensity}
\alias{diffDensity}
\title{Differenced 2D Kernel Density Estimates}
\usage{
  diffDensity(data, var, values)
}
\arguments{
  \item{data}{PITCHf/x data}

  \item{var}{variable of interest}

  \item{values}{values of var used for 2D Kernel Density
  Estimation.}
}
\value{
  Returns a data frame with differenced density estimates.
}
\description{
  Computes differenced 2D Kernel Density Estimates using
  MASS::kde2d
}
\details{
  Computes two densities on the same support and subtracts
  them according to the \code{density} expression. The
  expression should look as follows:
  variable~value1-value2.
}

