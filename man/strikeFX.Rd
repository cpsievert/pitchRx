\name{strikeFX}
\alias{strikeFX}
\title{Visualize PITCHf/x strikezones}
\usage{
  strikeFX(data, layer = list(), geom = "point",
    tile.density = des ~ Called.Strike - Ball,
    adjust = TRUE, point.color = aes(color = pitch_types),
    point.alpha = aes(alpha = 0.5), point.size = 100)
}
\arguments{
  \item{data}{PITCHf/x data to be visualized.}

  \item{layer}{list of other ggplot2 (layered)
  modifications.}

  \item{geom}{type of geometry used for plotting.}

  \item{tile.density}{If geom="tile", this formula
  describes the (possibly differenced) 2D Binned Kernel
  Density Estimates.}

  \item{adjust}{logical vector. Should vertical locations
  be adjusted according to batter height?}

  \item{point.color}{variable used to control coloring
  scheme when \code{geom = "point"}.}

  \item{point.alpha}{variable used to control alpha when
  \code{geom = "point"}.}

  \item{point.size}{control size of "points".}
}
\value{
  Returns a ggplot2 object.
}
\description{
  Pitch locations as they crossed home plate.
}
\details{
  Scatterplot with "px" on the horizonatl axis and "pz" on
  the vertical axis.
}
\examples{
#value
}

