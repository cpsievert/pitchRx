\name{strikeFX}
\alias{strikeFX}
\title{Visualize PITCHf/x strikezones}
\usage{
  strikeFX(data, layer = list(), geom = "point",
    tile.density = des ~ Called.Strike, adjust = TRUE,
    point.color = aes(color = pitch_types))
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
data(pitches)
strikeFX(pitches)
strikeFX(pitches, layer=facet_grid(pitcher_name~stand))
strikeFX(pitches, geom="tile", layer=facet_grid(pitcher_name~stand))
strikeFX(pitches, geom="tile", tile.density=des~Called.Strike-Ball, layer=facet_grid(pitcher_name~stand))
}

