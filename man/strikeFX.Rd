\name{strikeFX}
\alias{strikeFX}
\title{Visualize PITCHf/x strikezones}
\usage{
  strikeFX(data, geom = "point", point.size = 3,
    point.alpha = 1/3, color = "pitch_types",
    density1 = list(), density2 = list(),
    diff.density = TRUE, breaks = c(0, 5, 10),
    adjust = TRUE, layer = list(), ...)
}
\arguments{
  \item{data}{PITCHf/x data to be visualized.}

  \item{geom}{type of geometry used for plotting.}

  \item{point.size}{Size of points}

  \item{point.alpha}{ggplot2 alpha parameter}

  \item{color}{variable used to control coloring scheme.}

  \item{tile.density1}{If geom="tile", this list defines a
  density estimate.}

  \item{tile.density2}{If geom="tile", this list defines a
  second density estimate, which is subtracted from the
  first density etimate.}

  \item{breaks}{bin breaks for counts when \code{geom ==
  "hex"}.}

  \item{adjust}{logical vector. Should vertical locations
  be adjusted according to batter height?}

  \item{layer}{list of other ggplot2 (layered)
  modifications.}

  \item{...}{extra options passed onto geom commands}
}
\value{
  Returns a ggplot2 object.
}
\description{
  Pitch locations as they crossed home plate.
}
\details{
  Scatterplot with "px" on the horizontal axis and "pz" on
  the vertical axis.
}
\examples{
data(pitches)
strikeFX(pitches)
strikeFX(pitches, layer=facet_grid(pitcher_name~stand))
strikeFX(pitches, geom="tile", layer=facet_grid(pitcher_name~stand))
strikeFX(pitches, geom="tile", tile.density1=list(des="Called Strike"), layer=facet_grid(pitcher_name~stand))
strikeFX(pitches, geom="tile", tile.density1=list(des="Called Strike"), tile.density2=list(des="Ball"),layer=facet_grid(pitcher_name~stand))
}

