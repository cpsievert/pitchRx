\name{strikeFX}
\alias{strikeFX}
\title{Visualize PITCHf/x strikezones}
\usage{
  strikeFX(data, geom = "point", contour = FALSE,
    point.size = 3, point.alpha = 1/3,
    color = "pitch_types", fill = "des", layer = list(),
    density1 = list(), density2 = list(), adjust = TRUE,
    limitz = c(-2.5, 2.5, 0, 5), parent = FALSE, ...)
}
\arguments{
  \item{data}{PITCHf/x data to be visualized.}

  \item{geom}{plotting geometry. Current choices are:
  "point", "hex", "bin", "tile" and "subplot2d".}

  \item{contour}{logical. Should contour lines be
  included?}

  \item{point.size}{Size of points (when geom="point")}

  \item{point.alpha}{plotting transparency parameter (when
  geom="point")}

  \item{color}{variable used to control coloring scheme.}

  \item{fill}{variable used to control subplot scheme (when
  geom="subplot2d")}

  \item{layer}{list of other ggplot2 (layered)
  modifications.}

  \item{density1}{List defines a density estimate.}

  \item{density2}{List defines a density estimate. If
  \code{density1 != density2}, the density estimates are
  automatically differenced.}

  \item{adjust}{logical. Should vertical locations be
  adjusted according to batter height?}

  \item{limitz}{limits for horizontal and vertical axes.}

  \item{parent}{is the function being called from a
  higher-level function? (experimental)}

  \item{...}{extra options passed onto geom commands}
}
\value{
  Returns a ggplot2 object.
}
\description{
  Pitch locations as they crossed home plate.
}
\details{
  Various bivariate plots with "px" on the horizontal axis
  and "pz" on the vertical axis.
}
\examples{
data(pitches)
strikeFX(pitches, geom="tile", layer=facet_grid(.~stand))
\dontrun{strikeFX(pitches, geom="hex", density1=list(des="Called Strike"), density2=list(des="Ball"), layer=facet_grid(.~stand))}
}

