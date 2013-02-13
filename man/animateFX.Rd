\name{animateFX}
\alias{animateFX}
\title{Animate PITCHf/x}
\usage{
  animateFX(data, color = "pitch_types", point.size = 3,
    point.alpha = 1/3, limitz = c(-3.5, 3.5, 0, 7),
    flag = FALSE, interval = 0.01, sleep = 1e-12,
    layer = list(), env = parent.frame(), ...)
}
\arguments{
  \item{data}{data frame with appropriately named PITCHf/x
  variables}

  \item{point.size}{Size of points}

  \item{point.alpha}{ggplot2 alpha parameter}

  \item{limitz}{limits for horizontal and vertical axes.}

  \item{color}{variable used to control coloring scheme.}

  \item{flag}{indicate whether or not batter has decided to
  swing.}

  \item{interval}{time (in seconds) between plotting the
  pitch locations.}

  \item{sleep}{passed along to Sys.sleep() to flush current
  plot.}

  \item{layer}{list of ggplot2 layer modifications.}

  \item{env}{the \link{environment} in which expr is to be
  evaluated.}

  \item{...}{extra options passed onto geom commands}
}
\value{
  Returns a series of ggplot2 objects.
}
\description{
  Pitch trajectories animated on a two-dimensional plot.
}
\details{
  \code{animateFX} plots a series of "snapshots" with pitch
  locations from the point of release - when the ball
  leaves the pitcher's hand - until all of them reach home
  plate. The graphic takes on the viewpoint of the umpire;
  that is, the pitches are getting closer to the viewer
  with time. This is relected with the increase in size of
  the "balls" as the animation progresses. To reduce the
  time and thinking required to produce plots,
  \code{animateFX} makes several assumptions about the
  opacity and size associated with each "snapshot" of pitch
  locations.
}
\examples{
data(pitches)
animateFX(pitches)
animateFX(pitches, layer = facet_grid(pitcher_name~stand))
}

