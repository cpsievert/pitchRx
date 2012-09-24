\name{animateFX}
\alias{animateFX}
\title{Animate PITCHf/x}
\usage{
  animateFX(data, layer = list(), geom = "point",
    point.color = pitch_types, point.alpha = 0.5,
    point.size = 100 - snapshot$y, breaks = c(0, 5, 10),
    interval = 0.01, sleep = 1e-12, ...)
}
\arguments{
  \item{data}{PITCHf/x data to be visualized.}

  \item{layer}{list of other ggplot2 (layered)
  modifications.}

  \item{geom}{type of geometry used for plotting.}

  \item{point.color}{variable used to control coloring
  scheme when \code{geom = "point"}.}

  \item{point.alpha}{variable used to control alpha when
  \code{geom = "point"}.}

  \item{point.size}{control size of "points".
  Theoretically, this should be based on the distance from
  home plate (ie, \code{snapshot$y})}

  \item{breaks}{bin breaks for counts when \code{geom ==
  "hex"}.}

  \item{interval}{time (in seconds - real time) between
  plotting the pitch locations.}

  \item{sleep}{passed along to Sys.sleep() to flush current
  plot.}
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
#First, grab some data

#Now, the fun part
}

