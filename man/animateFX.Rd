\name{animateFX}
\alias{animateFX}
\title{Animate PITCHf/x}
\usage{
  animateFX(data, layer = NULL, interval = 0.01,
    sleep = 1e-12)
}
\arguments{
  \item{data}{PITCHf/x data to be visualized.}

  \item{layer}{list of ggplot2 modifications to the plot}

  \item{interval}{time (in seconds - real time) between
  plotting the pitch locations}

  \item{sleep}{passed along to Sys.sleep() to flush current
  plot}
}
\value{
  ggplot2 object
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
  geometry, color, opacity and size associated with each
  "snapshot" of pitch locations.
}
\examples{
#Simple scraping example
}

