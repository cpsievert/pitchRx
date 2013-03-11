\name{interactiveFX}
\alias{interactiveFX}
\title{Use rgl to visualize PITCHf/x}
\usage{
  interactiveFX(data, spheres = TRUE,
    color = "pitch_types", interval = 0.01, alpha = 1,
    print.legend = TRUE)
}
\arguments{
  \item{data}{data.frame with appropriately named PITCHf/x
  variables}

  \item{spheres}{Use \link{spheres3d} or \link{plot3d}?}

  \item{color}{variable used to control coloring scheme.}

  \item{interval}{the amount of time between 'snapshots'}

  \item{alpha}{color transparency}

  \item{print.legend}{print coloring legend in R console?}

  \item{...}{other param passed onto \link{plot3d} or
  \link{spheres3d}}
}
\value{
  rgl object is returned.
}
\description{
  Three-dimensional plot of pitch trajectories.
}
\details{
  Details go here.
}
\examples{
data(pitches)
Rivera <- subset(pitches, pitcher_name=="Mariano Rivera")
\dontrun{interactiveFX(Rivera, interval=.05)}
}

