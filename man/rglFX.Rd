\name{rglFX}
\alias{rglFX}
\title{Use rgl to visualize PITCHf/x}
\usage{
  rglFX(data, color = "pitch_types", interval = 0.01,
    print.legend = TRUE)
}
\arguments{
  \item{data}{data frame with appropriately named PITCHf/x
  variables}

  \item{interval}{the amount of time between 'snapshots'}

  \item{print.legend}{print coloring legend in R console?}
}
\value{
  rgl plot3d() object is returned.
}
\description{
  Three-dimensional plot of pitch trajectories
}
\details{
  Details go here.
}

