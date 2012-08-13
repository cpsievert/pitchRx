\name{getSnapshots}
\alias{getSnapshots}
\title{Produce time sequenced pitch locations from PITCHf/x parameters}
\usage{
  getSnapshots(data, interval = 0.01)

  getSnapshots(data, interval = 0.01)
}
\arguments{
  \item{data}{The nine PITCHf/x parameters used to
  determine the location of a pitch at a given time.}

  \item{interval}{the amount of time between 'snapshots'}

  \item{data}{The nine PITCHf/x parameters used to
  determine the location of a pitch at a given time.}

  \item{interval}{the amount of time between 'snapshots'}
}
\value{
  Return a three dimensinal array. The third dimension
  corresponds to different 'snapshots' of locations.

  Return a three dimensinal array. The third dimension
  corresponds to different 'snapshots' of locations.
}
\description{
  Pitch trajectories animated on a two-dimensional plot.

  Pitch trajectories animated on a two-dimensional plot.
}
\details{
  Details to go here.

  Details to go here.
}

