\name{ggFX}
\alias{ggFX}
\title{Animate Pitch F/X}
\usage{
  ggFX(data, layer = list(), interval = 0.01,
    sleep = 1e-06)
}
\arguments{
  \item{data}{pitch F/X data to be visualized.}

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
  More flexible animation.
}

