\name{ggFX}
\alias{ggFX}
\title{Animate PITCHf/x}
\usage{
  ggFX(data, layer = list(), interval = 0.01,
    sleep = 1e-06)
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
  More flexible animation than \code{animateFX}. This
  function requires the user to specify a geometry (or, a
  statistical transfromation) to be made to the data before
  plotting. It allows allows for coloring by
}

