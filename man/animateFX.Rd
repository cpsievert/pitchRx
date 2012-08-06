\name{animateFX}
\alias{animateFX}
\title{Animate Pitch F/X}
\usage{
  animateFX(data, layer = NULL, interval = 0.01,
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
  Details to go here.
}
\examples{
#Simple scraping example
#data <- scrapePitchFX(start = "2011-10-01", end = "2011-10-02")
#pitches <- data$pitch
#atbats <- data$atbat
#pitchFX <- join(pitches, atbats, by = c("num", "url"))
#Subset data by pitch type
#pitchFX2 <- pitchFX[pitchFX$zone < 4 & pitchFX$pitch_type == c("FF", "CU", "SL"), ]
#animateFX(pitchFX2)
#animateFX(pitchFX2, layer = facet_grid(stand~p_throws))#How do I add titles to the facets (stand vs. p_throws)
}

