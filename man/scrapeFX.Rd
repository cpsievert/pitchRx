\name{scrapeFX}
\alias{scrapeFX}
\title{Scrape Major League Baseball's PITCHf/x Data}
\usage{
  scrapeFX(start = "2012-01-01", end = Sys.Date(),
    tables = list(atbat = fields$atbat, pitch = fields$pitch),
    player = NULL, type = NULL)
}
\arguments{
  \item{start}{date "yyyy-mm-dd" to commence scraping of
  pitch F/X data}

  \item{end}{date "yyyy-mm-dd" to terminate scraping pitch
  F/X data}

  \item{tables}{XML nodes to be parsed into a data frame}

  \item{player}{narrow scraping to a specifc player or set
  of players. The default value NULL will scrape for all
  players in the time period.}

  \item{type}{A value of "pitcher" or "batter", the user
  can constrain scraping to those cases. The default value
  of NULL will scrape for both types.}
}
\value{
  Returns a list containing two different data frames. The
  larger data frame contains data on every pitch thrown
  (pitch F/X). The smaller one contains data on every
  atbat.
}
\description{
  This function is a wrapper around \code{urlsToDataFrame}
  which reduces the time required to obtain PITCHf/x from
  the source files.
}
\details{
  Details go here.
}
\examples{
#data <- scrapeFX(start = "2011-05-01", end = "2011-05-01")
#pitchFX <- join(data$pitch, data$atbat, by = c("num", "url"), type = "inner")
}

