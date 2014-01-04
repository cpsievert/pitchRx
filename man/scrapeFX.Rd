\name{scrapeFX}
\alias{scrapeFX}
\title{Scrape Major League Baseball's PITCHf/x Data}
\usage{
  scrapeFX(start, end,
    tables = list(atbat = fields$atbat, pitch = fields$pitch))
}
\arguments{
  \item{start}{date "yyyy-mm-dd" to commence scraping of
  pitch F/X data}

  \item{end}{date "yyyy-mm-dd" to terminate scraping pitch
  F/X data}

  \item{tables}{XML nodes to be parsed into a data frame}
}
\value{
  Returns a list containing a data frame specific to each
  element in \code{tables}. The default setting returns two
  data frames. The larger one contains data "PITCHfx
  parameters" for each pitch. The smaller one contains data
  relevant to each atbat.
}
\description{
  \textbf{This function is deprecated as of version 1.0}
}
\details{
  This function is a wrapper around \link{urlsToDataFrame}
  which increases convenience for scraping PITCHf/x
  directly from XML files.

  Data should be collected on a yearly (or shorter) basis.
  By default, records from the 'pitch' and 'atbat' level
  are collected. One should manipulate the \code{tables}
  parameter if other data is desired.
}
\examples{
\dontrun{
#Collect PITCHf/x data for May 1st, 2012
dat <- scrapeFX(start = "2012-05-01", end = "2012-05-01")
#Join tables for data analysis
pitches <- plyr::join(dat$pitch, dat$atbat, by = c("num", "url"), type = "inner")
}

\dontrun{Algorithm for obtaining all available PITCHfx data**
# (1) Collect PITCHfx data from 2012
data12 <- scrapeFX(start="2012-01-01", end="2013-01-01")
# (2) Write data12$pitch and data12$atbat to a database
# (3) Remove 2012 data from working space
rm(data12)
# (4) Repeat (1)-(3) for 2011, 2010, 2009 & 2008}
}
\seealso{
  \link{urlsToDataFrame}
}

