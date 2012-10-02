\name{updateUrls}
\alias{updateUrls}
\title{Update \code{urls} data frame}
\usage{
  updateUrls(last.date, end)
}
\arguments{
  \item{last.date}{most recent date in \code{data(urls)}}

  \item{end}{any date more recent than last.date}
}
\value{
  returns a data frame
}
\description{
  This function scrapes "gameday_links" from the MLB
  website. These links are used to construct the urls
  needed to obtain pitchf/x data. It should only be called
  in \code{scrapeFX} when the user enters an end date later
  than the most recent date present in \code{data(urls)}.
}
\examples{
newUrls <- updateUrls(Sys.Date() - 1, Sys.Date())
head(newUrls)
}

