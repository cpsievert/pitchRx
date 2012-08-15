\name{getStrikezones}
\alias{getStrikezones}
\title{Calculate strikezone boundaries}
\usage{
  getStrikezones(data, facets)
}
\arguments{
  \item{data}{PITCHf/x orginally entered into
  \code{animateFX}}

  \item{facets}{variables used for faceting (passed along
  from \code{layer})}
}
\value{
  Returns a list of boundaries for both right handed
  batters and left handed batters
}
\description{
  Strikezone boundaries calculated according to Mike Fast's
  specifications
}
\references{
  \url{http://www.baseballprospectus.com/article.php?articleid=14572}
}

