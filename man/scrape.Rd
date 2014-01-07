\name{scrape}
\alias{scrape}
\title{Scrape Major League Baseball's Gameday Data}
\usage{
  scrape(start, end, gids,
    suffix = "inning/inning_all.xml", ...)
}
\arguments{
  \item{start}{date "yyyy-mm-dd" to commence scraping.}

  \item{end}{date "yyyy-mm-dd" to terminate scraping.}

  \item{gids}{character vector of gameday_links. If this
  option is used, \code{start} and \code{end} are ignored.
  See \code{data(gids)} for examples.}

  \item{suffix}{character vector with suffix of the XML
  files to be parsed. Currently supported options are:
  "inning_all.xml", "inning_hit.xml", ""}

  \item{...}{options passed onto \code{XML2R::XML2Obs}}
}
\value{
  Returns a list of tables.
}
\description{
  Function for obtaining PITCHf/x and other related Gameday
  Data. In theory, this function is able to extract data
  from any XML file under a "game directory". For example,
  see
  \url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/}
  for the game directory specific to the game between the
  Twins and the Yankees on April 4th 2011.
}
\details{
  This function has special handling for files ending with:
  \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/players.xml}{players.xml},
  \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/miniscoreboard.xml}{miniscoreboard.xml}
  and
  \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/inning/inning_all.xml}{inning/inning_all.xml}.
  For other files, this function essentially just calls
  \code{XML2R::XML2Obs} then \code{XML2R::collapse} after
  constructing the proper file names. As a result,
  information in the structure of the data may be lost. If
  this is the case, the user should see utilities from the
  \code{XML2R} package
}
\examples{
\dontrun{
#Collect PITCHf/x and some related Gameday data for May 1st, 2012
files <- c("inning/inning_all.xml", "inning/inning_hit.xml",
             "miniscoreboard.xml", "players.xml")
dat <- scrape(start = "2012-05-01", end = "2012-05-01",
                   suffix = files)
#OR equivalently
data(gids)
dat <- scrape(gids=gids[grep("2012_05_01", gids)], suffix=files)

#scrape data from Minnesota Twins 2011 season
twins11 <- gids[grepl("min", gids) & grepl("2011", gids)]
dat <- scrape(gids=twins11, suffix=files)

}
}
\seealso{
  \link{XML2R::XML2Obs}
}

