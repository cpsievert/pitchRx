\name{scrape}
\alias{scrape}
\title{Scrape Major League Baseball's Gameday Data}
\usage{
scrape(start, end, game.ids, suffix = "inning/inning_all.xml", connect)
}
\arguments{
  \item{start}{date "yyyy-mm-dd" to commence scraping.}

  \item{end}{date "yyyy-mm-dd" to terminate scraping.}

  \item{game.ids}{character vector of gameday_links. If
  this option is used, \code{start} and \code{end} are
  ignored. See \code{data(gids, package="pitchRx")} for
  examples.}

  \item{suffix}{character vector with suffix of the XML
  files to be parsed. Currently supported options are:
  'players.xml', 'miniscoreboard.xml', 'inning_all.xml',
  'inning_hit.xml'.}

  \item{connect}{A database connection object. The class of
  the object should be "MySQLConnection" or
  "SQLiteConnection". If a valid connection is supplied,
  tables will be copied to the database, which will result
  in better memory management. If a connection is supplied,
  but the connection fails for some reason, csv files will
  be written to the working directory.}
}
\value{
Returns a list of data frames.
}
\description{
Function for obtaining PITCHf/x and other related Gameday
Data. If values for \code{start} and \code{end} are
supplied, then only relevant directories will be
considered. For example, if \code{start="2011-04-04"} and
\code{end="2011-04-05"}, then files under the
\url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/}
and
\url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_05/}
homepages will be considered. When \code{start} and
\code{end} are supplied, \code{scrape} will automatically
find and append necessary gameday_links to urls. For
example,
\url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/}
would be one of many games played on April 4th 2011.
}
\details{
On the other hand, the \code{game.ids} argument allows one
to query specific games rather than all games within a
window of dates.

This function (currently) has special handling for files
ending with:
\href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/players.xml}{players.xml},
\href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/miniscoreboard.xml}{miniscoreboard.xml},
\href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/inning/inning_all.xml}{inning/inning_all.xml},
and
\href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/inning/inning_hit.xml}{inning/inning_hit.xml}
}
\examples{
\dontrun{
# Collect PITCHf/x (and other data from inning_all.xml files) from May 1st, 2012
dat <- scrape(start = "2013-08-01", end = "2013-08-01")
# OR, equivalently, use the gids option
data(gids)
dat2 <- scrape(game.ids=gids[grep("2012_05_01", gids)])

#Create SQLite database, then collect and store data in that database
library(dplyr)
my_db <- src_sqlite("my_db.sqlite3", create=T)
scrape(start = "2013-08-01", end = "2013-08-01", connect=my_db$con)

#simple example of a common query
pit <- inner_join(tbl(my_db, "pitches"), tbl(my_db, "atbats"))
pit$query #analyze query (do you really want all these columns?)
pitchfx <- collect(pit) #submit query and bring data into R


# Collect PITCHf/x and other supporting information which scrape() will format nicely
files <- c("inning/inning_all.xml", "inning/inning_hit.xml",
             "miniscoreboard.xml", "players.xml")
dat3 <- scrape(start = "2012-05-01", end = "2012-05-01",
                   suffix = files)

#scrape PITCHf/x from Minnesota Twins 2011 season
twins11 <- gids[grepl("min", gids) & grepl("2011", gids)]
dat <- scrape(game.ids=twins11)

}
}
\seealso{
\code{XML2R::XML2Obs}
}

