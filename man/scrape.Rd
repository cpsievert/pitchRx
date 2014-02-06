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
  'players.xml', 'miniscoreboard.xml',
  'inning/inning_all.xml', 'inning/inning_hit.xml'.}

  \item{connect}{A database connection object. The class of
  the object should be "MySQLConnection" or
  "SQLiteConnection". If a valid connection is supplied,
  tables will be copied to the database, which will result
  in better memory management. If a connection is supplied,
  but the connection fails for some reason, csv files will
  be written to the working directory.}
}
\value{
Returns a list of data frames (or nothing if writing to a
database).
}
\description{
Function for obtaining PITCHf/x and other related Gameday
Data. \code{scrape} currently has support for files ending
with:
\href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/inning/inning_all.xml}{inning/inning_all.xml},
\href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/inning/inning_hit.xml}{inning/inning_hit.xml},
\href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/players.xml}{players.xml},
or
\href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/miniscoreboard.xml}{miniscoreboard.xml}.
It's worth noting that PITCHf/x is contained in files
ending with "inning/inning_all.xml", but the other files
can complement this data depending on the goal for
analysis. Any collection of file names may be passed to the
\code{suffix} argument, and \code{scrape} will retrieve
data from a (possibly large number) of files based on
either a window of dates or a set of \code{game.ids}. If
collecting data in bulk, it is strongly recommended that
one establishes a database connection and supplies the
connection to the \code{connect} argument. See the examples
section for a simple example of how to do so.
}
\note{
This function was adapted from \code{scrapeFX} which is
deprecated as of version 1.0
}
\examples{
\dontrun{
# Collect PITCHf/x (and other data from inning_all.xml files) from May 1st, 2012
dat <- scrape(start = "2013-08-01", end = "2013-08-01")
# OR, equivalently, use the game.ids argument
data(gids, package="pitchRx")
dat2 <- scrape(game.ids=gids[grep("2012_05_01", gids)])

#scrape PITCHf/x from Minnesota Twins 2011 season
twins11 <- gids[grepl("min", gids) & grepl("2011", gids)]
dat <- scrape(game.ids=twins11)

#Create SQLite database, then collect and store data in that database
library(dplyr)
my_db <- src_sqlite("my_db.sqlite3", create=T)
scrape(start = "2013-08-01", end = "2013-08-01", connect=my_db$con)

#simple example to demonstrate database query using dplyr
#note that 'num' and 'url' together make a key that allows us to join these tables
locations <- select(tbl(my_db, "pitches"), px, pz, des, num, url)
names <- select(tbl(my_db, "atbats"), pitcher_name, batter_name, num, url)
que <- inner_join(locations, filter(names, batter_name == "Paul Goldschmidt"))
que$query #refine sql query if you'd like
pitchfx <- collect(que) #submit query and bring data into R

# Collect PITCHf/x and other complementary data
files <- c("inning/inning_all.xml", "inning/inning_hit.xml",
             "miniscoreboard.xml", "players.xml")
dat3 <- scrape(start = "2012-05-01", end = "2012-05-01", suffix = files)

}
}
\seealso{
If you want to add support for more file types, the
\code{XML2R} package is a good place to start.
}

