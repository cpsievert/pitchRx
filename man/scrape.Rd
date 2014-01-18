\name{scrape}
\alias{scrape}
\title{Scrape Major League Baseball's Gameday Data}
\usage{
scrape(start, end, game.ids, suffix = "inning/inning_all.xml", ...)
}
\arguments{
  \item{start}{date "yyyy-mm-dd" to commence scraping.}

  \item{end}{date "yyyy-mm-dd" to terminate scraping.}

  \item{game.ids}{character vector of gameday_links. If
  this option is used, \code{start} and \code{end} are
  ignored. See \code{data(gids)} for examples.}

  \item{suffix}{character vector with suffix of the XML
  files to be parsed. Currently supported options are:
  'players.xml', 'miniscoreboard.xml', 'inning_all.xml',
  'inning_hit.xml'}

  \item{...}{options passed onto \code{XML2R::XML2Obs}}
}
\value{
Returns a list of tables.
}
\description{
Function for obtaining PITCHf/x and other related Gameday
Data. In theory, this function is able to extract data from
any XML file under the "home" Gameday directory --
\url{http://gd2.mlb.com/components/game/mlb/}.
}
\details{
If values for \code{start} and \code{end} are supplied,
then only relevant directories will be considered. For
example, if \code{start="2011-04-04"} and
\code{end="2011-04-05"}, then only files under the
\url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/}
and
\url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_05/}
will be considered. When \code{start} and \code{end} are
supplied and \code{gids = "infer"}, \code{scrape} will
append relevant gameday_links to urls. For example,
\url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/}
would be one of many games played on April 4th 2011.

This function has special handling for files ending with:
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

#Create SQLite database and copy game table using dplyr
library(dplyr)
my_db <- src_sqlite("my_db.sqlite3", create=T)
copy_to(my_db, dat2$pitch, name="pitches", temporary = FALSE)
copy_to(my_db, dat2$atbat, name="atbats", temporary = FALSE)
copy_to(my_db, dat2$action, name="actions", temporary = FALSE)
copy_to(my_db, dat2$po, name="pos", temporary = FALSE)
copy_to(my_db, dat2$runner, name="runners", temporary = FALSE)

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

