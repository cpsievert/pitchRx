#' Update an existing PITCHf/x database
#'
#' Data from games played starting the day after the most recent date in the database
#' are appended to the appropriate tables.
#'
#' @details Using this function requires the DBI package
#' @seealso \url{http://baseballwithr.wordpress.com/2014/04/13/modifying-and-querying-a-pitchfx-database-with-dplyr/}
#'
#' @param connect Either an SQLite or MySQL database connection
#' @param end date to stop data collection. The default value of 'yesterday' is recommended to ensure the update performs properly.
#' @param ... arguments passed onto \link{scrape}
#' @export
#' @examples
#'
#'
#' \dontrun{
#'  library(dplyr)
#'  db <- src_sqlite("pitchRx.sqlite3")
#'  update_db(db$con)
#' }
#'

update_db <- function(connect, end = Sys.Date() - 1, ...) {
  if (!requireNamespace('DBI')) warning("The DBI package is required to use this function.\n",
                               "Please run install.packages('DBI')")
  if(!DBI::dbExistsTable(connect, "atbat")) stop("Your database must have the atbat table in order to work")
  # Create an index for faster querying
  # Note this throws an error if the INDEX already exists
  res <- plyr::try_default(DBI::dbSendQuery(connect, 'CREATE INDEX gid_idx ON atbat(gameday_link)'), NULL, quiet = TRUE)
  old.gids <- DBI::dbGetQuery(connect, "SELECT DISTINCT gameday_link FROM atbat")[,1]
  old.dates <- gid2date(old.gids)
  if (max(old.dates >= end)) { message("Already up to date"); return(NULL) }

  # Figure out what suffices to pass along scrape
  tbls <- DBI::dbListTables(connect)
  suffices <- NULL
  inning_all.tbls <- c("action", "atbat", "pitch", "po", "runner")
  if (any(inning_all.tbls %in% tbls)) suffices <- c(suffices, "inning/inning_all.xml")
  inning_hit.tbls <- c("hip")
  if (any(inning_hit.tbls %in% tbls)) suffices <- c(suffices, "inning/inning_hit.xml")
  game.tbls <- c("game", "media")
  if (any(game.tbls %in% tbls)) suffices <- c(suffices, "miniscoreboard.xml")
  player.tbls <- c("player", "coach", "umpire")
  if (any(player.tbls %in% tbls)) suffices <- c(suffices, "players.xml")

  scrape(start = max(old.dates) + 1, end = end, suffix = suffices, connect = connect, ...)
  # count the number of records per game and rescrape game that are missing records?
  # Nice idea, but how would I update a data structre with the number of records in a reasonable way?
}


gid2date <- function(x) {
  chr <- gsub("_[a-z]+_[a-z]+_[0-9]$", "", gsub("^gid_", "", x))
  as.Date(chr, format = "%Y_%m_%d")
}