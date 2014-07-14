#' Sample PITCHf/x Data Set
#' 
#' Every four-seam and cutting fastball thrown by Mariano Rivera and Phil Hughes during the 2011 season.
#' 
#' @format A data frame with variables from the 'atbat' and 'pitch' tables.
#' @seealso \url{http://fastballs.wordpress.com/2007/08/02/glossary-of-the-gameday-pitch-fields/}
#' 
#' @examples
#' #This can reproduce data(pitches), but it takes a while...
#' \dontrun{
#' data <- scrape(start="2011-01-01", end="2011-12-31") 
#' names <- c("Mariano Rivera", "Phil Hughes") 
#' atbats <- subset(data$atbat, pitcher_name %in% names) 
#' pitchFX <- plyr::join(atbats, data$pitch, by=c("num", "url"), type="inner") 
#' pitches <- subset(pitchFX, pitch_type %in% c("FF", "FC")) 
#' }
"pitches"

#' All MLB and MiLB players from 2008 to date
#' 
#' A data frame with the full name and corresponding ID for every player. This data is used during \code{scrape} to append a name 
#' to the atbat table so we can reference data by batter_name and pitcher_name without any extra hassle. This was constructed by taking the unique 
#' name and ID combinations from every player file of the form - \url{http://gd2.mlb.com/components/game/mlb/year_YYYY/month_MM/day_DD/gid_YYYY_MM_DD_xxxmlb_yyymlb_1/players.xml}
#' 
#' @format A data frame with 2 variables: \code{ID} and \code{full_name}
#'
"players"

#' All MLB Gameday IDs from 2008-2013
#' 
#' A character vector with every "gameday" attribute in the "game" element taken from scoreboard files like this one:
#' \url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/miniscoreboard.xml}
#' Note they are ordered from oldest game to newest game.
#' 
#' @format A character vector
#'
"gids"

#' All non-MLB Gameday IDs from 2008-2013
#' 
#' A character vector with every "gameday" attribute in the "game" element taken from scoreboard files like this one:
#' \url{http://gd2.mlb.com/components/game/aaa/year_2013/month_06/day_08/gid_2013_06_08_freaaa_slcaaa_1/miniscoreboard.xml}
#' Note they are ordered from oldest game to newest game.
#' 
#' @format A character vector
#'
"nonMLBgids"

#' Master list of tables and fields returned by \code{scrape}
#' 
#' This data object is as a template for fields and fields types for each table.
#' Since it's much easier to write to a table with more fields (than vice versa), 
#' this object contains every possible field for each table.
#' 
#' @format A list of character vectors.
#'
"fields"

