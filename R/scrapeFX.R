#' Scrape Major League Baseball's PITCHf/x Data
#'
#' This function is a wrapper around \link{urlsToDataFrame} which increases convenience for scraping PITCHf/x directly from XML files.
#'
#' Data should be collected on a yearly (or shorter) basis. By default, records from the 'pitch' and 'atbat' level are collected.
#' One should manipulate the \code{tables} parameter if other data is desired. 
#' 
#' @param start date "yyyy-mm-dd" to commence scraping of pitch F/X data
#' @param end date "yyyy-mm-dd" to terminate scraping pitch F/X data
#' @param tables XML nodes to be parsed into a data frame
#' @seealso \link{urlsToDataFrame}
#' @return Returns a list containing a data frame specific to each element in \code{tables}. The default setting returns two data frames. The larger one contains data "PITCHfx parameters" for each pitch. The smaller one contains data relevant to each atbat.
#' @export
#' @examples
#' #Collect PITCHf/x data for May 1st, 2012
#' data <- scrapeFX(start = "2012-05-01", end = "2012-05-01")
#' #Join tables for data analysis
#' pitchFX <- join(data$pitch, data$atbat, by = c("num", "url"), type = "inner")
#' 
#' \dontrun{Algorithm for obtaining all available PITCHfx data** 
#' # (1) Collect PITCHfx data from 2012
#' data12 <- scrapeFX()
#' # (2) Write data12$pitch and data12$atbat to a database
#' # (3) Remove 2012 data from working space
#' rm(data12)
#' # (4) Repeat (1)-(3) for 2011, 2010, 2009 & 2008}

scrapeFX <- function(start = "2012-01-01", end = "2012-12-31", tables = list(atbat = fields$atbat, pitch = fields$pitch)) { 
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  if (is.null(names(tables))) stop("Please specify at least one XML node of interest.")
  if (year(start) < 2005) {
    warning("Not only is pitchFX data not avaliable before 2008, data on each game isn't consistent until 2005")
    start <- as.POSIXct("2005-01-01")
  }
  if (year(start) < 2008) warning("pitchFX data wasn't recorded consistently until 2008. Do you want to consider a later start date?")
  if (end > as.POSIXct(Sys.Date())) warning("Sorry, I can't scrape data on the future!")
  data(fields)
  data(urls) 
  last.date <- as.POSIXct(max(urls$date))
  if (last.date < end) { #update urls if new ones exist
    new.urls <- updateUrls(last.date, end)
    urls <- rbind(urls, new.urls)
#     new.players <- updatePlayers(new.urls$url_player)
#     browser()
#     players <- rbind(players, new.players)
  }
  urls <- subset(urls, date >= start & date <= end) #Subset urls to dates of interest
  if ("game" %in% names(tables)) { #Special handling since game node appears in other files (and we don't want the records from the other files)
    scoreboards <- unique(urls[,"url_scoreboard"])
    game <- urlsToDataFrame(scoreboards, tables=list(game=fields$game))
    game <- attachUrls(game)
    print("This function will scrape the game node within the '~/miniscoreboard.xml'. Information in the game node from other files can be derived from here.")
    tables <- tables[-grep("game", names(tables))] #'game' info is collected, don't need the tables element anymore
  } else game <- NULL
#   if (length(player) > 0) { 
#     data(players)
#     desired.players <- suppressWarnings(subset(players, full_name == player))
#     scoreboards <- unique(gsub("gid_[0-9]{1,4}_[0-9]{1,2}_[0-9]{1,2}_[a-z]{1,6}_[a-z]{1,6}_[0-9]/players.xml", "miniscoreboard.xml", desired.players$url_player))
#     pfx.urls <- gsub("players.xml", "inning/inning_all.xml", desired.players$url_player)
#     player.urls <- c(desired.players$url_player, scoreboards, pfx.urls)
#     scraping.urls <- scraping.urls[scraping.urls %in% player.urls] #How do I subset the miniscoreboards?
#   }
  scraping.urls <- NULL
  if (any(names(tables) %in% c("atbat", "pitch", "runner"))) scraping.urls <- c(scraping.urls, urls[,"url"])
  if (any(names(tables) %in% c("player", "coach", "umpire"))) scraping.urls <- c(scraping.urls, urls[,"url_player"])
  data <- urlsToDataFrame(urls = scraping.urls, tables) #tables doesn't have 'game'
  if (!is.null(game)) data$game <- game 
  data <- cleanList(data)
  if ("atbat" %in% names(tables)) {
#     if (length(player) > 0) { 
#       player.ids <- unique(desired.players$id)
#       if (length(type) == 0) { #Subset 'atbats' by specified player(s)
#         data$atbat <- suppressWarnings(subset(data$atbat, pitcher %in% player.ids || batter %in% player.ids))
#       } else { #Subset 'atbats' by specified player(s) and their respective type(s)
#         typed <- tolower(type)
#         if (typed %in% c("pitcher", "batter")) {
#           for (i in 1:length(player.ids)) {
#             if (typed == "pitcher") data$atbat <- suppressWarnings(subset(data$atbat, pitcher %in% as.character(player.ids[i])))
#             if (typed == "batter") data$atbat <- suppressWarnings(subset(data$atbat, batter %in% as.character(player.ids[i])))
#           }
#         } else warning("The data was not subsetted according to type. At least one entry is not equal to 'pitcher' or 'batter'.")
#       }
#     }
    #Add batter name to 'atbats'
    unique.players <- players[!duplicated(players[,c("id", "full_name")]), c("id", "full_name")]
    names(data$atbat) <- gsub("batter", "id", names(data$atbat))
    data$atbat <- join(data$atbat, unique.players, by = "id")
    names(data$atbat) <- gsub("id", "batter", names(data$atbat))
    names(data$atbat) <- gsub("full_name", "batter_name", names(data$atbat))
    #Add pitcher name to 'atbats'
    names(data$atbat) <- gsub("pitcher", "id", names(data$atbat))
    data$atbat <- join(data$atbat, unique.players, by = "id")
    names(data$atbat) <- gsub("id", "pitcher", names(data$atbat))
    names(data$atbat) <- gsub("full_name", "pitcher_name", names(data$atbat))
  }
  return(data)
}

#' Update \code{urls} data frame
#'
#' This function scrapes "gameday_links" from the MLB website. These links are used to construct the urls needed to 
#' obtain PITCHf/x data. It should only be called in \code{scrapeFX} when the user enters an end date later 
#' than the most recent date present in \code{data(urls)}.
#'
#' @param last.date most recent date in \code{data(urls)}
#' @param end any date more recent than last.date
#' @return returns a data frame
#' @export
#' @examples
#' newUrls <- updateUrls(Sys.Date() - 1, Sys.Date())
#' head(newUrls)

updateUrls <- function(last.date, end) {
    cat("updating urls", "\n")
    diff <- as.numeric(end - last.date)
    dates <- last.date + c(0:diff) * days(1)
    years <- year(dates)
    mnths <- formatC(month(dates), width = 2, flag = "0")
    dys <- formatC(day(dates), width = 2, flag = "0")
    branches <- as.list(paste("http://gd2.mlb.com/components/game/mlb/year_", years, "/month_", mnths, "/day_", dys, "/", sep = ""))
    game.branches <- NULL
    for (i in branches) {
        cat(i, "\n")
        doc <- try_default(htmlParse(i), NULL, quiet = TRUE)
        if (!is.null(doc)) {
          gids <- unlist(str_extract_all(xpathSApply(doc, "//a[@href]", xmlGetAttr, "href"), "gid_[0-9]{1,4}_[0-9]{1,2}_[0-9]{1,2}_[a-z]{1,6}_[a-z]{1,6}_[0-9]"))
          game.branches <- c(game.branches, paste(i, gids, "/", sep = ""))
        }
    }
    url <- paste(game.branches, "inning/inning_all.xml", sep = "")
    url.player <- paste(game.branches, "players.xml", sep = "")
    url.scoreboard <- gsub("gid_[0-9]+_[0-9]+_[0-9]+_[a-z]+_[a-z]+_[0-9]/", "miniscoreboard.xml", game.branches)
    #Generate date from game id
    split.urls <- str_split(url, "/")
    gids <- sapply(split.urls, function(x) { x[10] })
    date <- sapply(str_split(gids, "_"), function(x) { paste(x[2], x[3], x[4], sep = "/") })
    urls <- data.frame(date = date, url_scoreboard = url.scoreboard, url = url, url_player = url.player)
    if (length(grep("NA", date) > 0)) {#Some days don't have games (even in the middle of season). In this case, "NA/NA/NA" are produced, which causes errors when urls is subsetted
      urls <- urls[-grep("NA", date),]
    }
    return(urls) 
}

#' Update \code{players} data frame
#'
#' This function takes on (new) player urls and scrapes personal information on each player.
#'
#' @param new.urls new player urls added to the \code{urls} data frame
#' @return returns new player information in a data frame

updatePlayers <- function(new.urls) {
  cat("updating players", "\n")
  new.players <- urlsToDataFrame(urls = new.urls, tables = list(player = c("id", "first", "last", "position")))
  full_name <- paste(new.players$first, new.players$last, sep = " ")
  new.players <- cbind(new.players[,c("url_player", "id")], full_name)
  return(new.players)
}


#' Clean list of data frames (for scrapeFX)
#' 
#' @param ldf list of dataframes
#' @return list of data frames


cleanList <- function(ldf) {
  for (j in names(ldf)) {
    if (j %in% c("player", "coach", "umpire")) {
      names(ldf[[j]]) <- gsub("url", "url_player", names(ldf[[j]]))
    }
    if (j == "atbat_id"){
      ldf$pitch$num <- ldf$atbat_id #attach 'num' column to 'pitch' df (generated from the atbat node)
      ldf$atbat_id <- NULL
    }
    if (j == "pitch") {
      ldf$pitch$count <- addPitchCount(ldf$pitch)
    }
  }
  return(ldf)
}

#' Add columns with relevant "~/miniscoreboard.xml", "~/inning/inning_all.xml" and "~/player.xml" 
#' file names to games table.
#'
#' @param df data frame with all "game" attributes from "~/miniscoreboard.xml" files.
#' @return returns the original data frame with the proper url columns attached at the end.

attachUrls <- function(df) {
  names(df) <- gsub("url", "url_scoreboard", names(df))
  branch <- gsub("miniscoreboard.xml", "", df$url_scoreboard) #common branch among urls
  df$url <- paste(branch, paste("gid_", df$gameday_link, sep = ""), "/inning/inning_all.xml", sep = "") #files with pitchf/x info
  df$url_player <- gsub("/inning/inning_all.xml", "/players.xml", df$url) #files with player information and statistics
  df$date <- sapply(str_split(df$gameday_link, "_"), function(x) { paste(x[1], x[2], x[3], sep = "-") })
  return(df)
}

#' Add columns with relevant pitch count to the 'pitch' data frame.
#'
#' @param df data frame with all "pitch" attributes from "~/inning/inning_all.xml" files.
#' @return returns the original data frame with the proper pitch count columns attached at the end.
#' @export

addPitchCount <- function(df) {
  df$balls <- as.numeric(df$type == "B")
  df$strikes <- as.numeric(df$type == "S")
  counts <- dlply(idata.frame(df[,c("url", "num", "type", "balls", "strikes")]), c("url", "num"), function(x) { 
    n <- nrow(x) 
    cbind(cumsum(c(0, x$balls[-n])), pmin(cumsum(c(0, x$strikes[-n])), 2)) 
  })
  counts <- llply(counts, as.data.frame)
  counts <- ldply(counts, rbind)
  return(paste(counts[,2], counts[,3], sep = "-"))
}
