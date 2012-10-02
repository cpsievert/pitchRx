#' Scrape Major League Baseball's PITCHf/x Data
#'
#' This function is a wrapper around \code{urlsToDataFrame} which reduces the time required to obtain PITCHf/x
#' from the source files.
#'
#' Details go here.
#' 
#' @param start date "yyyy-mm-dd" to commence scraping of pitch F/X data
#' @param end date "yyyy-mm-dd" to terminate scraping pitch F/X data
#' @param tables XML nodes to be parsed into a data frame
#' @param player narrow scraping to a specifc player or set of players. The default value NULL will scrape for all players in the time period.
#' @param type A value of "pitcher" or "batter", the user can constrain scraping to those cases. The default value of NULL will scrape for both types.
#' @return Returns a list containing two different data frames. The larger data frame contains data on every pitch thrown (pitch F/X). The smaller one contains data on every atbat.
#' @export
#' @examples
#' #data <- scrapeFX(start = "2011-05-01", end = "2011-05-01")
#' #pitchFX <- join(data$pitch, data$atbat, by = c("num", "url"), type = "inner")

scrapeFX <- function(start = "2012-01-01", end = Sys.Date(), tables = list(atbat = fields$atbat, pitch = fields$pitch), player = NULL, type = NULL) {
    start <- as.POSIXct(start)
    end <- as.POSIXct(end)
    if (is.null(names(tables))) stop("Please specify at least one XML node of interest.")
    if (year(start) < 2005) {
        warning("Not only is pitchFX data not avaliable before 2008, data on each game isn't consistent until 2005")
        start <- as.POSIXct("2005-01-01")
    }
    if (year(start) < 2008) warning("pitchFX data wasn't recorded consistently until 2008. Do you want to consider a later start date?")
    if (end > as.POSIXct(Sys.Date())) {
        warning("Sorry, I can't scrape data on the future!")
        end <- as.POSIXct(Sys.Date())
    }
    data(urls) 
    last.date <- as.POSIXct(max(urls$date))
    if (last.date < end) { #update data objects if there are new items to scrape.
      new.urls <- updateUrls(last.date, end)
      new.players <- updatePlayers(new.urls$url_player)
      urls <- rbind(urls, new.urls)
      players <- rbind(players, new.players)
    }
    urls <- subset(urls, date >= start & date <= end) #Subset urls to dates of interest
    scraping.urls <- NULL
    if (any(names(tables) == "game")) {
      scraping.urls <- c(scraping.urls, unique(urls[,"url_scoreboard"]))  
      warning("This function will scrape the game node within the '~/miniscoreboard.xml'. Information in the game node from other files can be derived from here.")
    }
    if (any(names(tables) %in% c("atbat", "pitch", "runner"))) scraping.urls <- c(scraping.urls, urls[,"url"])
    if (any(names(tables) %in% c("player", "coach", "umpire"))) scraping.urls <- c(scraping.urls, urls[,"url_player"])
    if (!is.null(player)) { 
        data(players)
        desired.players <- suppressWarnings(subset(players, full_name == player))
        scoreboards <- unique(gsub("gid_[0-9]{1,4}_[0-9]{1,2}_[0-9]{1,2}_[a-z]{1,6}_[a-z]{1,6}_[0-9]/players.xml", "miniscoreboard.xml", desired.players$url_player))
        pfx.urls <- gsub("players.xml", "inning/inning_all.xml", desired.players$url_player)
        player.urls <- c(desired.players$url_player, scoreboards, pfx.urls)
        scraping.urls <- scraping.urls[scraping.urls %in% player.urls] #How do I subset the miniscoreboards?
    }
    data(fields)
    data <- urlsToDataFrame(urls = scraping.urls, tables)
    if (any(names(tables) == "atbat")) {
        if (!is.null(player)) { #Subset 'atbats' by specified player(s) and type'
            player.ids <- unique(desired.players$id)
            if (type == "pitcher") data$atbat <- suppressWarnings(subset(data$atbat, pitcher == player.ids))
            if (type == "batter") data$atbat <- suppressWarnings(subset(data$atbat, batter == player.ids))
            if (is.null(type)) data$atbat <- suppressWarnings(subset(data$atbat, pitcher == player.ids || batter == player.ids))
        }
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
    #Should I subset pitches too?? It might too slow to be worthwhile.
    return(data)
}

#' Update \code{urls} data frame
#'
#' This function scrapes "gameday_links" from the MLB website. These links are used to construct the urls needed to 
#' obtain pitchf/x data. It should only be called in \code{scrapeFX} when the user enters an end date later 
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
    for (i in branches) {
        cat(i, "\n")
        doc <- try_default(htmlParse(i), NULL, quiet = TRUE)
        if (!is.null(doc)) {
        gids <- unlist(str_extract_all(xpathSApply(doc, "//a[@href]", xmlGetAttr, "href"), "gid_[0-9]{1,4}_[0-9]{1,2}_[0-9]{1,2}_[a-z]{1,6}_[a-z]{1,6}_[0-9]"))
        game.branches <- paste(i, gids, "/", sep = "")
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