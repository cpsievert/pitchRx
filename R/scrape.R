#' Scrape Major League Baseball's Gameday Data
#'
#' Function for obtaining PITCHf/x and other related Gameday Data. In theory, this function is able 
#' to extract data from any XML file under the "home" Gameday directory -- \url{http://gd2.mlb.com/components/game/mlb/}.
#' 
#' If values for \code{start} and \end{end} are supplied, then only relevant directories will be considered. For example,
#' if \code{start="2011-04-04"} and \code{end="2011-04-05"}, then only files under the \url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/}
#' and \url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_05/} will be considered. When \code{start} and \end{end} are supplied and \code{gids = "infer"},
#' \code{scrape} will append relevant gameday_links to urls. For example, \url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/}
#' would be one of many games played on April 4th 2011.
#' 
#' This function has special handling for files ending with:
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/players.xml}{players.xml},
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/miniscoreboard.xml}{miniscoreboard.xml} and
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/inning/inning_all.xml}{inning/inning_all.xml}. 
#' For other files, this function essentially just calls \code{XML2R::XML2Obs} then \code{XML2R::collapse} after constructing the proper file names. 
#' As a result, information in the structure of the data may be lost. If this is the case, the user should see 
#' utilities from the \code{XML2R} package
#' 
#' 
#' @param start date "yyyy-mm-dd" to commence scraping.
#' @param end date "yyyy-mm-dd" to terminate scraping.
#' @param game.ids character vector of gameday_links. If this option is used, \code{start} and \code{end} are ignored. 
#' See \code{data(gids)} for examples.
#' @param suffix character vector with suffix of the XML files to be parsed. Currently supported options are: "inning_all.xml", "inning_hit.xml", ""
#' @param ... options passed onto \code{XML2R::XML2Obs}
#' @seealso \link{XML2R::XML2Obs}
#' @return Returns a list of tables.
#' @export
#' @import XML2R
#' @import plyr
#' @importFrom lubridate days
#' @examples
#' \dontrun{
#' # Collect PITCHf/x (and other data from inning_all.xml files) from May 1st, 2012
#' dat <- scrape(start = "2013-08-01", end = "2013-08-01")
#' 
#' # OR, equivalently, use the gids option
#' data(gids)
#' dat2 <- scrape(gids=gids[grep("2012_05_01", gids)])
#'                    
#' # Collect PITCHf/x and other supporting information which scrape() will format nicely
#' files <- c("inning/inning_all.xml", "inning/inning_hit.xml",
#'              "miniscoreboard.xml", "players.xml")
#' dat3 <- scrape(start = "2012-05-01", end = "2012-05-01", 
#'                    suffix = files)
#' 
#' #scrape data from Minnesota Twins 2011 season
#' twins11 <- gids[grepl("min", gids) & grepl("2011", gids)]
#' dat <- scrape(gids=twins11, suffix = files)
#' 
#' }
#' 

scrape <- function(start, end, game.ids, suffix = "inning/inning_all.xml", ...) { 
  #check for valid inputs
  valid.suffix <- c("inning/inning_all.xml", "inning/inning_hit.xml", "miniscoreboard.xml", "players.xml")
  if (!all(suffix %in% valid.suffix)) {
    warning("Currently supported file suffix are: 'inning/inning_all.xml', 'inning/inning_hit.xml', 'miniscoreboard.xml', and 'players.xml'")
    Sys.sleep(5) #time to read warning
  }
  if (missing(game.ids)) {
    gameDir <- makeUrls(start=start, end=end)
  } else {
    if (!all(grepl("gid_", game.ids))) warning("Any Game IDs supplied to the gids option should be of the form gid_YYYY_MM_DD_xxxmlb_zzzmlb_1")
    gameDir <- makeUrls(gids=game.ids)
  }
  
  #initiate master list of tables
  master.list <- list(NULL) 
  #scrape scoreboards first since the "game" node clashes with other files
  if (any(grepl("miniscoreboard.xml", suffix))) {
    dayDir <- unique(gsub("/gid_.*", "", gameDir))
    scoreboards <- paste0(dayDir, "/miniscoreboard.xml")
    obs <- XML2Obs(scoreboards, as.equiv=TRUE, url.map=FALSE)
    # Only attributes of "game" element is kept since everything else isn't inherently useful.
    tables <- collapse(obs) #two tables
    game <- format.game(tables[["games//game"]])
    game$url_scoreboard <- game$url
    game$url <- paste0(gsub("miniscoreboard.xml", "", game$url), "gid_", game$gameday_link, "/inning/inning_all.xml")
    master.list[["game"]] <- game
  }
  
  #scrape player files next since the "game" node clashes with inning_all.xml
  if (any(grepl("players.xml", suffix))) {
    player.files <- paste0(gameDir, "/players.xml")
    #selects all the child nodes of the game element (info in game node can be linked back to scoreboards)
    obs <- XML2Obs(player.files, as.equiv=TRUE, url.map=FALSE)
    obs <- add_key(obs, parent="game//team", key.name="key_name", quiet=TRUE)
    tables <- collapse(obs) 
    #In order to easily identify which team a player/coach was on, we merge info on team level with player/coach level
    #Note 'id' field on team level conflicts with 'id' field on player level (it isn't really needed anyway)
    team <- tables[["game//team"]]
    id <- grep("id", colnames(team))
    if (length(id) > 0) team <- team[, -id] 
    player <- merged(x=tables[["game//team//player"]], y=team, by=c("url", "key_name"))
    coach <- merged(x=tables[["game//team//coach"]], y=team, by=c("url", "key_name"))
    #no longer need 'key_name'
    player <- player[, -grep("key_name", names(player), fixed=TRUE)]
    coach <- coach[, -grep("key_name", names(coach), fixed=TRUE)]
    master.list[["player"]] <- format.player(player)
    master.list[["coach"]] <- format.coach(coach)
    master.list[["umpire"]] <- format.umpire(tables[["game//umpires//umpire"]])
  }
  
  #Now scrape the inning/inning_hit.xml files
  if (any(grepl("inning/inning_hit.xml", suffix))) {
    inning.files <- paste0(gameDir, "/inning/inning_hit.xml")
    obs <- XML2Obs(inning.files, as.equiv=TRUE, url.map=FALSE)
    hip <- collapse(obs)
    master.list[["hip"]] <- format.hip(hip)
  }
  
  #Now scrape the inning/inning_all.xml files
  if (any(grepl("inning/inning_all.xml", suffix))) {
    inning.files <- paste0(gameDir, "/inning/inning_all.xml")
    obs <- XML2Obs(inning.files, as.equiv=FALSE, url.map=FALSE)
    nms <- names(obs)
    #extract the url prefix from observation names (will need this later when generating keys)
    #idea from http://stackoverflow.com/questions/2192316/extract-a-regular-expression-match-in-r-version-2-10
    exprs <- regexpr("url[0-9]+", nms)
    urlz <- substr(nms, exprs, exprs + attr(exprs, "match.length")-1)
    #"equivalent" levels of observations to be used for renaming
    action.eq <- c("game//inning//top//action", "game//inning//bottom//action")
    atbat.eq <- c("game//inning//top//atbat", "game//inning//bottom//atbat")
    pitch.eq <- c("game//inning//top//atbat//pitch", "game//inning//bottom//atbat//pitch")
    po.eq <- c("game//inning//top//atbat//po", "game//inning//bottom//atbat//po")
    runner.eq <- c("game//inning//top//atbat//runner", "game//inning//bottom//atbat//runner")
    #Ignore the urls when renaming observations
    names(obs) <- gsub("url[0-9]+//", "", nms)
    #use namez arg here to speed things up?
    obs <- re_name(obs, equiv=action.eq, diff.name="top_inning", quiet=TRUE)
    obs <- re_name(obs, equiv=atbat.eq, diff.name="top_inning", quiet=TRUE)
    obs <- re_name(obs, equiv=pitch.eq, diff.name="top_inning", quiet=TRUE)
    obs <- re_name(obs, equiv=po.eq, diff.name="top_inning", quiet=TRUE)
    obs <- re_name(obs, equiv=runner.eq, diff.name="top_inning", quiet=TRUE)
    #have to generate keys for each game individually (otherwise keys will keep incrementing - and we dont want inning=143!!!)
    obswkey <- NULL
    for (i in unique(urlz)){
      current.obs <- obs[urlz == i]
      #warnings will thrown for non inning_all.xml files, but that's OK
      current.obs <- suppressWarnings(add_key(current.obs, parent="game//inning", key.name = "inning", quiet=TRUE))
      current.obs <- suppressWarnings(add_key(current.obs, parent="game//inning//atbat", key.name = "num", quiet=TRUE))
      obswkey <- c(obswkey, current.obs)
    }
    #Free up some memory
    rm(obs)
    #poor attempt to vectorize key generation
    #obs <- tapply(obs, INDEX=urlz, add_key, parent="game//inning", key.name = "inning")
    #obs <- tapply(obs, INDEX=urlz, function(x) add_key(x, parent="game//inning//atbat", key.name = "num"))
    tables <- collapse(obswkey)
    pitch <- appendPitchCount(tables[[grep("pitch", names(tables), fixed=TRUE)]])
    atbat <- tables[["game//inning//atbat"]] #grep would have several matches here
    
    #Add batter name to 'atbats'
    scrape.env <- environment() #avoids bringing data objects into global environment
    data(players, package="pitchRx", envir=scrape.env)
    players$id <- as.character(players$id)
    colnames(atbat) <- gsub("batter", "id", colnames(atbat))
    atbat <- merged(x=atbat, y=players, by = "id")
    colnames(atbat) <- gsub("id", "batter", colnames(atbat))
    colnames(atbat) <- gsub("full_name", "batter_name", colnames(atbat))
    
    #Add pitcher name to 'atbats'
    colnames(atbat) <- gsub("pitcher", "id", colnames(atbat))
    atbat <- merged(x=atbat, y=players, by = "id")
    colnames(atbat) <- gsub("id", "pitcher", colnames(atbat))
    colnames(atbat) <- gsub("full_name", "pitcher_name", colnames(atbat))
    colnames(atbat) <- gsub("des", "atbat_des", colnames(atbat))
    
    #Note that we can ignore "game" and "game//inning"
    master.list[["action"]] <- format.action(tables[[grep("action", names(tables), fixed=TRUE)]])
    master.list[["po"]] <- format.po(tables[[grep("po", names(tables), fixed=TRUE)]])
    master.list[["runner"]] <- format.runner(tables[[grep("runner", names(tables), fixed=TRUE)]])
    master.list[["atbat"]] <- format.atbat(atbat)
    master.list[["pitch"]] <- format.pitch(pitch)
  }
#   #Scrape any left-over files in a general way (good idea?)
#   if (length(file.names) > 0) {
#     obs <- XML2Obs(file.names, ...)
#     tables <- collapse(obs)
#     un <- unique(names(obs))
#     if (length(un) == 2) {
#       master.list[[un[1]]] <- tables
#     } else {
#       master.list <- append(master.list, tables)
#     }
#   }
  #return non-null elements of master.list
  return(master.list[-(which(sapply(master.list, is.null), arr.ind=TRUE))]) 
}

#' Construct Gameday urls based on some parameters.
#' 
#' This is a convenience function (used by \link{scrape}) which constructs urls with the common
#' Gameday root \url{http://gd2.mlb.com/components/game/mlb/}. 
#' 
#' @param start date "yyyy-mm-dd" to commence scraping.
#' @param end date "yyyy-mm-dd" to terminate scraping.
#' @param gids The default value "infer" suggests gameday_links should be derived 
#' and appended appropriately (based on values of \code{start} and \code{end}). 
#' Otherwise, a character vector with gameday_links can be supplied.
#' @return Returns a character.
#' @export
#' @examples
#' 
#' # XML file names with pitch-by-pitch level data
#' prefix <- makeUrls(start="2011-04-04", end="2011-04-04")
#' paste0(prefix, "/inning/inning_all.xml")
#' # XML file names with hit location data
#' paste0(prefix, "/inning/inning_hit.xml")
#' # XML file names with game-by-game level data
#' paste0(makeUrls(start="2011-04-04", end="2011-04-04", gids=""), "/miniscoreboard.xml")
#' # Use gids option instead
#' data(gids)
#' identical(prefix, makeUrls(gids=gids[grep("2011_04_04", gids)]))
#' 
makeUrls <- function(start, end, gids="infer") {
  root <- "http://gd2.mlb.com/components/game/mlb/"
  if (all(gids %in% "infer")) {
    if (missing(start) || missing(end)) {
      warning("Can't 'infer' game urls without start/end date.")
      return(root)
    } else {
      start <- as.POSIXct(start)
      end <- as.POSIXct(end)
      env <- environment()
      data(gids, package="pitchRx", envir=env)
      last.game <- strsplit(gids[length(gids)], split="_")[[1]]
      last.date <- as.POSIXct(paste(last.game[2], last.game[3], last.game[4], sep="-"))
      if (last.date < end) gids <- c(gids, updateGids(max(start, last.date), end))
      return(gids2urls(subsetGids(gids, first=start, last=end)))
    }
  } else {
    gidz <- gids[grep("gid", gids)]
    if (length(gidz) != length(gids)) {
      #message("The option gids was ignored since some values did not contain 'gid'")
      return(paste0(root, dates2urls(as.POSIXct(start), as.POSIXct(end))))
    } else {
      return(gids2urls(gidz))
    }
  }
}

# Update Gameday IDs.
#
# This function scrapes "gameday_links" from the MLB website. 
# It should only be called when the user enters an end date later than the most recent ID
#
# @param last.date most recent date found in existing IDs
# @param end any date more recent than last.date
updateGids <- function(last.date, end) {
  message("grabbing new game IDs")
  scoreboards <- paste0(makeUrls(start=last.date, end=end, gids=""), "/miniscoreboard.xml")
  obs <- XML2Obs(scoreboards, xpath="//game[@gameday_link]")
  obs2 <- suppressMessages(rename(obs, names(obs)))
  dat <- XML2R::collapse(obs2)
  gids <- dat[,"gameday_link"]
  return(gids[!is.na(gids)])
}

#Take a start and an end date and make vector of "year_XX/month_XX/day_XX"
dates2urls <- function(first.day, last.day) {
  diff <- as.numeric(last.day - first.day)
  dates <- first.day + c(0:diff) * lubridate::days(1)
  years <- substr(dates, 0, 4)
  months <- substr(dates, 6, 7)
  days <- substr(dates, 9, 10)
  mnths <- formatC(months, width = 2, flag = "0")
  dys <- formatC(days, width = 2, flag = "0")
  paste0("year_", years, "/month_", mnths, "/day_", dys)
}

#Take a game ID and construct the appropriate url prefix
gids2urls <- function(x) {
  elements <- strsplit(x, split="_")
  urls <- sapply(elements, function(y) paste0("http://gd2.mlb.com/components/game/mlb/year_", y[2], 
                                              "/month_", y[3], "/day_", y[4], "/"))
  return(paste0(urls, x))
}

#Find the proper subset of game IDs based on start/end dates
subsetGids <- function(gids, first, last) {
  elements <- strsplit(gids, split="_")
  dates <- as.POSIXct(sapply(elements, function(x) paste(x[2], x[3], x[4], sep="-")))
  return(gids[last >= dates & dates >= first])
}

#silly function to work around stringsAsFactors=TRUE when using merge
merged <- function(x, y, by){
  dat <- merge(x=x, y=y, by=by, sort=FALSE)
  dat[] <- lapply(dat, function(x) as.character(x))
  return(dat)
}

# Series of formatting functions to coerce variables to their proper (numeric) types
format.table <- function(dat, nums) {
  dat <- data.frame(dat, stringsAsFactors=FALSE)
  numz <- nums[nums %in% names(dat)] #error handling (just in case one of the columns doesn't exist)
  for (i in numz) dat[, i] <- as.numeric(dat[, i])
  return(dat)
}
format.game <- function(dat) {
  game.nums <- c("venue_id", "scheduled_innings", "away_team_id", "away_league_id", "home_team_id", 
              "home_league_id", "away_games_back_wildcard", "away_win",  "away_loss", "home_win", 
              "home_loss", "inning", "outs", "away_team_runs","home_team_runs", "away_team_hits", 
              "home_team_hits", "away_team_errors", "home_team_errors")
  format.table(dat, nums=game.nums)
}
format.player <- function(dat) {
  player.nums <- c("id", "num", "avg", "hr", "rbi", "bat_order", "wins", "losses", "era")
  format.table(dat, nums=player.nums)
}
format.coach <- function(dat) {
  coach.nums <- c("id", "num")
  format.table(dat, nums=coach.nums)
}
format.umpire <- function(dat) {
  umpire.nums <- "id"
  format.table(dat, nums=umpire.nums)
}
format.hip <- function(dat) {
  hip.nums <- c("x", "y", "batter", "pitcher", "inning")
  format.table(dat, nums=hip.nums)
}
format.action <- function(dat) {
  action.nums <- c("b", "s", "o", "player", "pitch", "inning")
  format.table(dat, nums=action.nums)
}
format.atbat <- function(dat) {
  #atbat should already be a data.frame
  atbat.nums <- c("pitcher", "batter", "num", "b", "s", "o", "inning", "obviously wrong") #ensure bad column names don't throw error
  numz <- atbat.nums[atbat.nums %in% names(dat)] #error handling (just in case one of the columns doesn't exist)
  for (i in numz) dat[, i] <- as.numeric(dat[, i])
  return(dat)
}
format.pitch <- function(dat) {
  pitch.nums <- c("id", "x", "y", "start_speed", "end_speed", "sz_top", "sz_bot", "pfx_x", "pfx_z", "px", 
                "pz", "x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az", "type_confidence", 
                "zone", "nasty", "spin_dir", "spin_rate", "inning", "num", "on_1b", "on_2b", "on_3b")
  format.table(dat, nums=pitch.nums)
}
format.po <- function(dat) {
  po.nums <- c("inning", "num")
  format.table(dat, nums=po.nums)
}
format.runner <- function(dat) {
  runner.nums <- c("id", "inning", "num")
  format.table(dat, nums=runner.nums)
}

# Add columns with relevant pitch count to the 'pitch' table.
# @param dat 'pitch' matrix/df
# @return returns the original matrix/df with the proper pitch count column appended.
appendPitchCount <- function(dat) {
  balls <- as.numeric(dat[,"type"] == "B")
  strikes <- as.numeric(dat[,"type"] == "S")
  idx <- paste(dat[, "url"], dat[,"num"], sep="-")
  cum.balls <- unlist(tapply(balls, INDEX=idx, function(x){ n <- length(x); cumsum(c(0, x[-n])) }))
  cum.strikes <- unlist(tapply(strikes, INDEX=idx, function(x) { n <- length(x); pmin(cumsum(c(0, x[-n])), 2) }))
  count <- paste(cum.balls, cum.strikes, sep = "-")
  return(cbind(dat, count))
}

# Update \code{players} data frame
#
# This function takes on (new) player urls and scrapes personal information on each player.
#
# @param new.urls new player urls added to the \code{urls} data frame
# @return returns new player information in a data frame

# updatePlayers <- function(new.urls) {
#   cat("updating players", "\n")
#   new.players <- urlsToDataFrame(urls = new.urls, tables = list(player = c("id", "first", "last", "position")))
#   full_name <- paste(new.players$first, new.players$last, sep = " ")
#   new.players <- cbind(new.players[,c("url_player", "id")], full_name)
#   return(new.players)
# }