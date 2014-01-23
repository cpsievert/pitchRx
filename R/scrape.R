#' Scrape Major League Baseball's Gameday Data
#'
#' Function for obtaining PITCHf/x and other related Gameday Data. If values for \code{start} and \code{end} 
#' are supplied, then only relevant directories will be considered. For example, if \code{start="2011-04-04"}
#' and \code{end="2011-04-05"}, then files under the \url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/}
#' and \url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_05/} homepages will be considered. 
#' When \code{start} and \code{end} are supplied, \code{scrape} will automatically find and append necessary 
#' gameday_links to urls. 
#' For example, 
#' \url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/}
#' would be one of many games played on April 4th 2011. 
#' 
#' On the other hand, the \code{game.ids} argument allows one to query specific games rather than 
#' all games within a window of dates.
#' 
#' This function (currently) has special handling for files ending with:
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/players.xml}{players.xml},
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/miniscoreboard.xml}{miniscoreboard.xml},
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/inning/inning_all.xml}{inning/inning_all.xml}, and
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/inning/inning_hit.xml}{inning/inning_hit.xml}
#' 
#' 
#' @param start date "yyyy-mm-dd" to commence scraping.
#' @param end date "yyyy-mm-dd" to terminate scraping.
#' @param game.ids character vector of gameday_links. If this option is used, \code{start} and \code{end} are ignored. 
#' See \code{data(gids, package="pitchRx")} for examples.
#' @param suffix character vector with suffix of the XML files to be parsed. Currently supported options are: 
#' 'players.xml', 'miniscoreboard.xml', 'inning_all.xml', 'inning_hit.xml'.
#' @param connect A database connection object. The class of the object should be "MySQLConnection" or "SQLiteConnection".
#' If a valid connection is supplied, tables will be copied to the database, which will result in better memory management.
#' If a connection is supplied, but the connection fails for some reason, csv files will be written to the working directory.
#' @seealso \code{XML2R::XML2Obs}
#' @return Returns a list of data frames.
#' @export
#' @import XML2R
#' @importFrom lubridate days
#' @importFrom plyr try_default
#' @examples
#' \dontrun{
#' # Collect PITCHf/x (and other data from inning_all.xml files) from May 1st, 2012
#' dat <- scrape(start = "2013-08-01", end = "2013-08-01")
#' # OR, equivalently, use the gids option
#' data(gids)
#' dat2 <- scrape(game.ids=gids[grep("2012_05_01", gids)])
#' 
#' #Create SQLite database, then collect and store data in that database
#' library(dplyr)
#' my_db <- src_sqlite("my_db.sqlite3", create=T)
#' scrape(start = "2013-08-01", end = "2013-08-01", connect=my_db$con)
#' 
#' #simple example of a common query
#' pit <- inner_join(tbl(my_db, "pitches"), tbl(my_db, "atbats"))
#' pit$query #analyze query (do you really want all these columns?)
#' pitchfx <- collect(pit) #submit query and bring data into R
#' 
#' 
#' # Collect PITCHf/x and other supporting information which scrape() will format nicely
#' files <- c("inning/inning_all.xml", "inning/inning_hit.xml",
#'              "miniscoreboard.xml", "players.xml")
#' dat3 <- scrape(start = "2012-05-01", end = "2012-05-01", 
#'                    suffix = files)
#'                    
#' #scrape PITCHf/x from Minnesota Twins 2011 season
#' twins11 <- gids[grepl("min", gids) & grepl("2011", gids)]
#' dat <- scrape(game.ids=twins11)
#' 
#' }
#' 

scrape <- function(start, end, game.ids, suffix = "inning/inning_all.xml", connect) { 
  #check for valid file inputs
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
  
  if (!missing(connect)) {
    if (!require(DBI)) warning("You will need the DBI package to write tables to a database.")
    #DBI::dbWriteTable method only works for these two classes
    valid.conn <- c("MySQLConnection", "SQLiteConnection")
    if (!class(connect) %in% valid.conn) warning("You need either a MySQLConnection or SQLiteConnection.")
#SMART PREVENTION OF APPENDING DATA TWICE MIGHT GET MESSY (HOW DO I CHECK EVERY TYPE OF FILE IN A NICE WAY???)
#     DBTables <- dbListTables(connect)
#     #should I try tables until this is non-empty?
#     existing.urls <- gsub("/inning/inning_all.xml", "", 
#                           as.character(dbGetQuery(connect, "SELECT DISTINCT(urls) FROM atbats")))
#     idx <- gameDir %in% existing.urls
#     if (any(idx)) {
#       warning("I detected urls in your database that match your query! I will not be scraping these files")
#     }
  }

  #scrape scoreboards first since the "game" node clashes with other files
  if (any(grepl("miniscoreboard.xml", suffix))) {
    dayDir <- unique(gsub("/gid_.*", "", gameDir))
    scoreboards <- paste0(dayDir, "/miniscoreboard.xml")
    obs <- XML2Obs(scoreboards, as.equiv=TRUE, url.map=FALSE)
    nms <- names(obs)
    #simplify names
    nms <- gsub("^games//game//game_media//media$", "media", nms)
    nms <- gsub("^games//game$", "game", nms)
    obs <- setNames(obs, nms)
    #"games" observations are not informative -- no longer need them
    game.idx <- grep("^games$", nms)
    if (length(game.idx) > 0) obs <- obs[-game.idx] 
    tables <- collapse_obs(obs) #two tables
    #Coerce matrices to data frames; turn appropriate variables into numerics
    tables[["game"]] <- format.game(tables[["game"]])
    tables[["media"]] <- data.frame(tables[["media"]])
    #Try to write to database, 
    if (!missing(connect)) {
#       game.success <- export(connect, value=game, name="games")
#       media.success <- export(connect, value=media, name="medias")
      for (i in names(tables)) export(connect, name=i, value=tables[[i]])
      rm(obs)
      rm(tables)
      message("Collecting garbage")
      gc() 
    }
  }
  
  #scrape player files next since the "game" node clashes with inning_all.xml
  if (any(grepl("players.xml", suffix))) {
    player.files <- paste0(gameDir, "/players.xml")
    #selects all the child nodes of the game element (info in game node can be linked back to scoreboards)
    obs <- XML2Obs(player.files, as.equiv=TRUE, url.map=FALSE)
    #recycle information on the team level (there are two per file)
    obs <- add_key(obs, parent="game//team", recycle="id", key.name="name_abbrev", quiet=TRUE)
    obs <- add_key(obs, parent="game//team", recycle="type", quiet=TRUE)
    obs <- add_key(obs, parent="game//team", recycle="name", quiet=TRUE)
    nms <- names(obs)
    #simplify names
    nms <- gsub("^game//team//player$", "player", nms)
    nms <- gsub("^game//team//coach$", "coach", nms)
    nms <- gsub("^game//umpires//umpire$", "umpire", nms)
    obs <- setNames(obs, nms)
    #no longer need the 'game' and 'game//team' observations
    game.idx <- grep("game", nms)
    if (length(game.idx) > 0) obs <- obs[-game.idx]
    if (exists("tables")){
      tables <- c(tables, collapse_obs(obs))
    } else {
      tables <- collapse_obs(obs)
    }
    #Coerce matrices to data frames; turn appropriate variables into numerics
    tables[["player"]] <- format.player(tables[["player"]])
    tables[["coach"]] <- format.coach(tables[["coach"]])
    tables[["umpire"]] <- format.umpire(tables[["umpire"]])
    if (!missing(connect)) {
      #Try to write to database, if that fails, write csv. Then clear up memory
#       player.success <- export(connect, value=tables[["player"]], name="players")
#       umpire.success <- export(connect, value=tables[["umpires"]], name="umpires")
#       coach.success <- export(connect, value=tables[["coaches"]], name="coaches")
      for (i in names(tables)) export(connect, name=i, value=tables[[i]])
      rm(obs)
      rm(tables)
      message("Collecting garbage")
      gc() 
    }
  }
  
  #Now scrape the inning/inning_hit.xml files
  if (any(grepl("inning/inning_hit.xml", suffix))) {
    inning.files <- paste0(gameDir, "/inning/inning_hit.xml")
    obs <- XML2Obs(inning.files, as.equiv=TRUE, url.map=FALSE)
    if (exists("tables")){
      tables <- c(tables, list(hip=collapse_obs(obs))) #only one table
    } else {
      tables <- list(hip=collapse_obs(obs))
    }
    #Coerce matrix to data frame; turn appropriate variables into numerics
    tables[["hip"]] <- format.hip(tables[["hip"]]) 
    if (!missing(connect)) {
      #Try to write to database, if that fails, write csv. Then clear up memory
      export(connect, name="hip", value=tables[["hip"]])
      rm(obs)
      rm(tables)
      message("Collecting garbage")
      gc() 
    }
  }
  
  #Now scrape the inning/inning_all.xml files
  if (any(grepl("inning/inning_all.xml", suffix))) {
    inning.files <- paste0(gameDir, "/inning/inning_all.xml")
    #cap the number of files to be parsed at once (helps avoid exhausting memory)
    cap <- 50
    n.files <- length(inning.files)
    n.loops <- ceiling(n.files/cap)
    for (i in seq_len(n.loops)) {
      #grab subset of files to be parsed
      inning.filez <- inning.files[seq(1, cap)+(i-1)*cap]
      obs <- XML2Obs(inning.filez, as.equiv=TRUE, url.map=FALSE)
      obs <- re_name(obs, equiv=c("game//inning//top//atbat//pitch", 
                                  "game//inning//bottom//atbat//pitch"), diff.name="inning_side", quiet=TRUE) 
      obs <- re_name(obs, equiv=c("game//inning//top//atbat//runner", 
                                  "game//inning//bottom//atbat//runner"), diff.name="inning_side", quiet=TRUE)   
      obs <- re_name(obs, equiv=c("game//inning//top//atbat//po", 
                                  "game//inning//bottom//atbat//po"), diff.name="inning_side", quiet=TRUE) 
      obs <- re_name(obs, equiv=c("game//inning//top//atbat", 
                                  "game//inning//bottom//atbat"), diff.name="inning_side", quiet=TRUE) 
      obs <- re_name(obs, equiv=c("game//inning//top//action", 
                                  "game//inning//bottom//action"), diff.name="inning_side", quiet=TRUE) 
      obs <- add_key(obs, parent="game//inning", recycle="num", key.name="inning", quiet=TRUE) 
      obs <- add_key(obs, parent="game//inning", recycle="next", quiet=TRUE)
      #trick to make add_key think 'actions' are a descendant of 'atbat' (they really are in a way) -- so that we can link the two.
      names(obs) <- sub("^game//inning//action$", "game//inning//atbat//action", names(obs))
      obs <- add_key(obs, parent="game//inning//atbat", recycle="num", quiet=TRUE)
      #no longer need the 'game' and 'game//team' observations
      nms <- names(obs)
      rm.idx <- c(grep("^game$", nms), grep("^game//inning$", nms))
      if (length(rm.idx) > 0) obs <- obs[-rm.idx]
      if (exists("tables")){
        tables <- c(tables, collapse_obs(obs))
      } else {
        tables <- collapse_obs(obs)
      }
      #Free up some memory
      rm(obs)
      gc()
      #simplify table names
      tab.nms <- names(tables)
      tab.nms <- sub("^game//inning//atbat$", "atbat", tab.nms)
      tab.nms <- sub("^game//inning//atbat//action$", "action", tab.nms)
      tab.nms <- sub("^game//inning//atbat//po$", "po", tab.nms)
      tab.nms <- sub("^game//inning//atbat//runner$", "runner", tab.nms)
      tab.nms <- sub("^game//inning//atbat//pitch$", "pitch", tab.nms)
      tables <- setNames(tables, tab.nms)
      
      #Add batter name to 'atbats'
      scrape.env <- environment() #avoids bringing data objects into global environment
      data(players, package="pitchRx", envir=scrape.env)
      players$id <- as.character(players$id)
      colnames(tables[["atbat"]]) <- gsub("batter", "id", colnames(tables[["atbat"]]))
      tables[["atbat"]] <- merged(x=tables[["atbat"]], y=players, by = "id")
      colnames(tables[["atbat"]]) <- gsub("id", "batter", colnames(tables[["atbat"]]))
      colnames(tables[["atbat"]]) <- gsub("full_name", "batter_name", colnames(tables[["atbat"]]))
      #Add pitcher name to 'atbats'
      colnames(tables[["atbat"]]) <- gsub("pitcher", "id", colnames(tables[["atbat"]]))
      tables[["atbat"]] <- merged(x=tables[["atbat"]], y=players, by = "id")
      colnames(tables[["atbat"]]) <- gsub("id", "pitcher", colnames(tables[["atbat"]]))
      colnames(tables[["atbat"]]) <- gsub("full_name", "pitcher_name", colnames(tables[["atbat"]]))
      colnames(tables[["atbat"]]) <- gsub("des", "atbat_des", colnames(tables[["atbat"]]))
      
      #Note that we can ignore "game" and "game//inning"
      tables[["action"]] <- format.action(tables[["action"]])
      tables[["po"]] <- format.po(tables[["po"]])
      tables[["runner"]] <- format.runner(tables[["runner"]])
      tables[["atbat"]] <- format.atbat(tables[["atbat"]])
      #generate a "count" column from "b" (balls) & "s" (strikes)
      tables[["pitch"]] <- format.pitch(appendPitchCount(tables[["pitch"]]))
      if (!missing(connect)) {
        #Try to write tables to database, if that fails, write csv. Then clear up memory
        for (i in names(tables)) export(connect, name=i, value=tables[[i]])
        rm(tables)
        message("Collecting garbage")
        gc() 
      }
    }
  }
  if (exists("tables")) {
    return(tables)
  } else {
    #Should I return the connection or something else instead?
    return(NULL)
  }
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
      #need to rework this guy
      #if (last.date < end) gids <- c(gids, updateGids(max(start, last.date), end))
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

#Try to append table to an existing database and return FALSE if error
export <- function(connect, name, value) {
  #NOTE TO SELF: find complete list of fields and force tables to have that set (it's easier to append a smaller df to larger table -- fill with NAs b4 appending!)
  #plyr::try_default(DBI::dbListFields(connect, name), default=FALSE, quiet=TRUE)
  #res <- dbSendQuery(my_db$con, paste("SELECT * FROM", name, "LIMIT 1"))
  success <- plyr::try_default(DBI::dbWriteTable(conn=connect, name=name, value=value, 
                                                 append=TRUE, overwrite=FALSE, row.names=FALSE), 
                               default=FALSE, quiet=TRUE)
  if (success) {
    message(paste("Successfully copied", name, "table to database connection."))
  } else {
    file.name <- paste0(name, "-", Sys.Date(), ".csv")
    message(paste("Failed to copy", name, "table to database connection. Writing", file.name, "instead."))
    write.csv(value, file=file.name, row.names=FALSE)
  }
  #rm(value, inherits=TRUE)
  return(success)
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
  obs2 <- suppressMessages(re_name(obs, names(obs)))
  dat <- collapse_obs(obs2)
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
  for (i in numz) dat[, i] <- suppressWarnings(as.numeric(dat[, i]))
  return(dat)
}
format.game <- function(dat) {
  game.nums <- c("venue_id", "scheduled_innings", "away_team_id", "away_league_id", "home_team_id", 
              "home_league_id", "away_games_back_wildcard", "away_win",  "away_loss", "home_win", 
              "home_loss", "inning", "outs", "away_team_runs","home_team_runs", "away_team_hits", 
              "home_team_hits", "away_team_errors", "home_team_errors")
  game <- format.table(dat, nums=game.nums)
  game$url_scoreboard <- game$url
  game$url <- paste0(gsub("miniscoreboard.xml", "", game$url), "gid_", game$gameday_link, "/inning/inning_all.xml")
  return(game)
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