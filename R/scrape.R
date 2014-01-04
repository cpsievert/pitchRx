#' Scrape Major League Baseball's Gameday Data
#'
#' Function for obtaining PITCHf/x and other related Gameday Data. In theory, this function is able 
#' to extract data from any XML file under a "game directory". For example, see \url{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/}
#' for the game directory specific to the game between the Twins and the Yankees on April 4th 2011.
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
#' @param gids character vector of gameday_links. If this option is used, \code{start} and \code{end} are ignored. 
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
#' #Collect PITCHf/x and some related Gameday data for May 1st, 2012
#' files <- c("inning/inning_all.xml", "inning/inning_hit.xml",
#'              "miniscoreboard.xml", "players.xml")
#' dat <- scrape(start = "2012-05-01", end = "2012-05-01", 
#'                    suffix = files)
#' #OR equivalently
#' data(gids)
#' dat <- scrape(gids=gids[grep("2012_05_01", gids)], suffix=files)
#' 
#' #scrape data from Minnesota Twins 2011 season
#' twins11 <- gids[grepl("min", gids) & grepl("2011", gids)]
#' dat <- scrape(gids=twins11, suffix=files)
#' 
#' }
#' 

scrape <- function(start, end, gids, suffix = "inning/inning_all.xml", ...) { 
  if (missing(gids)) {
    if (missing(start)) stop("Please provide a start date. Dates should be in 'yyyy-mm-dd' format. For example, '2012-06-07' represents June 7th, 2012.")
    if (missing(end)) stop("Please provide an end date. Dates should be in 'yyyy-mm-dd' format. For example, '2012-06-07' represents June 7th, 2012.")   
    start <- as.POSIXct(start)
    end <- as.POSIXct(end)
    start.year <- as.numeric(substr(start, 0, 4))
    #if (is.null(names(tables))) stop("Please specify at least one XML node of interest.")
    if (start.year < 2005) {
      warning("Not only is pitchFX data not avaliable before 2008, data on each game isn't consistent until 2005")
      start <- as.POSIXct("2005-01-01")
    }
    if (start.year < 2008) warning("pitchFX data wasn't recorded consistently until 2008. Do you want to consider a later start date?")
    if (end > as.POSIXct(Sys.Date())) warning("Sorry, I can't scrape data on the future!")
    scrape.env <- environment()
    data(gids, package="pitchRx", envir=scrape.env)
    last.game <- strsplit(gids[length(gids)], split="_")[[1]]
    last.date <- as.POSIXct(paste(last.game[2], last.game[3], last.game[4], sep="-"))
    if (last.date < end) { #update urls if new ones exist
      new.gids <- updateGids(max(last.date, start), end)
      gids <- c(gids, new.gids)
    }
    gids <- subsetGids(gids, first=start, last=end) #Subset gids to dates of interest
  }
  
  #generate all the urls!
  prefix <- addPrefix(gids)
  file.names <- NULL
  for (i in suffix) {
    file.names <- c(file.names, paste(prefix, gids, "/", suffix, sep=""))
  }
  
  #initiate master list of tables
  master.list <- list(NULL) 
  #scrape scoreboards first since the "game" node clashes with other files
  scores <- grepl("miniscoreboard.xml", file.names)
  if (sum(scores) > 0){
    scoreboards <- file.names[scores] 
    file.names <- file.names[!scores]
    obs <- XML2Obs(scoreboards, as.equiv=TRUE)
    # Only attributes of "game" tag is kept since everything else is post-processing inning_all.xml files.
    url.map <- obs[["url_map"]]
    obs <- obs[names(obs) == "game"] 
    game <- collapse(obs) #one table
    game <- cbind(game, url=url.map)
    key <- grep("url_key", colnames(game))
    if (length(key) > 0) game <- game[, -key]
    master.list[["game"]] <- game
  }
  #scrape player files next since the "game" node clashes with inning_all.xml
  playerz <- grepl("players.xml", file.names)
  if (sum(playerz) > 0){
    player.files <- file.names[playerz] 
    file.names <- file.names[!playerz]
    #selects all the child nodes of the game element (info in game node can be linked back to scoreboards)
    obs <- XML2Obs(player.files, as.equiv=TRUE)
    obs <- add_key(obs, parent="game//team", quiet=TRUE)
    tables <- collapse(obs) 
    #In order to easily identify which team a player/coach was on, we merge info on team level with player/coach level
    #Note 'id' field on team level conflicts with 'id' field on player level (it isn't really needed anyway)
    team <- tables[["game//team"]]
    id <- grep("id", colnames(team))
    if (length(id) > 0) team <- team[, -id] 
    player <- merged(x=tables[["game//team//player"]], y=team, by=c("url_key", "key_name"))
    coach <- merged(x=tables[["game//team//coach"]], y=team, by=c("url_key", "key_name"))
    umpire <- tables[["game//umpires//umpire"]]
    url.map <- obs[["url_map"]]
    url.map <- cbind(url_key=names(url.map), url=url.map)
    player <- merged(x=player, y=url.map, by="url_key")
    master.list[["player"]] <- player
    master.list[["coach"]] <- coach
    master.list[["umpire"]] <- umpire
  }
  
  #Now scrape the inning/inning_all.xml files
  inning.all <- grepl("inning_all.xml", file.names)
  if (sum(inning.all) > 0) {
    inning.files <- file.names[inning.all] 
    file.names <- file.names[!inning.all]
    obs <- XML2Obs(inning.files, as.equiv=FALSE)
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
    unique.urls <- names(obs[["url_map"]])
    for (i in unique.urls){
      current.obs <- obs[which(urlz == i)]
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
    atbat <- tables[["game//inning//atbat"]] #dangerous to grep here
    #Add batter name to 'atbats'
    scrape.env <- environment()
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
    master.list[["action"]] <- tables[[grep("action", names(tables), fixed=TRUE)]]
    master.list[["po"]] <- tables[[grep("po", names(tables), fixed=TRUE)]]
    master.list[["runner"]] <- tables[[grep("runner", names(tables), fixed=TRUE)]]
    master.list[["atbat"]] <- atbat
    master.list[["pitch"]] <- pitch
  }
  #Scrape any left-over files in a general way
  if (length(file.names) > 0) {
    obs <- XML2Obs(file.names, ...)
    tables <- collapse(obs)
    un <- unique(names(obs))
    if (length(un) == 2) {
      master.list[[un[1]]] <- tables
    } else {
      master.list <- append(master.list, tables)
    }
  }
  #return non-null elements of master.list
  return(master.list[-(which(sapply(master.list, is.null), arr.ind=TRUE))]) 
}

#silly function to work around stringsAsFactors=TRUE when using merge
merged <- function(x, y, by){
  dat <- merge(x=x, y=y, by=by, sort=FALSE)
  dat[] <- lapply(dat, function(x) as.character(x))
  return(dat)
}

# Update Gameday IDs
#
# This function scrapes "gameday_links" from the MLB website. 
# It should only be called when the user enters an end date later than the most recent ID
#
# @param last.date most recent date found in existing IDs
# @param end any date more recent than last.date

updateGids <- function(last.date, end) {
  cat("grabbing new game IDs", "\n")
  diff <- as.numeric(end - last.date)
  dates <- last.date + c(0:diff) * days(1)
  years <- substr(dates, 0, 4)
  months <- substr(dates, 6, 7)
  days <- substr(dates, 9, 10)
  mnths <- formatC(months, width = 2, flag = "0")
  dys <- formatC(days, width = 2, flag = "0")
  scoreboards <- as.list(paste("http://gd2.mlb.com/components/game/mlb/year_", years, 
                               "/month_", mnths, "/day_", dys, "/", "miniscoreboard.xml", sep = ""))
  obs <- XML2Obs(scoreboards, xpath="//game[@gameday_link]")
  obs2 <- suppressMessages(rename(obs, names(obs)))
  dat <- XML2R::collapse(obs2)
  gids <- dat[,"gameday_link"]
  return(gids[!is.na(gids)])
}

#Find the proper subset of game IDs based on start/end dates
subsetGids <- function(gids, first, last) {
  elements <- strsplit(gids, split="_")
  dates <- as.POSIXct(sapply(elements, function(x) paste(x[2], x[3], x[4], sep="-")))
  return(gids[last >= dates & dates >= first])
}

#Take a game ID and add the appropriate prefix
addPrefix <- function(x) {
  elements <- strsplit(x, split="_")
  urls <- sapply(elements, function(x) paste("http://gd2.mlb.com/components/game/mlb/year_", x[2], 
                                              "/month_", x[3], "/day_", x[4], "/", sep = ""))
  return(urls)
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

# Add columns with relevant pitch count to the 'pitch' table.
# @param dat 'pitch' table
# @return returns the original object with the proper pitch count column appended.

appendPitchCount <- function(dat) {
  balls <- as.numeric(dat[,"type"] == "B")
  strikes <- as.numeric(dat[,"type"] == "S")
  idx <- paste(dat[, "url_key"], dat[,"num"], sep="-")
  cum.balls <- unlist(tapply(balls, INDEX=idx, function(x){ n <- length(x); cumsum(c(0, x[-n])) }))
  cum.strikes <- unlist(tapply(strikes, INDEX=idx, function(x) { n <- length(x); pmin(cumsum(c(0, x[-n])), 2) }))
  count <- paste(cum.balls, cum.strikes, sep = "-")
  return(cbind(dat, count))
}
