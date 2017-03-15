#' Scrape Major League Baseball's Gameday Data
#'
#' Function for obtaining PITCHf/x and other related Gameday Data. \code{scrape} currently has support for files ending with:
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/inning/inning_all.xml}{inning/inning_all.xml},
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/inning/inning_hit.xml}{inning/inning_hit.xml},
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/players.xml}{players.xml}, or
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/miniscoreboard.xml}{miniscoreboard.xml}.
#' It's worth noting that PITCHf/x is contained in files ending with "inning/inning_all.xml", but the other files can complement this data depending on the goal for analysis.
#' Any collection of file names may be passed to the \code{suffix} argument, and \code{scrape} will retrieve data from a (possibly large number)
#' of files based on either a window of dates or a set of \code{game.ids}.
#' If collecting data in bulk, it is strongly recommended that one establishes a database connection and supplies the
#' connection to the \code{connect} argument. See the examples section for a simple example of how to do so.
#'
#' @note This function was adapted from \code{scrapeFX} which is deprecated as of version 1.0
#' @param start character string specifying a date "yyyy-mm-dd" to commence scraping.
#' @param end character string specifying a date "yyyy-mm-dd" to terminate scraping.
#' @param game.ids character vector of gameday_links. If this option is used, \code{start} and \code{end} are ignored.
#' See \code{data(gids, package="pitchRx")} for examples.
#' @param suffix character vector with suffix of the XML files to be parsed. Currently supported options are:
#' 'players.xml', 'miniscoreboard.xml', 'inning/inning_all.xml', 'inning/inning_hit.xml'.
#' @param connect A database connection object. The class of the object should be "MySQLConnection" or "SQLiteConnection".
#' If a valid connection is supplied, tables will be copied to the database, which will result in better memory management.
#' If a connection is supplied, but the connection fails for some reason, csv files will be written to the working directory.
#' @param ... arguments passed onto \code{XML2R::XML2Obs}. Among other things, this can be used to switch on asynchronous downloads.
#' @seealso If you want to add support for more file types, the \code{XML2R} package is a good place to start.
#' @return Returns a list of data frames (or nothing if writing to a database).
#' @export
#' @import XML2R
#' @examples
#' \dontrun{
#' # Collect PITCHf/x (and other data from inning_all.xml files) from
#' # all games played on August 1st, 2013 (using asynchronous downloads)
#' dat <- scrape(start = "2013-08-01", end = "2013-08-01")
#' #As of XML2R 0.0.5, asyncronous downloads can be performed
#' dat <- scrape(start = "2013-08-01", end = "2013-08-01", async = TRUE)
#'
#' # Scrape PITCHf/x from Minnesota Twins 2011 season
#' data(gids, package = "pitchRx")
#' twins11 <- gids[grepl("min", gids) & grepl("2011", gids)]
#' dat <- scrape(game.ids = twins11[1]) #scrapes 1st game only
#'
#' data(nonMLBgids, package = "pitchRx")
#' # Grab IDs for triple A games on June 1st, 2011
#' # This post explains more about obtaining game IDs with regular expressions --
#' # http://baseballwithr.wordpress.com/2014/06/30/pitchrx-meet-openwar-4/
#' aaa <- nonMLBgids[grepl("2011_06_01_[a-z]{3}aaa_[a-z]{3}aaa", nonMLBgids)]
#' dat <- scrape(game.ids = aaa)
#'
#' # Create SQLite database, then collect and store data in that database
#' library(dplyr)
#' my_db <- src_sqlite("Gameday.sqlite3")
#' scrape(start = "2013-08-01", end = "2013-08-01", connect = my_db$con)
#'
#' # Collect other data complementary to PITCHf/x and store in database
#' files <- c("inning/inning_hit.xml", "miniscoreboard.xml", "players.xml")
#' scrape(start = "2013-08-01", end = "2013-08-01", connect=my_db$con, suffix = files)
#'
#' # Simple example to demonstrate database query using dplyr
#' # Note that 'num' and 'gameday_link' together make a key that allows us to join these tables
#' locations <- select(tbl(my_db, "pitch"), px, pz, des, num, gameday_link)
#' names <- select(tbl(my_db, "atbat"), pitcher_name, batter_name, num, gameday_link)
#' que <- inner_join(locations, filter(names, batter_name == "Paul Goldschmidt"),
#'                    by = c("num", "gameday_link"))
#' que$query #refine sql query if you'd like
#' pitchfx <- collect(que) #submit query and bring data into R
#'
#' }
#'

scrape <- function(start, end, game.ids, suffix = "inning/inning_all.xml", connect, ...) {
  # Run some checks to make sure we can append to the database connection
  # Also, try to append a 'date' column to the 'atbat' table (if it's missing)
  if (!missing(connect)) {
    if (!requireNamespace('DBI')) warning("You will need the DBI package to write tables to a database.")
    fieldz <- plyr::try_default(DBI::dbListFields(connect, "atbat"), NULL, quiet = TRUE)
    if (!"date" %in% fieldz && !is.null(fieldz)) {
      msg <- "An 'atbat' table without the 'date' column was detected\n"
      if (!requireNamespace('dplyr') || packageVersion("dplyr") < 0.2) {
        message(msg, "To automatically append 'date', please install/update the dplyr and DBI packages \n",
                "More details are discussed here -- \n",
                "http://baseballwithr.wordpress.com/2014/04/13/modifying-and-querying-a-pitchfx-database-with-dplyr/")
      } else {
        message(msg, "A 'date' column will now be appended. Please be patient.")
        new.col <- if ("SQLiteConnection" %in% class(connect)) {
          if ("gameday_link" %in% fieldz) "SUBSTR(gameday_link, 15, -10)" else "SUBSTR(url, 80, -10)"
        } else {
          if ("gameday_link" %in% fieldz) "SUBSTR(gameday_link, 5, 10)" else "SUBSTR(url, 70, 10)"
        }
        res <- DBI::dbSendQuery(connect, paste("CREATE TABLE atbat_temp AS SELECT *,", new.col, "AS date FROM atbat"))
        DBI::dbRemoveTable(connect, name = 'atbat')
        DBI::dbSendQuery(connect, 'ALTER TABLE atbat_temp RENAME TO atbat')
      }
    }
  }
  #check for valid file inputs
  message("If file names don't print right away, please be patient.")
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

  #SMART PREVENTION OF APPENDING SAME DATA MIGHT GET MESSY (HOW DO I CHECK EVERY TYPE OF FILE IN A NICE WAY???)
  #     DBTables <- dbListTables(connect)
  #     #should I try tables until this is non-empty?
  #     existing.urls <- gsub("/inning/inning_all.xml", "",
  #                           as.character(dbGetQuery(connect, "SELECT DISTINCT(urls) FROM atbats")))
  #     idx <- gameDir %in% existing.urls
  #     if (any(idx)) {
  #       warning("I detected urls in your database that match your query! I will not be scraping these files")
  #     }

  # upload fields so we have table templates (for exporting to database)
  fields = NULL # happy BDR?
  env2 <- environment()
  data(fields, package="pitchRx", envir=env2)

  #scrape scoreboards first since the "game" node clashes with other files
  if (any(grepl("miniscoreboard.xml", suffix))) {
    dayDir <- unique(gsub("/gid_.*", "", gameDir))
    scoreboards <- paste0(dayDir, "/miniscoreboard.xml")
    obs <- XML2Obs(scoreboards, as.equiv=TRUE, url.map=FALSE, ...)
    # These observations typically show up *before* the game is played
    # I'm not so sure I want to support them....
    illegal <- paste0("games//game//", c("review","home_probable_pitcher", "away_probable_pitcher"))
    obs <- obs[!names(obs) %in% illegal]
    nms <- names(obs)
    #simplify names
    nms <- gsub("^games//game//game_media//media$", "media", nms)
    nms <- gsub("^games//game$", "game", nms)
    obs <- setNames(obs, nms)
    #"games" observations are not informative -- no longer need them
    game.idx <- grep("^games$", nms)
    if (length(game.idx) > 0) obs <- obs[-game.idx]
    tables <- collapse_obs2(obs)
    #Coerce matrices to data frames; turn appropriate variables into numerics
    for (i in names(tables)) tables[[i]] <- format.table(tables[[i]], name=i)
    if (!missing(connect)) {
      #Try to write tables to database, if that fails, write to csv. Then clear up memory
      for (i in names(tables)) export(connect, name = i, value = tables[[i]], template = fields[[i]])
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
    obs <- XML2Obs(player.files, as.equiv=TRUE, url.map=FALSE, ...)
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
      tables <- c(tables, collapse_obs2(obs))
    } else {
      tables <- collapse_obs2(obs)
    }
    #Coerce matrices to data frames; turn appropriate variables into numerics
    for (i in names(tables)) tables[[i]] <- format.table(tables[[i]], name=i)
    if (!missing(connect)) {
      #Try to write tables to database, if that fails, write to csv. Then clear up memory
      for (i in names(tables)) export(connect, name = i, value = tables[[i]], template = fields[[i]])
      rm(obs)
      rm(tables)
      message("Collecting garbage")
      gc()
    }
  }

  #Now scrape the inning/inning_hit.xml files
  if (any(grepl("inning/inning_hit.xml", suffix))) {
    inning.files <- paste0(gameDir, "/inning/inning_hit.xml")
    obs <- XML2Obs(inning.files, as.equiv=TRUE, url.map=FALSE, ...)
    if (exists("tables")){
      tables <- c(tables, collapse_obs2(obs)) #only one table
    } else {
      tables <- collapse_obs2(obs)
    }
    names(tables) <- sub("^hitchart//hip$", "hip", names(tables))
    #Coerce matrices to data frames; turn appropriate variables into numerics
    for (i in names(tables)) tables[[i]] <- format.table(tables[[i]], name=i)
    if (!missing(connect)) {
      #Try to write tables to database, if that fails, write to csv. Then clear up memory
      for (i in names(tables)) export(connect, name = i, value = tables[[i]], template = fields[[i]])
      rm(obs)
      rm(tables)
      message("Collecting garbage")
      gc()
    }
  }

  #Now scrape the inning/inning_all.xml files
  if (any(grepl("inning/inning_all.xml", suffix))) {

    #If there are MLB and non-MLB gids in a set, we need to separate so each can be passed to its own method
    MLB.gids <- game.ids[grep("mlb", game.ids)]
    nonMLB.gids <- game.ids[-grep("mlb", game.ids)]

    #Check to see if the gids are MLB
    if (all(substr(MLB.gids, nchar(MLB.gids)-4, nchar(MLB.gids)-2)=="mlb")) {
      inning.files <- paste0(gameDir, "/inning/inning_all.xml")
      n.files <- length(inning.files)
      #cap the number of files to be parsed at once (helps avoid exhausting memory)
      cap <- min(200, n.files)
      if (n.files > cap && missing(connect)) {
        warning("play-by-play data for just the first 200 games will be returned (even though you've asked for", n.files, ")",
                "If you want/need more, please consider using the 'connect' argument.")
      }
      n.loops <- ceiling(n.files/cap)
      for (i in seq_len(n.loops)) {
        #grab subset of files to be parsed
        inning.filez <- inning.files[seq(1, cap)+(i-1)*cap]
        inning.filez <- inning.filez[!is.na(inning.filez)]
        obs <- XML2Obs(inning.filez, as.equiv=TRUE, url.map=FALSE, ...)
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
        obs <- add_key(obs, parent="game//inning", recycle="next", key.name="next_", quiet=TRUE)
        #trick to make add_key think 'actions' are a descendant of 'atbat' (they really are in a way) -- so that we can link the two.
        names(obs) <- sub("^game//inning//action$", "game//inning//atbat//action", names(obs))
        obs <- add_key(obs, parent="game//inning//atbat", recycle="num", quiet=TRUE)
        #no longer need the 'game' and 'game//inning' observations
        nms <- names(obs)
        rm.idx <- c(grep("^game$", nms), grep("^game//inning$", nms))
      }
    }

    #Check to see if gids are non-MLB
    if (all(substr(nonMLB.gids, nchar(nonMLB.gids)-4, nchar(nonMLB.gids)-2)!="mlb")) {
      # Define some empty lists to be used in the loop.
      inningValz <- list(); finalValz <- list(); gamzList <- list();
      # Read lines of each game directory from gameDir.
      for (i in 1:length(gameDir)) {
        gamzList[[i]] <- readLines(paste0(gameDir[i], "/inning"))
        # We have to find the number of innings played. We'll assume 30 just to be safe.
        # For each inning we find in the gamzList, append the correct base URL from gameDir.
        for(x in 1:30) {
          if (isTRUE(any(grepl(paste0("inning_", x, ".xml", sep="", collapse="|"), gamzList[[i]])))) {
            inningValz[[x]] <- paste0(gameDir[i], "/inning/inning_", x, ".xml", collapse="|")
            finalValz[[i]] <- inningValz
          }
        }
      }
      # Final list of game URLs.
      inning.files <- unlist(finalValz)
      # Remove uneeded lists.
      rm(inningValz, finalValz, gamzList)
      n.files <- length(inning.files)
      #cap the number of files to be parsed at once (helps avoid exhausting memory)
      cap <- min(200, n.files)
      if (n.files > cap && missing(connect)) {
        warning("play-by-play data for just the first 200 games will be returned (even though you've asked for", n.files, ")",
                "If you want/need more, please consider using the 'connect' argument.")
      }
      n.loops <- ceiling(n.files/cap)
      for (i in seq_len(n.loops)) {
        #grab subset of files to be parsed
        inning.filez <- inning.files[seq(1, cap)+(i-1)*cap]
        inning.filez <- inning.filez[!is.na(inning.filez)]
        obs <- XML2Obs(inning.filez, as.equiv=TRUE, url.map=FALSE)
        obs <- re_name(obs, equiv=c("inning//top//atbat//pitch",
                                    "inning//bottom//atbat//pitch"), diff.name="inning_side", quiet=TRUE)
        obs <- re_name(obs, equiv=c("inning//top//atbat//runner",
                                    "inning//bottom//atbat//runner"), diff.name="inning_side", quiet=TRUE)
        obs <- re_name(obs, equiv=c("inning//top//atbat//po",
                                    "inning//bottom//atbat//po"), diff.name="inning_side", quiet=TRUE)
        obs <- re_name(obs, equiv=c("inning//top//atbat",
                                    "inning//bottom//atbat"), diff.name="inning_side", quiet=TRUE)
        obs <- re_name(obs, equiv=c("inning//top//action",
                                    "inning//bottom//action"), diff.name="inning_side", quiet=TRUE)
        obs <- add_key(obs, parent="inning", recycle="num", key.name="inning", quiet=TRUE)
        obs <- add_key(obs, parent="inning", recycle="next", key.name="next_", quiet=TRUE)
        #trick to make add_key think 'actions' are a descendant of 'atbat' (they really are in a way) -- so that we can link the two.
        names(obs) <- sub("^inning//action$", "inning//atbat//action", names(obs))
        obs <- add_key(obs, parent="inning//atbat", recycle="num", quiet=TRUE)
        #no longer need the 'inning' and 'game//inning' observations
        nms <- names(obs)
        rm.idx <- c(grep("^inning$", nms), grep("^game//inning$", nms))
      }
    }
      if (length(rm.idx) > 0) obs <- obs[-rm.idx]
      if (exists("tables")){
        tables <- c(tables, collapse_obs2(obs))
      } else {
        tables <- collapse_obs2(obs)
      }
      #Free up some memory
      rm(obs)
      gc()
      #simplify table names for MLB games
      if (all(substr(MLB.gids, nchar(MLB.gids)-4, nchar(MLB.gids)-2)=="mlb")) {
        tab.nms <- names(tables)
        tab.nms <- sub("^game//inning//atbat$", "atbat", tab.nms)
        tab.nms <- sub("^game//inning//atbat//action$", "action", tab.nms)
        tab.nms <- sub("^game//inning//atbat//po$", "po", tab.nms)
        tab.nms <- sub("^game//inning//atbat//runner$", "runner", tab.nms)
        tab.nms <- sub("^game//inning//atbat//pitch$", "pitch", tab.nms)
      }
      if (all(substr(nonMLB.gids, nchar(nonMLB.gids)-4, nchar(nonMLB.gids)-2)!="mlb")) {
        #simplify table names for non MLB games
        tab.nms <- names(tables)
        tab.nms <- sub("inning//atbat$", "atbat", tab.nms)
        tab.nms <- sub("inning//atbat//action$", "action", tab.nms)
        tab.nms <- sub("inning//atbat//po$", "po", tab.nms)
        tab.nms <- sub("inning//atbat//runner$", "runner", tab.nms)
        tab.nms <- sub("inning//atbat//pitch$", "pitch", tab.nms)
      }
      tables <- setNames(tables, tab.nms)
      #Add names to atbat table for convenience
      scrape.env <- environment() #avoids bringing data objects into global environment
      data(players, package="pitchRx", envir=scrape.env)
      players$id <- as.character(players$id)
      #Add batter name to 'atbat'
      colnames(tables[["atbat"]]) <- sub("^batter$", "id", colnames(tables[["atbat"]]))
      tables[["atbat"]] <- merged(x=tables[["atbat"]], y=players, by = "id", all.x = TRUE)
      colnames(tables[["atbat"]]) <- sub("^id$", "batter", colnames(tables[["atbat"]]))
      colnames(tables[["atbat"]]) <- sub("^full_name$", "batter_name", colnames(tables[["atbat"]]))
      #Add pitcher name to 'atbat'
      colnames(tables[["atbat"]]) <- sub("^pitcher$", "id", colnames(tables[["atbat"]]))
      tables[["atbat"]] <- merged(x=tables[["atbat"]], y=players, by = "id", all.x = TRUE)
      colnames(tables[["atbat"]]) <- sub("^id$", "pitcher", colnames(tables[["atbat"]]))
      colnames(tables[["atbat"]]) <- sub("^full_name$", "pitcher_name", colnames(tables[["atbat"]]))
      colnames(tables[["atbat"]]) <- sub("^des", "atbat_des", colnames(tables[["atbat"]]))
      #Coerce matrices to data frames; turn appropriate variables into numerics
      for (i in names(tables)) tables[[i]] <- format.table(tables[[i]], name=i)

      #generate a "count" column from "b" (balls) & "s" (strikes)
      tables[["pitch"]] <- appendPitchCount(tables[["pitch"]])
      tables[["atbat"]] <- appendDate(tables[["atbat"]])
      if (!missing(connect)) {
        #Try to write tables to database, if that fails, write to csv. Then clear up memory
        for (i in names(tables)) export(connect, name = i, value = tables[[i]], template = fields[[i]])
        rm(tables)
        message("Collecting garbage")
        gc()
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
#' @return Returns a character vector.
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

#wrapper around collapse_obs to ensure a list is always returned (if only table, return list of length 1)
collapse_obs2 <- function(x) {
  val <- collapse_obs(x)
  if (is.list(val)) {
    return(val)
  } else {
    li <- list(val)
    names(li) <- unique(names(x))
    return(li)
  }
}

#' Export (append) a data.frame to a remote table in a database.
#'
#' This function is convenient if you plan on repeatedly appending to a table in a database.
#' All that is required is a database connection and a data.frame you want to export to that database.
#' If you want to initiate a table with more columns use the \code{template} argument.
#' Note that if the table already exists, the \code{template} argument will be ignored.
#'
#' @param connect database connection.
#' @param value local data frame.
#' @param template a named character vector. The names of the vector should contain the names of \code{value}. The values of this vector should contain the relevant field types.
#' @param name name of the remote table.
#' @param ... arguments passed onto \code{DBI::dbWriteTable}
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' my_db <- src_sqlite("DB.sqlite3")
#' data(pitches, package="pitchRx")
#' # Creates the 'pitches' table in the database
#' export(connect=my_db$con, value=pitches, name="pitches")
#' # Appends to the 'pitches' tables, but with the first column missing
#' export(connect=my_db$con, value=pitches[,-1], name="pitches")
#' tail(data.frame(collect(tbl(my_db, "pitches")))) #verify it appends correctly
#' # This data frame has a column that doesn't exist in the pitches table --
#' # so a new table is created.
#' export(connect=my_db$con, value=cbind(pitches, test="works"), name="pitches")
#' }

export <- function(connect, value, name, template, ...) {
  # '.' in table names are not good!
  names(value) <- sub("\\.", "_", names(value))
  #if url.map=FALSE, have to change 'url_key' to url
  names(value) <- sub("^url_key$", "url", names(value))
  if ("url" %in% names(value)) { #url should never be NA -- this is specific to pitchRx implementation
    throw <- is.na(value$url)
    if (any(throw)) value <- value[-throw,]
  }
  if (dim(value)[1] == 0) return(NULL)
  df.fields <- names(value)
  #Return fields if table exists already; otherwise, return NULL
  prior.fields <- plyr::try_default(DBI::dbListFields(connect, name), default=NULL, quiet=TRUE)
  #If prior.fields is non-empty, then the table already exists, and the table is used as the 'template'.
  if (!is.null(prior.fields)) {
    missing.fields <- setdiff(prior.fields, df.fields)
    value <- fill.NAs(value, missing.fields)
    new_fields <- setdiff(df.fields, prior.fields)
    # if there are new fields, add an appropriate column to the database table
    for (i in new_fields) {
      type <- DBI::dbDataType(connect, value[, i])
      DBI::dbSendQuery(connect, sprintf("ALTER TABLE %s ADD %s %s", name, i, type))
    }
    value <- value[DBI::dbListFields(connect, name)]
    success <- plyr::try_default(DBI::dbWriteTable(conn=connect, name=name, value=value, append=TRUE, overwrite=FALSE, row.names=FALSE),
                                 default=FALSE, quiet=TRUE)
  } else {
    if (missing(template)) {
      template <- sapply(value, function(x) DBI::dbDataType(connect, x))
      names(template) <- df.fields
    }
    master.fields <- names(template)
    missing.fields <- setdiff(master.fields, df.fields)
    value <- fill.NAs(value, missing.fields)
    new.fields <- setdiff(df.fields, master.fields)
    if (length(new.fields)) { # Expand the template to reflect the new fields
      new.types <- sapply(value[new.fields], function(x) DBI::dbDataType(connect, x))
      names(new.types) <- new.fields
      template <- c(template, new.types)
    }
    #the order of the columns in 'value' has to match the order of 'types'
    value <- value[names(template)]
    success <- plyr::try_default(DBI::dbWriteTable(conn=connect, name=name, value=value, field.types=template, row.names=FALSE),
                                 default=FALSE, quiet=TRUE)
  }
  if (success) {
    message(paste("Successfully copied", name, "table to database connection."))
  } else {
    file.name <- gsub(" ", "-", gsub(":", "-", paste0(name, " ", Sys.time(), ".csv")))
    message(paste("Failed to copy", name, "table to database connection. Writing", file.name, "instead."))
    write.csv(value, file=file.name, row.names=FALSE)
  }
  return(success)
}

#Function that appends columns of NAs
fill.NAs <- function(value, fields) {
  new.mat <- matrix(rep(NA, length(fields)), nrow=1)
  value <- cbind(value, `colnames<-`(new.mat, fields))
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
  obs <- XML2Obs(scoreboards) #Note to future self -- using `xpath='//game[@gameday_link]'` for future games gives the RCurl error -- Recv failure: Connection reset by peer
  obs2 <- obs[grep("^games//game$", names(obs))]
  gids <- collapse_obs(obs2)[,"gameday_link"]
  paste0("gid_", gids[!is.na(gids)])
}

#Take a start and an end date and make vector of "year_XX/month_XX/day_XX"
dates2urls <- function(first.day, last.day) {
  dates <- seq(as.Date(first.day), as.Date(last.day), by = "day")
  paste0("year_", format(dates, "%Y"), "/month_",
         format(dates, "%m"), "/day_", format(dates, "%d"))
}

# Take a game ID and construct url path for each specific game
gids2urls <- function(x) {
  root <- "http://gd2.mlb.com/components/game/"
  # Assume the league is 'mlb' unless we find evidence otherwise
  league <- rep("mlb", length(x))
  not.mlb <- !grepl("mlb", x)
  # If 'mlb' does not appear in the gid, use the home team's league
  if (any(not.mlb)) league[not.mlb] <- substr(x[not.mlb], 26, 28)
  base <- paste0(root, league)
  paste0(base, "/year_", substr(x, 5, 8), "/month_", substr(x, 10, 11),
         "/day_", substr(x, 13, 14), "/", x)
}

#Find the proper subset of game IDs based on start/end dates
subsetGids <- function(gids, first, last) {
  elements <- strsplit(gids, split="_")
  dates <- as.POSIXct(sapply(elements, function(x) paste(x[2], x[3], x[4], sep="-")))
  return(gids[last >= dates & dates >= first])
}

#silly function to work around stringsAsFactors=TRUE when using merge
merged <- function(x, y, ...){
  dat <- merge(x=x, y=y, sort=FALSE, ...)
  dat[] <- lapply(dat, function(x) as.character(x))
  return(dat)
}

# Take a matrix and turn into data frame and turn relevant columns into numerics
format.table <- function(dat, name) {
  nums <- NULL
  switch(name,
         game = nums <- c("venue_id", "scheduled_innings", "away_team_id", "away_league_id", "home_team_id",
                          "home_league_id", "away_games_back_wildcard", "away_win",  "away_loss", "home_win",
                          "home_loss", "inning", "outs", "away_team_runs","home_team_runs", "away_team_hits",
                          "home_team_hits", "away_team_errors", "home_team_errors"),
         player = nums <- c("id", "num", "avg", "hr", "rbi", "bat_order", "wins", "losses", "era"),
         coach = nums <- c("id", "num"),
         umpire = nums <- "id",
         hip = nums <- c("x", "y", "batter", "pitcher", "inning"),
         action = nums <- c("b", "s", "o", "player", "pitch", "inning"),
         atbat = nums <- c("pitcher", "batter", "num", "b", "s", "o", "inning"),
         pitch = nums <- c("id", "x", "y", "start_speed", "end_speed", "sz_top", "sz_bot", "pfx_x", "pfx_z", "px",
                           "pz", "x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az", "type_confidence",
                           "zone", "nasty", "spin_dir", "spin_rate", "inning", "num", "on_1b", "on_2b", "on_3b"),
         po = nums <- c("inning", "num"),
         runner = nums <- c("id", "inning", "num"))
  #For some reason, records are sometimes duplicated, remove them!
  dat <- data.frame(dat[!duplicated(dat),], stringsAsFactors=FALSE)
  nms <- names(dat)
  numz <- nums[nums %in% nms] #error handling (just in case one of the columns doesn't exist)
  for (i in numz) dat[, i] <- suppressWarnings(as.numeric(dat[, i]))
  if ("game" %in% name) {
    dat$url_scoreboard <- dat$url
    dat$url <- paste0(gsub("miniscoreboard.xml", "", dat$url), "gid_", dat$gameday_link, "/inning/inning_all.xml")
    # These fields only show up for suspended games...I don't think they're worth tracking...
    dat <- dat[, !names(dat) %in% c("runner_on_base_status", "runner_on_1b")]
  } else { #create a 'gameday_link' column for easier linking of tables
    if (length(grep("^url$", names(dat)))) dat$gameday_link <- sub("/.*", "", sub(".*gid", "gid", dat$url))
  }
  return(dat)
}

# Add columns with relevant pitch count to the 'pitch' table.
# @param dat 'pitch' matrix/df
# @return returns the original matrix/df with the proper pitch count column appended.
appendPitchCount <- function(dat) {
  if (any(!c("type", "gameday_link", "num") %in% colnames(dat))){
    warning("Count column couldn't be created")
    return(dat)
  }
  balls <- as.numeric(dat[,"type"] %in% "B")
  strikes <- as.numeric(dat[,"type"] %in% "S")
  pre.idx <- paste(dat[,"gameday_link"], dat[,"num"])
  idx <- factor(pre.idx, levels=unique(pre.idx))
  cum.balls <- unlist(tapply(balls, INDEX=idx, function(x){ n <- length(x); pmin(cumsum(c(0, x[-n])), 3) }))
  cum.strikes <- unlist(tapply(strikes, INDEX=idx, function(x) { n <- length(x); pmin(cumsum(c(0, x[-n])), 2) }))
  count <- paste(cum.balls, cum.strikes, sep = "-")
  return(cbind(dat, count))
}

# Add columns with relevant pitch count to the 'pitch' table.
# @param dat 'pitch' matrix/df
# @return returns the original matrix/df with the proper pitch count column appended.
appendDate <- function(dat) {
  if (!"gameday_link" %in% colnames(dat)){
    warning("'date' column couldn't be created")
    return(dat)
  }
  return(cbind(dat, date = substr(dat[,"gameday_link"], 5, 14)))
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

