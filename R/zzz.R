z=scrape(game.ids=c("gid_2010_06_01_lhvaaa_tolaaa_1", "gid_2010_06_02_albaaa_nasaaa_1"))


zz=scrape(game.ids="gid_2011_04_04_minmlb_nyamlb_1")

game.ids="gid_2010_06_01_lhvaaa_tolaaa_1"

game.ids="gid_2010_06_02_albaaa_nasaaa_1"

makeUrls(game.ids)


### Working: Need to find out why I had to change the `obs` arguments around line 248. Look at original and test.



# Check to see if any game.ids are minor league.
if (any(grepl("inning/inning_all.xml", suffix)) & all(substr(game.ids, nchar(game.ids)-4, nchar(game.ids)-2)!="mlb")) {
  # Define some empty lists to be used in the loop.
  inningValz <- list(); finalValz <- list(); gamzList <- list();
  # Read lines of each game directory from gameDir.
  for (i in 1:length(gameDir)) {
    gamzList[[i]] <- readLines(paste0(gameDir[i], "/inning"))
    # We have to find the number of innings played. We'll assume 100 just to be safe.
    # For each inning we find in the gamzList, append the correct base URL from gameDir.
    for(x in 1:100) {
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
    #no longer need the 'game' and 'game//inning' observations
    nms <- names(obs)
    rm.idx <- c(grep("^game$", nms), grep("^game//inning$", nms))
    if (length(rm.idx) > 0) obs <- obs[-rm.idx]
    if (exists("tables")){
      tables <- c(tables, collapse_obs2(obs))
    } else {
      tables <- collapse_obs2(obs)
    }
    #Free up some memory
    rm(obs)
    gc()
    #simplify table names
    tab.nms <- names(tables)
    tab.nms <- sub("inning//atbat$", "atbat", tab.nms)
    tab.nms <- sub("inning//atbat//action$", "action", tab.nms)
    tab.nms <- sub("inning//atbat//po$", "po", tab.nms)
    tab.nms <- sub("inning//atbat//runner$", "runner", tab.nms)
    tab.nms <- sub("inning//atbat//pitch$", "pitch", tab.nms)
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
}
