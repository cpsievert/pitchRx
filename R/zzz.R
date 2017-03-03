z=scrape(game.ids=c("gid_2010_06_01_lhvaaa_tolaaa_1", "gid_2010_06_02_albaaa_nasaaa_1"))


zz=scrape(game.ids="gid_2011_04_04_minmlb_nyamlb_1")


# Check to see if any game.ids are minor league.
if(any(substr(game.ids, nchar(game.ids)-4, nchar(game.ids)-2)!="mlb")) {
  print("boo")
}


gamzList <- list()
for (i in length(gameDir)) {
  inningz <- readLines(paste0(gameDir[i], "/inning"))
  inningzList[[i]] <- inningz
}



# Make place holder for game innings.
inningzList <- list()
# Assume there were a ton of innings.
inning_num=1:25
for (i in inning_num) {
  if (isTRUE(any(grepl(paste0("inning_", i, ".xml", sep="", collapse="|"), inningzList)))) {
  valz <- paste0("inning_", i, ".xml", sep=",", collapse="|")
  inningzList[[i]] <- valz
  }
}
df <- do.call(rbind, inningzList)
