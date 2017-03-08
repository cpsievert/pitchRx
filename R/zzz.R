z=scrape(game.ids=c("gid_2010_06_01_lhvaaa_tolaaa_1", "gid_2010_06_02_albaaa_nasaaa_1"))


zz=scrape(game.ids="gid_2011_04_04_minmlb_nyamlb_1")

game.ids="gid_2010_06_01_lhvaaa_tolaaa_1"

makeUrls(game.ids)


### Working: Need to find out why I had to change the `obs` arguments around line 248. Look at original and test.



# Check to see if any game.ids are minor league.
if(any(substr(game.ids, nchar(game.ids)-4, nchar(game.ids)-2)!="mlb")) {
  print("boo")
}


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
