# This script outlines the approach taken to add support for non-mlb games (nonMLBgids)
# To increase reproducibilty, grab the commit right before I added nonMLBgids 
devtools::install_github("cpsievert/pitchRx", ref = "07f26a00e6494e4432ccde97d2c9e780cffd7c68")
library(pitchRx)
library(XML2R)
data(gids, package = "pitchRx")

# list unique combinations of home and away "league" codes
getLeagues <- function(x) {
  unique(cbind(substr(x, 19, 21), substr(x, 26, 28)))
}

# Almost every gid has 'mlb' as either the home or away 'league'
# The only exception is international games, where 'mlb' is a team name abbreviation
lgs <- getLeagues(gids)
lgs
gids[grepl("[a-z]{3}int_[a-z]{3}int", gids)]

#lgs <- unique(c(getLeagues(gids)))
#new.lgs <- lgs[!grepl("mlb", lgs)]

# Function to grab gameday IDs for a given league and time period
# Not the most efficient, but it's an easy and safe way to do it
scrapeGids <- function(start = "2008-01-01", end = "2015-01-01", league = "/aaa/") {
  urls <- makeUrls(start, end, gids = "")
  mini <- paste0(gsub("/mlb/", league, urls), "/miniscoreboard.xml")
  obs <- XML2Obs(mini)
  nms <- names(obs)
  games <- collapse_obs(obs[grepl("^games//game$", nms)])
  paste0("gid_", games[,"gameday_link"])
}

# Triple-A
aaa.gids <- scrapeGids()
# The "official" league (that is, the league abbrevation used in the url root) 
# is always the home team's league (for non-mlb games)
getLeagues(aaa.gids)
# There are some gids with 'mlb' as away teams. These are not new.
setdiff(aaa.gids[grepl("[a-z]{3}mlb_[a-z]{3}aaa", aaa.gids)],
        gids[grepl("[a-z]{3}mlb_[a-z]{3}aaa", gids)])

# Double-A,
aax.gids <- scrapeGids(league = "/aax/")
save(aax.gids, file = "data/aaxGids.rdata", compress = "xz")
getLeagues(aax.gids)

# High-A
afa.gids <- scrapeGids(league = "/afa/")
save(afa.gids, file = "data/afaGids.rdata", compress = "xz")
getLeagues(afa.gids)

# Low-A
afx.gids <- scrapeGids(league = "/afx/")
getLeagues(afx.gids)

# Short season A-ball
asx.gids <- scrapeGids(league = "/asx/")
getLeagues(asx.gids)

# College
bbc.gids <- scrapeGids(league = "/bbc/")
# Not many 'new' bbc games
setdiff(bbc.gids, gids)
getLeagues(bbc.gids)

# World Baseball Classic
int.gids <- scrapeGids(league = "/int/")
# Not many 'new' int games
setdiff(int.gids, gids)
getLeagues(int.gids)

# Japanese league maybe?
jml.gids <- scrapeGids(league = "/jml/")
# No 'new' jml games
setdiff(jml.gids, gids)

# Generic minors
min.gids <- scrapeGids(league = "/min/")
# Not many 'new' min games
setdiff(min.gids, gids)

# Not sure what league this is
nae.gids <- scrapeGids(league = "/nae/")
# Not many 'new' nae games
setdiff(nae.gids, gids)
getLeagues(nae.gids)

# Rookie ball
rok.gids <- scrapeGids(league = "/rok/")
getLeagues(rok.gids)

# Winter league?
win.gids <- scrapeGids(league = "/win/")
getLeagues(win.gids)


nonMLBgids <- unique(c(aaa.gids, aax.gids, afa.gids, afx.gids, asx.gids,
                       bbc.gids, int.gids, min.gids, nae.gids, rok.gids, win.gids))
save(nonMLBgids, file = "data/nonMLBgids.rda", compress = "xz")

# Thanks Albert and Harry for helping w/ league abbrevations --  -- https://twitter.com/albertlyu/status/485643587011354624