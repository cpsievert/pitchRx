# Grab the commit right after I added nonMLBgids
devtools::install_github("cpsievert/pitchRx", ref = "afc06c21fdb0267db3990817155dbbf6a35b742a")
library(pitchRx)
library(XML2R)
data(nonMLBgids, package = "pitchRx")
head(nonMLBgids)

urls <- makeUrls(gids = nonMLBgids)
player_urls <- paste0(urls, "/players.xml")
# This is gonna take a while -- we're trying over 100,000 urls in total!
grabPlayers <- function(expr = "year_2008") {
  obs <- XML2Obs(player_urls[grepl(expr, player_urls)])
  nms <- names(obs)
  new_players <- collapse_obs(obs[grepl("player$", nms)])
  data.frame(id = new_players[, "id"], full_name = paste(new_players[, "first"], new_players[, "last"]))
}

new_players <- grabPlayers()
players08 <- new_players
save(players08, file = "data/players08.rdata", compress = "xz")
players09 <- grabPlayers("year_2009")
save(players09, file = "data/players09.rdata", compress = "xz")
players10 <- grabPlayers("year_2010")
save(players10, file = "data/players10.rdata", compress = "xz")
players11 <- grabPlayers("year_2011")
save(players11, file = "data/players11.rdata", compress = "xz")
players12 <- grabPlayers("year_2012")
save(players12, file = "data/players12.rdata", compress = "xz")
players13 <- grabPlayers("year_2013")
save(players13, file = "data/players13.rdata", compress = "xz")
players14 <- grabPlayers("year_2014")
save(players14, file = "data/players14.rdata", compress = "xz")

new_players <- rbind(players08, players09, players10, players11, players12, players13, players14)
new_players$id <- as.integer(as.character(new_players$id))
new_players$full_name <- as.character(new_players$full_name)
dupes <- duplicated(new_players)
new_players <- new_players[!dupes, ]
str(new_players)


# What if IDs are "recycled". That is, can an ID map to more than one player (maybe in a different league?)
library(dplyr)
new_players %>%
  tbl_df %>%
  group_by(full_name) %>%
  summarise(count_n = n()) %>%
  arrange(desc(count_n))

# The same name can have different IDs...are these the same person or just players with the same name?
subset(new_players, full_name == "Jose Rodriguez")

# One name linked to multiple IDs shouldn't (hopefully) be an issue. 
# However, one ID mapping to different players is an issue
new_players %>%
  tbl_df %>%
  group_by(id) %>%
  summarise(count_n = n()) %>%
  arrange(desc(count_n))

# These seems to be different spellings of the same name
subset(new_players, id == 500773)
subset(new_players, id == 502315)

# The ID column should comprise a unique key for this table otherwise it causes issues -- https://github.com/cpsievert/pitchRx/issues/8
dupes <- duplicated(new_players$id)
new_players <- new_players[!dupes,]

# Take the old players and add the new ones
data(players, package = "pitchRx")
players <- rbind(players, new_players)

# Some players are bound to be in both
dupes <- duplicated(players$id)
players <- players[!dupes,]

# Verify that there are no duplicate IDs
players %>%
  tbl_df %>%
  group_by(id) %>%
  summarise(count_n = n()) %>%
  arrange(desc(count_n))


save(players, file = "data/players.rda", compress = "xz")

