# Grab new game IDs
library(pitchRx)
library(rvest)
library(jsonlite)
data(gids)
last.game <- sort(gids)[length(gids)]
last.date <- as.Date(substr(last.game, 5, 14), format = "%Y_%m_%d")
day_urls <- pitchRx:::makeUrls(last.date, last.date + 371, gids="")
scoreboards <- paste0(day_urls, "/master_scoreboard.json")
day_list <- lapply(scoreboards, fromJSON)

new_gids <- lapply(day_list, function(day) {
  day$data$games$game$gameday
})
new_gids <- unlist(new_gids)
new_gids <- paste0('gid_', new_gids)
gids <- unique(c(gids, new_gids))
devtools::use_data(gids, overwrite = TRUE)
