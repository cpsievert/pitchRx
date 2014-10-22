library(pitchRx)
library(XML2R)
data(gids)
last.game <- sort(gids)[length(gids)]
last.date <- as.Date(substr(last.game, 5, 14), format = "%Y_%m_%d")
urls <- paste0("http://gd2.mlb.com/components/game/mlb/",
              pitchRx:::dates2urls(last.date, last.date + 371),
              "/miniscoreboard.xml")
obs <- XML2Obs(urls, url.map = FALSE)
nms <- names(obs)
games <- collapse_obs(obs[grepl("^games//game$", nms)])
gids <- unique(c(gids, games[,"gameday_link"]))
save(gids, file = "~/Desktop/github/local/pitchRx/gids.rda")
