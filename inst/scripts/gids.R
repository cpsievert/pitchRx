# Grab new game IDs
library(pitchRx)
data(gids)
last.game <- sort(gids)[length(gids)]
last.date <- as.Date(substr(last.game, 5, 14), format = "%Y_%m_%d")
new.gids <- pitchRx:::updateGids(last.date, last.date + 371)
gids <- unique(c(gids, new.gids))
devtools::use_data(gids, overwrite = TRUE)
