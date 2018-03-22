# Grab new game IDs
library(pitchRx)
library(rvest)
data(gids)
last.game <- sort(gids)[length(gids)]
last.date <- as.Date(substr(last.game, 5, 14), format = "%Y_%m_%d")
day_urls <- pitchRx:::makeUrls(last.date, last.date + 371, gids="")
day_list <- lapply(day_urls, xml2::read_html)
new_gids <- lapply(day_list, function(day) {
  day %>%
    html_nodes("a") %>%
    html_text() %>%
    stringr::str_trim() %>%
    stringr::str_remove("/$") %>%
    grep("^gid_", ., value = TRUE)
})
new_gids <- unlist(new_gids)
gids <- unique(c(gids, new_gids))
devtools::use_data(gids, overwrite = TRUE)
