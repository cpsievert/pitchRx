context("scraping")

test_that("Gameday scraping", { 
  files <- c("inning/inning_all.xml", "inning/inning_hit.xml",
             "miniscoreboard.xml", "players.xml")
  dat <- scrape(start = "2012-05-01", end = "2012-05-01", 
                suffix = files)
  #OR equivalently
  data(gids)
  dat2 <- scrape(gids=gids[grep("2012_05_01", gids)], suffix=files)
  expect_equal(dat, dat2)
})
 