context("scrape")

# restrict to non-pre-season games so we can safely sample game IDs for testing
data(gids, package = "pitchRx")
valid.gids <- gids[grep("gid_[0-9]+_0[5-8]+", gids)]

test_that("Scrape returns a list of appropriate data frames", {
  dat <- scrape(start = "2013-06-06", end = "2013-06-06")
  expect_that(is.list(dat), is_true())
  expect_equal(length(dat), 5)
  expect_equal(names(dat), c("atbat", "action", "pitch", "po", "runner"))
})

test_that("Scraping game ids works", {
  data(gids)
  dat <- scrape(game.ids = sample(valid.gids, 2))
  expect_that(is.list(dat), is_true())
})


test_that("Appending to SQLite database works", {
  test.file <- "test.sqlite"
  db <- src_sqlite(test.file, create = TRUE)
  scrape(game.ids = gids[1000:1001], connect = db$con)
  scrape(game.ids = gids[1002], connect = db$con)
  #expect_equal(dim(collect(select(tbl(db, "pitch"), des)))[1], 916)
  file.remove(test.file)
})

test_that("Date column appends properly", {
  test.file <- "test.sqlite"
  db <- src_sqlite(test.file, create = TRUE)
  scrape(game.ids = sample(valid.gids, 2), connect = db$con)
  atbats <- collect(tbl(db, "atbat"))
  atbats <- atbats[,!grepl("date", names(atbats))]
  dbRemoveTable(db$con, name = 'atbat')
  copy_to(db, df = data.frame(atbats), name = 'atbat')
  scrape(game.ids = sample(valid.gids, 2), connect = db$con)
  dates <- collect(select(tbl(db, "atbat"), date))[,1]
  expect_true(all(grepl("^[0-9]{4}_[0-9]{2}_[0-9]{2}$", dates)))
  file.remove(test.file)
})
