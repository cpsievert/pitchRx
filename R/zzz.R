z=scrape(game.ids="gid_2010_06_01_lhvaaa_tolaaa_1")


zz=scrape(game.ids="gid_2011_04_04_minmlb_nyamlb_1")


# Check to see if any game.ids are minor league
if(substr(game.ids, nchar(game.ids)-4, nchar(game.ids)-2)!="mlb") {
  print("boo")
}




inningz <- readLines("http://gd2.mlb.com/components/game/aaa/year_2010/month_06/day_01/gid_2010_06_01_lhvaaa_tolaaa_1/inning")

inning_num=1:9
for (i in inning_num) {

  if (any(grep(paste0("inning_", inning_num, ".xml", sep="", collapse="|"), inningz))) {
  valz <- paste0("inning_", inning_num, ".xml", sep=",", collapse="|")
  print("boo")

  }
}

