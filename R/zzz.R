z=scrape(game.ids="gid_2010_06_01_lhvaaa_tolaaa_1")


zz=scrape(game.ids="gid_2011_04_04_minmlb_nyamlb_1")


# Check to see if any game.ids are minor league
if(substr(game.ids, nchar(game.ids)-4, nchar(game.ids)-2)!="mlb") {
  print("boo")
}








