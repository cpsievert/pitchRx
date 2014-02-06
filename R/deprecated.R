#' Scrape Major League Baseball's PITCHf/x Data
#'
#' This function is deprecated as of version 1.0
#' 
#' @export


scrapeFX <- function(start, end, tables = list()) { 
  message("scrapeFX is deprecated as of version 1.0. Please see ?scrapeGames.")
  return(NULL)
}

#' Parse XML files into data frame(s)
#' 
#' This function is deprecated as of version 1.0
#' 
#' @export


urlsToDataFrame <- function(urls, tables = list(), add.children = FALSE, use.values = FALSE) {
  message("urlsToDataFrame is deprecated as of version 1.0. Please see ?scrapeGames or ?XML2R::XML2Obs.")
  return(NULL)
}