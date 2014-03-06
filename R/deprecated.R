#' Scrape Major League Baseball's PITCHf/x Data
#'
#' This function is deprecated as of version 1.0
#' 
#' @param start date "yyyy-mm-dd" to commence scraping of pitch F/X data
#' @param end date "yyyy-mm-dd" to terminate scraping pitch F/X data
#' @param tables XML nodes to be parsed into a data frame
#' @seealso \link{scrape}
#' @export


scrapeFX <- function(start, end, tables = list()) { 
  message("scrapeFX is deprecated as of version 1.0. Please see ?scrape.")
  return(NULL)
}

#' Parse XML files into data frame(s)
#' 
#' This function is deprecated as of version 1.0
#' 
#' @param urls set of urls for parsing
#' @param tables list of character vectors with appropriate names. The list names should correspond to XML nodes of interest within the XML files.
#' @param add.children logical parameter specifying whether to scrape the XML children of the node(s) specified in \code{tables}.
#' @param use.values logical parameter specifying whether to extract XML attributes or values of the node(s).
#' @export


urlsToDataFrame <- function(urls, tables = list(), add.children = FALSE, use.values = FALSE) {
  message("urlsToDataFrame is deprecated as of version 1.0. Please see ?scrape or ?XML2R::XML2Obs.")
  return(NULL)
}