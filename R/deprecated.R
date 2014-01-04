#' Scrape Major League Baseball's PITCHf/x Data
#'
#' \textbf{This function is deprecated as of version 1.0}
#'
#' This function is a wrapper around \link{urlsToDataFrame} which increases convenience for scraping PITCHf/x directly from XML files.
#'
#' Data should be collected on a yearly (or shorter) basis. By default, records from the 'pitch' and 'atbat' level are collected.
#' One should manipulate the \code{tables} parameter if other data is desired. 
#' 
#' @param start date "yyyy-mm-dd" to commence scraping of pitch F/X data
#' @param end date "yyyy-mm-dd" to terminate scraping pitch F/X data
#' @param tables XML nodes to be parsed into a data frame
#' @seealso \link{urlsToDataFrame}
#' @return Returns a list containing a data frame specific to each element in \code{tables}. The default setting returns two data frames. The larger one contains data "PITCHfx parameters" for each pitch. The smaller one contains data relevant to each atbat.
#' @export
#' @examples
#' \dontrun{
#' #Collect PITCHf/x data for May 1st, 2012
#' dat <- scrapeFX(start = "2012-05-01", end = "2012-05-01")
#' #Join tables for data analysis
#' pitches <- plyr::join(dat$pitch, dat$atbat, by = c("num", "url"), type = "inner")
#' }
#' 
#' \dontrun{Algorithm for obtaining all available PITCHfx data** 
#' # (1) Collect PITCHfx data from 2012
#' data12 <- scrapeFX(start="2012-01-01", end="2013-01-01")
#' # (2) Write data12$pitch and data12$atbat to a database
#' # (3) Remove 2012 data from working space
#' rm(data12)
#' # (4) Repeat (1)-(3) for 2011, 2010, 2009 & 2008}

scrapeFX <- function(start, end, tables = list(atbat = fields$atbat, pitch = fields$pitch)) { 
  message("scrapeFX is deprecated as of version 1.0. Please see ?scrapeGames.")
  return(NULL)
}

#' Parse XML files into data frame(s)
#' 
#' \textbf{This function is deprecated as of version 1.0}
#' 
#' This function takes on a list of XML files (ie, urls) and shapes them into a data frame or list of data frames
#' 
#' \code{urlsToDataFrame} coerces either XML attributes or XML values into a data frame. The XML nodes (aka, tags) of interest 
#' need to be specified as the name(s) of the \code{tables} parameter. The values of each \code{tables} parameter should be a 
#' character vector that defines the field names for the respective data frame. These field names should match XML attributes or tags.
#' 
#' When \code{use.values = FALSE}, the length of \code{tables} is equal to the number of data frames returned and 
#' the values of \code{tables} are the fields for each data frame. If a particular value of \code{tables} is \code{NULL}, 
#' the function will automatically determine the most complete set of fields and fill in \code{NA}s where 
#' information is missing. If \code{add.children = TRUE}, \code{tables} values should be \code{NULL} since 
#' child attributes will be used for naming convention (with the relevant node as the suffix name). 
#' 
#' When \code{use.values = TRUE}, the value(s) of \code{tables} are ignored. The XML children of the specified node
#' are the fields. If the children are inconsistent, missing values are filled with \code{NA}s.
#' 
#' @param urls set of urls for parsing
#' @param tables list of character vectors with appropriate names. The list names should correspond to XML nodes of interest within the XML files.
#' @param add.children logical parameter specifying whether to scrape the XML children of the node(s) specified in \code{tables}.
#' @param use.values logical parameter specifying whether to extract XML attributes or values of the node(s).
#' @return Returns a data frames if the length of tables is one. Otherwise, it returns a list of data frames.
#' @export
#' @examples
#' \dontrun{Obtain "batting" stats going into a game played on May 6th, 2008:
#' data(urls)
#' dir <- gsub("players.xml", "batters/", 
#'             urls$url_player[1000])
#' doc <- htmlParse(dir)
#' nodes <- getNodeSet(doc, "//a")
#' values <- gsub(" ", "", 
#'                sapply(nodes, xmlValue))
#' ids <- values[grep("[0-9]+", values)]
#' filenames <- paste(dir, ids, sep="")
#' stats <- urlsToDataFrame(filenames, 
#'                          tables=list(Player=NULL), 
#'                          add.children=TRUE)}

urlsToDataFrame <- function(urls, tables = list(), add.children = FALSE, use.values = FALSE) {
  message("urlsToDataFrame is deprecated as of version 1.0. Please see ?scrapeGames or ?XML2R::XML2Obs.")
  return(NULL)
}