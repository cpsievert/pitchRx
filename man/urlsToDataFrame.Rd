\name{urlsToDataFrame}
\alias{urlsToDataFrame}
\title{Parse XML files into data frame(s)}
\usage{
  urlsToDataFrame(urls, tables = list(),
    extract = "attributes", extract.children = NULL,
    ignore = NULL)
}
\arguments{
  \item{urls}{set of urls for parsing}

  \item{tables}{list of character vectors with appropriate
  names. The list names should correspond to XML nodes of
  interest within the XML files.}

  \item{extract}{extract XML attributes or XML values (from
  a specific node)?}

  \item{extract.children}{extract XML children for a
  specific node? If NULL, children will not be considered.}

  \item{ignore}{XML child nodes that should be excluded
  from data frame.}
}
\value{
  Returns a data frames if the length of tables is one.
  Otherwise, it returns a list of data frames.
}
\description{
  This function takes on a list of XML file names (ie,
  urls) and parses them into an appropriate amount of data
  frames.
}
\details{
  This function can coerce either XML attributes or XML
  values into a data frame. The node(s) of interest need to
  be specified as the name(s) of \code{tables}.

  When \code{use.values = FALSE}, the length of
  \code{tables} is equal to the number of data frames
  returned and the values of \code{tables} are the fields
  for each data frame. If a particular value is
  \code{NULL}, the function will automatically determine
  the most complete set of fields and fill in \code{NA}s
  where information is missing. If \code{add.children =
  TRUE}, it is recommended that \code{tables} values be
  \code{NULL} since child attributes will also be
  incorporated as fields (with the relevant node as the
  suffix name).

  When \code{use.values = TRUE}, the values of
  \code{tables} is ignored. The XML children of the
  specified node are the fields. If the children are
  inconsistent, missing values are filled with \code{NA}s.
}
\examples{
#Get Josh Hamilton's stats going into a game played on July 18th, 2009:
#url <- "http://gd2.mlb.com/components/game/mlb/year_2009/month_07/day_18/gid_2009_07_18_minmlb_texmlb_1/batters/285078.xml"
#urlsToDataFrame(url, tables = list(Player = NULL), extract.children = "attributes")

#Find XML files with twitter data
#branch <- "http://gd2.mlb.com/components/game/mlb/twitter/"
#doc <- htmlParse(branch)
#nodes <- getNodeSet(doc, "//a")
#values <- sapply(nodes, xmlValue)
#extensions <- str_extract_all(values, "([a-z]+)InsiderTweets.xml.([0-9]+)")
#twitter.urls <- paste(branch, extensions[llply(extensions, length) > 0], sep = "")
#Parse the files into a data frame
#tweets <- urlsToDataFrame(urls = twitter.urls, tables = list(status = NULL), extract = "values")
}

