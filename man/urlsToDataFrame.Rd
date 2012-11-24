\name{urlsToDataFrame}
\alias{urlsToDataFrame}
\title{Parse XML files into data frame(s)}
\usage{
  urlsToDataFrame(urls, tables = list(),
    add.children = FALSE, use.values = FALSE)
}
\arguments{
  \item{urls}{set of urls for parsing}

  \item{tables}{list of character vectors with appropriate
  names. The list names should correspond to XML nodes of
  interest within the XML files.}

  \item{add.children}{logical parameter specifying whether
  to scrape the XML children of the node(s) specified in
  \code{tables}.}

  \item{use.values}{logical parameter specifying whether to
  extract XML attributes or values of the node(s).}
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
#Get "batting" stats going into a game played on May 6th, 2008:
data(urls)
dir <- gsub("players.xml", "batters/",
            urls$url_player[1000])
doc <- htmlParse(dir)
nodes <- getNodeSet(doc, "//a")
values <- gsub(" ", "",
               sapply(nodes, xmlValue))
ids <- values[grep("[0-9]+", values)]
filenames <- paste(dir, ids, sep="")
stats <- urlsToDataFrame(filenames,
                         tables=list(Player=NULL),
                         add.children=TRUE)

#Scrape files with twitter data
branch <- "http://gd2.mlb.com/components/game/mlb/twitter/"
doc <- htmlParse(branch)
nodes <- getNodeSet(doc, "//a")
values <- sapply(nodes, xmlValue)
extensions <- str_extract_all(values, "([a-z]+)InsiderTweets.xml.([0-9]+)")
twitter.urls <- paste(branch, extensions[llply(extensions, length) > 0], sep = "")
#Parse the files into a data frame
tweets <- urlsToDataFrame(urls = twitter.urls, tables = list(status = NULL), use.values = TRUE)
}

