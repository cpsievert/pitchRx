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
  The number of data frames is equal to the length of
  \code{tables}. The name of each element in \code{tables}
  should correspond to an XML node of interest to the user.
  If the value of a particular list element is NULL, this
  function will automatically determine the most complete
  set of fields for that data frame.
}
\examples{
#Find some XML files with twitter data
branch <- "http://gd2.mlb.com/components/game/mlb/twitter/"
doc <- htmlParse(branch)
nodes <- getNodeSet(doc, "//a")
values <- sapply(nodes, xmlValue)
extensions <- str_extract_all(values, "([a-z]+)InsiderTweets.xml.([0-9]+)")
twitter.urls <- paste(branch, extensions[llply(extensions, length) > 0], sep = "")
#Parse the files into a data frame
tweets <- urlsToDataFrame(urls = twitter.urls, tables = list(status = NULL), use.values = TRUE)
}

