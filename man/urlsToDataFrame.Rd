\name{urlsToDataFrame}
\alias{urlsToDataFrame}
\title{Parse XML files into data frame(s)}
\usage{
urlsToDataFrame(urls, tables = list(), add.children = FALSE,
  use.values = FALSE)
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
This function is deprecated as of version 1.0
}
\details{
This function takes on a list of XML files (ie, urls) and
shapes them into a data frame or list of data frames

\code{urlsToDataFrame} coerces either XML attributes or XML
values into a data frame. The XML nodes (aka, tags) of
interest need to be specified as the name(s) of the
\code{tables} parameter. The values of each \code{tables}
parameter should be a character vector that defines the
field names for the respective data frame. These field
names should match XML attributes or tags.

When \code{use.values = FALSE}, the length of \code{tables}
is equal to the number of data frames returned and the
values of \code{tables} are the fields for each data frame.
If a particular value of \code{tables} is \code{NULL}, the
function will automatically determine the most complete set
of fields and fill in \code{NA}s where information is
missing. If \code{add.children = TRUE}, \code{tables}
values should be \code{NULL} since child attributes will be
used for naming convention (with the relevant node as the
suffix name).

When \code{use.values = TRUE}, the value(s) of
\code{tables} are ignored. The XML children of the
specified node are the fields. If the children are
inconsistent, missing values are filled with \code{NA}s.
}
\examples{
\dontrun{Obtain "batting" stats going into a game played on May 6th, 2008:
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
                         add.children=TRUE)}
}

