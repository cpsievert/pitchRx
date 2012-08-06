\name{urlsToDataFrame}
\alias{urlsToDataFrame}
\title{Function for parsing urls and specified nodes into data frames}
\usage{
  urlsToDataFrame(urls,
    tables = list(atbat = NULL, pitch = NULL),
    add.children = FALSE, use.values = FALSE)
}
\arguments{
  \item{urls}{set of urls for parsing}

  \item{tables}{list of character vectors containing field
  names for each table. The list names have to correspond
  to XML nodes of interest within the XML files.}

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
  The primary use for this function is to scrape an
  "atbats" table and the corresponding "pitch" (ie, Pitch
  F/X) table for the specified set of URLs. In fact, this
  function is used as the core functionality behind
  scrapePitchFX. This function provides added flexibility
  by allowing one to specify nodes of interest other than
  "atbat" and "pitch". If the value of the list is NULL,
  this function will automatically generate the most
  complete collection of data points. Important: You must
  have "atbat" AND "pitch" nodes if you want to identify
  who threw a particular pitch. Also, if you specify field
  names for the table, you should be confident that those
  are the most complete set of fields.
}
\examples{
#If it isn't currently baseball season, consider changing the dates below:
#Also, this is a small scaled example. Visit my website if you would like to see how to
#build a current and complete database.
#mini.urls <- getScoreboardURLs(first.date = Sys.Date() - 10, last.date = Sys.Date())
#game.urls <- getPitchFxURLs(mini.urls)
#data <- urlsToDataFrame(urls = game.urls)
#atbats <- data$atbat
#pitches <- data$pitch
}

