\name{docsToDataFrame}
\alias{docsToDataFrame}
\title{Turn XML documents into a Data Frames}
\usage{
  docsToDataFrame(docs, node, fields, urls,
    add.children = FALSE, use.values = FALSE)
}
\arguments{
  \item{docs}{XML documents}

  \item{node}{XML node of interest}

  \item{fields}{"Comlpete" set of field names for the data
  frame}

  \item{urls}{character vector of URLs used to build data
  frame}

  \item{add.children}{logical parameter specifying whether
  to scrape the XML children of the node(s) specified in
  \code{tables}.}

  \item{use.values}{logical parameter specifying whether to
  extract XML attributes or values of the node(s).}
}
\value{
  Returns a data frame.
}
\description{
  This function adds NAs to missing attributes.
}

