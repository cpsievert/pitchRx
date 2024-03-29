% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{urlsToDataFrame}
\alias{urlsToDataFrame}
\title{Parse XML files into data frame(s)}
\usage{
urlsToDataFrame(
  urls,
  tables = list(),
  add.children = FALSE,
  use.values = FALSE
)
}
\arguments{
\item{urls}{set of urls for parsing}

\item{tables}{list of character vectors with appropriate names. The list names should correspond to XML nodes of interest within the XML files.}

\item{add.children}{logical parameter specifying whether to scrape the XML children of the node(s) specified in \code{tables}.}

\item{use.values}{logical parameter specifying whether to extract XML attributes or values of the node(s).}
}
\description{
This function is deprecated as of version 1.0
}
