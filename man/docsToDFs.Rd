\name{docsToDFs}
\alias{docsToDFs}
\title{Turns a list of XML documents into a single data frame.}
\usage{
  docsToDFs(docs)
}
\arguments{
  \item{docs}{XML documents}
}
\value{
  returns a data frame
}
\description{
  This function will determine the most complete amount of
  fields among all XML documents and fill NAs where
  information is missing.
}

