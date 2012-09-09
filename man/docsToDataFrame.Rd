\name{docsToDataFrame}
\alias{docsToDataFrame}
\title{#Turns a list of XML documents into a single data frame.
#
#This function will determine the most complete amount of fields among all
#XML documents and fill NAs where information is missing.
#
#}
\usage{
  docsToDataFrame(docs, node, fields, urls, extract,
    extract.children)
}
\arguments{
  \item{docs}{XML documents #}

  \item{docs}{XML documents}

  \item{node}{XML node of interest}

  \item{fields}{"Comlpete" set of field names for the data
  frame}

  \item{urls}{character vector of URLs used to build data
  frame}

  \item{extract}{extract XML attributes or XML values (from
  a specific node)?}

  \item{extract.children}{extract XML children for a
  specific node? If NULL, children will not be considered.}

  \item{ignore}{XML child nodes that should be excluded
  from data frame.}
}
\value{
  returns a data frame Turn XML documents into a Data
  Frames

  This function adds NAs to missing attributes.

  Returns a data frame.
}
\description{
  #Turns a list of XML documents into a single data frame.
  # #This function will determine the most complete amount
  of fields among all #XML documents and fill NAs where
  information is missing. # #
}

