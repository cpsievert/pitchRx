\name{export}
\alias{export}
\title{Export (append) a data.frame to a remote table in a database.}
\usage{
export(connect, value, template = names(value),
  name = deparse(substitute(value)), ...)
}
\arguments{
  \item{connect}{database connection.}

  \item{value}{local data frame.}

  \item{template}{a named character vector. The names of
  the vector should contain the names of \code{value}. The
  values of this vector should contain the relevant field
  types.}

  \item{name}{name of the remote table.}

  \item{...}{arguments passed onto
  \code{DBI::dbWriteTable}}
}
\description{
This function assumes you have knowledge of a
\code{template} of fields to be used for the table. This
should be thought as a
}
\examples{
\dontrun{
library(dplyr)
my_db <- src_sqlite("pitches.sqlite3")
data(pitches, package="pitchRx")
data(fields, package="pitchRx")
export(connect=my_db$con, value=pitches, template = c(fields[["pitch"]], fields[["atbat"]]))
}
}

