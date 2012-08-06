\name{attachUrls}
\alias{attachUrls}
\title{Add columns with relevant "~/miniscoreboard.xml", "~/inning/inning_all.xml" and "~/player.xml"
file names to games table.}
\usage{
  attachUrls(df)
}
\arguments{
  \item{df}{data frame with all "game" attributes from
  "~/miniscoreboard.xml" files.}
}
\value{
  returns the original data frame with the proper url
  columns attached at the end.
}
\description{
  Add columns with relevant "~/miniscoreboard.xml",
  "~/inning/inning_all.xml" and "~/player.xml" file names
  to games table.
}

