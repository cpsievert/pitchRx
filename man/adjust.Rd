\name{adjust}
\alias{adjust}
\title{"Adjust" attributes to match the entire set}
\usage{
  adjust(info, tags)
}
\arguments{
  \item{info}{XML attributes from a particular node.}

  \item{tags}{"complete" set of attribute names.}
}
\value{
  returns all present info matching the tags criteria
}
\description{
  This function adds NAs to missing attributes.
}

