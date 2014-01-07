\docType{data}
\name{pitches}
\alias{pitches}
\title{Sample PITCHf/x Data Set}
\description{
Every four-seam and cutting fastball thrown by Mariano
Rivera and Phil Hughes during the 2011 season.
}
\examples{
#This can reproduce data(pitches), but it takes a while...
\dontrun{
data <- scrapeFX(start="2011-01-01", end="2011-12-31")
names <- c("Mariano Rivera", "Phil Hughes")
atbats <- subset(data$atbat, pitcher_name \%in\% names)
pitchFX <- plyr::join(atbats, data$pitch, by=c("num", "url"), type="inner")
pitches <- subset(pitchFX, pitch_type \%in\% c("FF", "FC"))
}
}
\author{
Carson Sievert
}

