\name{strikeFX}
\alias{strikeFX}
\title{Visualize PITCHf/x strikezones}
\usage{
  strikeFX(data, geom = "point", contour = FALSE,
    point.size = 3, point.alpha = 1/3,
    color = "pitch_types", fill = "des", layer = list(),
    model, density1 = list(), density2 = list(),
    adjust = FALSE, limitz = c(-2.5, 2.5, 0, 5),
    parent = FALSE, ...)
}
\arguments{
  \item{data}{PITCHf/x data to be visualized.}

  \item{geom}{plotting geometry. Current choices are:
  "point", "hex", "bin", "tile" and "subplot2d".}

  \item{contour}{logical. Should contour lines be
  included?}

  \item{point.size}{Size of points (when geom="point")}

  \item{point.alpha}{plotting transparency parameter (when
  geom="point").}

  \item{color}{variable used to define coloring scheme.}

  \item{fill}{variable used to define subplot scheme (when
  geom="subplot2d").}

  \item{layer}{list of other ggplot2 (layered)
  modifications.}

  \item{model}{A function specifying a model which is used
  to predict upon a grid defined by \code{limitz}. It is
  recommended that one uses \link{gam} to plot response
  surface of a Generalized Additive Model (see examples).
  It shouldn't be necessary to specify \code{data} within
  the \code{model} function since it will inherit from
  \code{strikeFX}. If this option is used, the geometry
  must be either "hex", "tile" or "bin". If a non-valid
  geometry is used, the geometry will be forced to "tile".}

  \item{density1}{List of length one that defines a density
  estimate. The name should correspond to a variable in
  \code{data}. The value should correspond to an (observed)
  value of that variable.}

  \item{density2}{Similar to \code{density1}. If
  \code{density1 != density2}, the density estimates are
  automatically differenced.}

  \item{adjust}{logical. Should vertical locations be
  adjusted according to batter height?}

  \item{limitz}{limits for horizontal and vertical axes.}

  \item{parent}{is the function being called from a
  higher-level function? (experimental)}

  \item{...}{extra options passed onto geom commands}
}
\value{
  Returns a ggplot2 object.
}
\description{
  A suite of bivariate plots with "px" on the horizontal
  axis and "pz" on the vertical axis.
}
\examples{
data(pitches)
strikeFX(pitches, geom="tile", layer=facet_grid(.~stand))
\dontrun{
strikeFX(pitches, geom="hex", density1=list(des="Called Strike"), density2=list(des="Ball"),
         layer=facet_grid(.~stand))
noswing <- subset(pitches, des \%in\% c("Ball", "Called Strike"))
noswing$strike <- as.numeric(noswing$des \%in\% "Called Strike")
strikeFX(noswing, model=gam(strike ~ s(px)+s(pz), family = binomial(link='logit')),
         layer=facet_grid(.~stand))
#If sample size is an issue, try increasing the binwidths
strikeFX(noswing, geom="bin", model=gam(strike ~ s(px)+s(pz), family = binomial(link='logit')),
         layer=facet_grid(.~stand), binwidth=c(.5, .5))
strikeFX(noswing, geom="bin", model=gam(strike ~ s(px)+s(pz), family = binomial(link='logit')),
         density1=list(top_inning="Y"), density2=list(top_inning="N"), layer=facet_grid(.~stand),
         binwidth=c(.5, .5))
}
}

