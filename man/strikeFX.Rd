\name{strikeFX}
\alias{strikeFX}
\title{Visualize PITCHf/x strikezones}
\usage{
  strikeFX(data, geom = "point", contour = FALSE,
    point.size = 3, point.alpha = 1/3,
    color = "pitch_types", fill = "des", layer = list(),
    model, model.save = TRUE, density1 = list(),
    density2 = list(), limitz = c(-2.5, 2.5, 0, 5),
    adjust = FALSE, draw_zones = TRUE, parent = FALSE, ...)
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

  \item{model}{Either a \link{gamObject} or a call to fit a
  model via \link{gam} or \link{bam}. Note that the
  horizontal and vertical location of the pitch MUST be
  included as covariates named "px" and "pz", respectively.
  Relevant factor variables must also be included as
  covariates in order to produce facetted or differenced
  plot(s). If this option is used, the geometry must be
  either "hex", "tile" or "bin". If a non-valid geometry is
  used, the geometry will be forced to "tile".}

  \item{model.save}{logical. Save the fitted \code{model}?
  If TRUE, the relevant model object is \link{assign}ed to
  the global environment}

  \item{density1}{List of length one. The name should
  correspond to a variable in \code{data}. The value should
  correspond to an (observed) value of that variable.}

  \item{density2}{Similar to \code{density1}. If
  \code{density1 != density2}, the relevant estimates are
  automatically differenced.}

  \item{limitz}{limits for horizontal and vertical axes.}

  \item{adjust}{logical. Should vertical locations be
  adjusted according to batter height?}

  \item{draw_zones}{logical. Should strikezones be
  included?}

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
p <- strikeFX(pitches, geom="tile", layer=facet_grid(.~stand))
p+theme(aspect.ratio=1)
\dontrun{
strikeFX(pitches, geom="hex", density1=list(des="Called Strike"), density2=list(des="Ball"),
         draw_zones=FALSE, contour=TRUE, layer=facet_grid(.~stand))
noswing <- subset(pitches, des \%in\% c("Ball", "Called Strike"))
noswing$strike <- as.numeric(noswing$des \%in\% "Called Strike")
m1 <- mgcv::bam(strike ~ s(px, pz, by=factor(stand)) +
               factor(stand), data=noswing, family = binomial(link='logit'))
strikeFX(noswing, model=m1, layer=facet_grid(.~stand))
#If sample size is an issue, try increasing the binwidths
strikeFX(noswing, model=m1, layer=facet_grid(.~stand), binwidth=c(.5,.5))
m2 <- mgcv::bam(strike ~ s(px, pz, by=factor(stand)) + s(px, pz, by=factor(top_inning)) +
           factor(stand) + factor(top_inning), data=noswing, family = binomial(link='logit'))
strikeFX(noswing, geom="bin", model=m2, density1=list(top_inning="Y"),
         density2=list(top_inning="N"), layer=facet_grid(.~stand), binwidth=c(.5, .5))
}
}

