#' Visualize PITCHf/x strikezones
#' 
#' A suite of bivariate plots with "px" on the horizontal axis and "pz" on the vertical axis.
#'
#' @param data PITCHf/x data to be visualized.
#' @param geom plotting geometry. Current choices are: "point", "hex", "bin", "tile" and "subplot2d". 
#' @param contour logical. Should contour lines be included?
#' @param point.size Size of points (when geom="point")
#' @param point.alpha plotting transparency parameter (when geom="point").
#' @param color variable used to define coloring scheme.
#' @param fill variable used to define subplot scheme (when geom="subplot2d").
#' @param layer list of other ggplot2 (layered) modifications.
#' @param model A function specifying a model which is used to predict upon a grid defined by \code{limitz}.
#' It is recommended that one uses \link{gam} to plot response surface of a Generalized Additive Model (see examples). 
#' It shouldn't be necessary to specify \code{data} within the \code{model} function since it will inherit from \code{strikeFX}. 
#' If this option is used, the geometry must be either "hex", "tile" or "bin". If a non-valid geometry is used, the geometry will be forced to "tile".
#' @param density1 List of length one that defines a density estimate. The name should correspond to a variable in \code{data}. The value should correspond to an (observed) value of that variable.
#' @param density2 Similar to \code{density1}. If \code{density1 != density2}, the density estimates are automatically differenced.
#' @param limitz limits for horizontal and vertical axes. 
#' @param adjust logical. Should vertical locations be adjusted according to batter height?
#' @param draw_zones logical. Should strikezones be included?
#' @param parent is the function being called from a higher-level function? (experimental)
#' @param ... extra options passed onto geom commands
#' @return Returns a ggplot2 object.
#' @export
#' @import mgcv
#' @import plyr
#' @importFrom MASS kde2d
#' @examples
#' data(pitches)
#' strikeFX(pitches, geom="tile", layer=facet_grid(.~stand))
#' \dontrun{
#' strikeFX(pitches, geom="hex", density1=list(des="Called Strike"), density2=list(des="Ball"), 
#'          draw_zones=FALSE, contour=TRUE, layer=facet_grid(.~stand))
#' noswing <- subset(pitches, des %in% c("Ball", "Called Strike"))
#' noswing$strike <- as.numeric(noswing$des %in% "Called Strike")
#' strikeFX(noswing, model=gam(strike ~ s(px)+s(pz), family = binomial(link='logit')), 
#'          layer=facet_grid(.~stand))
#' #If sample size is an issue, try increasing the binwidths
#' strikeFX(noswing, geom="bin", model=gam(strike ~ s(px)+s(pz), family = binomial(link='logit')), 
#'          layer=facet_grid(.~stand), binwidth=c(.5, .5))
#' strikeFX(noswing, geom="bin", model=gam(strike ~ s(px)+s(pz), family = binomial(link='logit')), 
#'          density1=list(top_inning="Y"), density2=list(top_inning="N"), layer=facet_grid(.~stand), 
#'          binwidth=c(.5, .5))
#' }
#' 

strikeFX <- function(data, geom = "point", contour=FALSE, point.size=3, point.alpha=1/3, color = "pitch_types", fill = "des", layer = list(), model, density1=list(), density2=list(), limitz=c(-2.5, 2.5, 0, 5), adjust=FALSE, draw_zones=TRUE, parent=FALSE, ...){ 
  px=pz_adj=..density..=top=bottom=right=left=x=y=z=NULL #ugly hack to comply with R CMD check
  if (any(!geom %in% c("point", "bin", "hex", "tile", "subplot2d"))) warning("Current functionality is designed to support the following geometries: 'point', 'bin', 'hex', 'tile', 'subplot2d'.")
  if ("pitch_type" %in% names(data)) { #Add descriptions as pitch_types
    data$pitch_type <- factor(data$pitch_type)
    types <- data.frame(pitch_type=c("SI", "FF", "IN", "SL", "CU", "CH", "FT", "FC", "PO", "KN", "FS", "FA", NA, "FO"),
                   pitch_types=c("Sinker", "Fastball (four-seam)", "Intentional Walk", "Slider", "Curveball", "Changeup", 
                                 "Fastball (two-seam)", "Fastball (cutter)", "Pitchout", "Knuckleball", "Fastball (split-finger)",
                                 "Fastball", "Unknown", "Forkball"))
    data <- join(data, types, by = "pitch_type", type="inner")
  } 
  if (!"b_height" %in% names(data)) {
    warning("pitchRx assumes the height of each batter is recorded as 'b_height'. Since there is no such column, we will assume each batter has a height of 6'2''")
    data$b_height <- "6-2"
  }
  if (!color %in% names(data)) {
    warning(paste(color, "is the variable that defines coloring but it isn't in the dataset!"))
    color <- ""
  }
  locations <- c("px", "pz")
  FX <- data[complete.cases(data[,locations]),] #get rid of records missing the necessary parameters
  for (i in locations) FX[,i] <- as.numeric(FX[,i])
  if (parent) { #ugly workaround for shiny implementation
    layers <- NULL
    for (i in layer)
      layers <- list(layers, eval(i)) 
  } else {
    layers <- layer
  }
  facets <- getFacets(layer=layers)
  boundaries <- getStrikezones(FX, facets, strikeFX = TRUE) 
  if (adjust) {
    FX$pz_adj <- boundaries[[1]] #adjusted vertical locations
  } else {
    FX$pz_adj <- FX$pz # "adjusted" vert locations
  }
  FX <- join(FX, boundaries[[2]], by="stand", type="inner")
  for (i in locations) FX[,i] <- as.numeric(FX[,i])
  #Recycled plot formats
  labelz <- labs(x = "Horizontal Pitch Location", y = "Height from Ground")
  legendz <- theme(legend.position = c(0.25,0.05), legend.direction = "horizontal")
  xrange <- xlim(limitz[1:2])
  yrange <- ylim(limitz[3:4])
  if (draw_zones) {
    white_zone <- geom_rect(mapping=aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha=0, fill="pink", colour="white")
    black_zone <- geom_rect(mapping=aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha=0, fill="pink", colour="grey20")
  } else {
    white_zone <- NULL
    black_zone <- NULL
  }
  if (!missing(model)) {
    FX1 <- subsetFX(FX, density1)
    form <- as.list(substitute(model))
    x.grid <- seq(limitz[1], limitz[2], 0.05) #how to add option for grid granularity?
    y.grid <- seq(limitz[3], limitz[4], 0.05) #how to add option for grid granularity?
    grid <- expand.grid(px=x.grid, pz=y.grid)
    if (identical(density1, density2)) { #densities are not differenced
      if (!is.null(facets)) {
        stuff <- dlply(FX1, facets, function(x) { fitModel(x, form, grid) } )
        densities <- ldply(stuff)
      } else {
        densities <- fitModel(FX1, form, grid)
      }
      p <- plotDensity(densities, boundaries, contour, geom, ...)
      p <- p + white_zone
      return(p+labelz+xrange+yrange+layers)
    } else {  #densities are differenced
      FX2 <- subsetFX(FX, density2)
      if (!is.null(facets)) {
        stuff <- dlply(FX1, facets, function(x) { fitModel(x, form, grid) } )
        stuff2 <- dlply(FX2, facets, function(x) { fitModel(x, form, grid) } )
        densities <- ldply(stuff)
        densities2 <- ldply(stuff2)
      } else {
        densities <- fitModel(FX1, form, grid)
        densities2 <- fitModel(FX2, form, grid)
      }
      diff <- densities[-grep("z", names(densities))]
      diff$z <- densities$z - densities2$z
      t2 <- plotDensity(diff, boundaries, contour, geom, ...)
      t2 <- t2 + black_zone
      return(t2+scale_fill_gradient2(midpoint=0)+labelz+xrange+yrange+layers)
    }
  }
  if (geom %in% "subplot2d") { #special handling for subplotting
    if (!require(ggsubplot)) {
      message("The 'subplot2d' geom requires library(subplot2d)!")
      return()
    }
    require(ggsubplot)
    return(ggplot(data=FX)+labelz+xrange+yrange+
             geom_subplot2d(aes(x=px, y=pz_adj, 
                    subplot = geom_bar(aes_string(x=fill, fill = fill))), ...)+
             black_zone+layers)
  }
  if (geom %in% c("bin", "hex", "tile")) { #special handling for (2D) density geometries
    if (identical(density1, density2)) { #densities are not differenced
      FX1 <- subsetFX(FX, density1)
      t <- ggplot(data=FX1, aes(x=px, y=pz_adj))+labelz+xrange+yrange
      if (geom %in% "bin") t <- t + geom_bin2d(...) 
      if (geom %in% "hex") t <- t + geom_hex(...)
      if (geom %in% "tile") t <- t + stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)
      #Contours and strikezones are drawn last
      if (contour) t <- t + geom_density2d()
      t <- t + white_zone
      return(t+layers)
    } else { #densities are differenced (note that diffDensity handles the subsetting)
      if (!is.null(facets)) {
        stuff <- dlply(FX, facets, function(x) { diffDensity(x, density1, density2, limz=limitz) } )
        densities <- ldply(stuff)
      } else densities <- diffDensity(FX, density1, density2, limz=limitz)
    }
    t2 <- plotDensity(densities, boundaries, contour, geom, ...)
    t2 <- t2 + black_zone
    return(t2+scale_fill_gradient2(midpoint=0)+labelz+xrange+yrange+layers)
  }
  if (geom %in% "point") {
    p <- ggplot(data=FX, aes(ymax=top, ymin=bottom, xmax=right, xmin=left)) + legendz + labelz + xrange + yrange + scale_size(guide = "none") + scale_alpha(guide="none")
    p <- p + black_zone
    if (color == "") {
      point_mapping <- aes_string(x = "px", y="pz_adj")
    } else {
      point_mapping <- aes_string(x = "px", y="pz_adj", colour = color)
    }
    if (contour) p <- p + geom_density2d(point_mapping, colour="black")
    p <- p + geom_point(mapping=point_mapping, size=point.size, alpha=point.alpha, ...)
    return(p + layers)
  }
}

#fit a model and return the predicted values for a (possibly differenced) 2D grid
fitModel <- function(dat, expr, grid) {
  fun <- as.character(expr[[1]])
  expr[[1]] <- NULL
  expr$data <- dat #overwrite data argument
  fit <- do.call(fun, expr)
  pred.grid <- predict(fit, grid, type="response")
  grid.df <- data.frame(x=grid[,1], y=grid[,2], z=pred.grid)
  grid.df
}

#Plot a 2D density with pre-computed heights
plotDensity <- function(dens, bounds, contour, geom, ...){
  px=pz_adj=..density..=top=bottom=right=left=x=y=z=NULL #ugly hack to comply with R CMD check
  common <- intersect(names(dens), names(bounds[[2]]))
  if (length(common) == 0) { #Artifically create a variable (stand) if none exists (required for joining and thus drawing strikezones)
    nhalf <- dim(dens)[1]/2
    if (nhalf == floor(nhalf)) {
      dens$stand <- c(rep("R", nhalf), rep("L", nhalf))
    } else {
      dens$stand <- c(rep("R", nhalf), rep("L", nhalf), "L")
    }
  }
  dens.df <- join(dens, bounds[[2]], type="inner") #defaults to join "by" all common variables
  p <- ggplot(data=dens.df)
  if (geom %in% "hex") {
    p <- p + stat_summary_hex(aes(x=x,y=y,z=z), ...)
  } else {
    p <- p + stat_summary2d(aes(x=x,y=y,z=z), ...) 
  }
  if (contour) p <- p + stat_contour(aes(x=x,y=y,z=z)) #passing binwidth here throws error
  return(p)
}


# Special subset handling for density estimates
# 
# @param data PITCHf/x data
# @param density either density1 or density2 passed on from \link{strikeFX}

subsetFX <- function(data, density) {
  if (length(density) == 1) {
    index <- data[,names(density)] %in% density
    subset(data, index)
  } else {
    if (length(density) > 1) warning("The length of each density parameter should be 0 or 1.")
    data
  }
}

# Differenced 2D Kernel Density Estimates
#
# Computes differenced 2D Kernel Density Estimates using MASS::kde2d
#
# Computes two densities on the same support and subtracts them according to the \code{density} expression.
# 
# 
# @param data PITCHf/x data
# @param density1 either density1 or density2 passed on from \link{strikeFX}.
# @param density2 either density1 or density2 passed on from \link{strikeFX}.
# @param limz MASS::kde2d paramaters passed from \link{strikeFX}.
# @return Returns a data frame with differenced density estimates as column z.

diffDensity <- function(data, density1, density2, limz){
  data1 <- subsetFX(data, density1)
  data2 <- subsetFX(data, density2)
  est1 <- kde2d(data1[,"px"], data1[,"pz_adj"], n=100, lims=limz)
  grid <- expand.grid(x = est1$x, y = est1$y)
  est2 <- kde2d(data2[,"px"], data2[,"pz_adj"], n=100, lims=limz)
  grid["z"] <- as.vector(est1$z) - as.vector(est2$z)
  return(grid)
}
