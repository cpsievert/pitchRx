#' Visualize PITCHf/x strikezones
#' 
#' Pitch locations as they crossed home plate.
#' 
#' Scatterplot with "px" on the horizontal axis and "pz" on the vertical axis.
#'
#' @param data PITCHf/x data to be visualized.
#' @param geom plotting geometry. Multiple geometries can be plotted at once, but only considers: "point", "hex", "bin" and "contour". 
#' @param point.size Size of points
#' @param point.alpha ggplot2 alpha parameter
#' @param color variable used to control coloring scheme.
#' @param density1 List defines a density estimate.
#' @param density2 List defines a density estimate. If \code{density1 != density2}, the density estimates are automatically differenced.
#' @param contour logical. Should contour lines be included?
#' @param adjust logical. Should vertical locations be adjusted according to batter height?
#' @param layer list of other ggplot2 (layered) modifications.
#' @param limitz limits for horizontal and vertical axes. 
#' @param ... extra options passed onto geom commands
#' @return Returns a ggplot2 object.
#' @export
#' @examples
#' data(pitches)
#' strikeFX(pitches)
#' strikeFX(pitches, geom="tile")
#' strikeFX(pitches, geom="tile", contour=TRUE)
#' strikeFX(pitches, geom="tile", contour=TRUE, layer=facet_grid(.~stand))
#' strikeFX(pitches, geom="tile", contour=TRUE, layer=facet_grid(pitcher_name~stand))
#' strikeFX(pitches, geom="hex")
#' strikeFX(pitches, geom="hex", contour=TRUE, binwidth=c(0.1, 0.1))
#' strikeFX(pitches, geom="hex", contour=TRUE, density1=list(des="Called Strike"), density2=list(des="Ball"))
#' strikeFX(pitches, geom="hex", contour=TRUE, density1=list(des="Called Strike"), density2=list(des="Ball"), layer=facet_grid(.~stand))
#' 

strikeFX <- function(data, geom = "point", point.size=3, point.alpha=1/3, color = "pitch_types", density1=list(), density2=list(), contour=FALSE, adjust=TRUE, layer = list(), limitz=c(-2.5, 2.5, 0, 5), ...){ 
  if (any(!geom %in% c("point", "bin", "hex", "tile"))) warning("Current functionality is designed to support the following geometries: 'point', 'bin', 'hex', 'tile'.")
  if ("pitch_type" %in% names(data)) { #Add descriptions as pitch_types
    data$pitch_type <- factor(data$pitch_type)
    types <- data.frame(pitch_type=c("SI", "FF", "IN", "SL", "CU", "CH", "FT", "FC", "PO", "KN", "FS", "FA", NA, "FO"),
                   pitch_types=c("Sinker", "Fastball (four-seam)", "Intentional Walk", "Slider", "Curveball", "Changeup", 
                                 "Fastball (two-seam)", "Fastball (cutter)", "Pitchout", "Knuckleball", "Fastball (split-finger)",
                                 "Fastball", "Unknown", "Fastball ... (FO?)"))
    data <- join(data, types, by = "pitch_type", type="inner")
  } 
  if (!color %in% names(data)) {
    warning(paste(color, "is the variable that defines coloring but it isn't in the dataset!"))
    color <- ""
  }
  locations <- c("px", "pz")
  FX <- data[complete.cases(data[,locations]),] #get rid of records missing the necessary parameters
  for (i in locations)
    FX[,i] <- as.numeric(FX[,i])
  layers <- as.character(as.list(match.call())$layer)
  facets <- getFacets(layers)
  if ("p_throws" %in% names(FX)) FX$p_throws <- paste("Pitcher Throws:", FX$p_throws) #Add suffixes for context
  if ("stand" %in% names(FX)) FX$stand <- paste("Batter Stands:", FX$stand)
  if ("b_height" %in% names(FX)) { #plot strikezones (and adjust loactions) if heights are numeric
    boundaries <- getStrikezones(FX, facets, strikeFX = TRUE) 
    if (adjust) {
      FX$pz_adj <- boundaries[[1]] #adjusted vertical locations
    } else FX$pz_adj <- FX$pz # "adjusted" vert locations
    FX <- join(FX, boundaries[[2]], by="stand", type="inner")
  } else {
    FX$pz_adj <- FX$pz
    zones <- NULL
    warning("Strikezones and location adjustments depend on the stance (and height) of the batter. Make sure these variables are being entered as 'stand' and 'b_height', respectively.")
  }
  for (i in locations)
    FX[,i] <- as.numeric(FX[,i])
  #Recycled plot formats
  labelz <- labs(x = "Horizontal Pitch Location", y = "Height from Ground")
  legendz <- theme(legend.position = c(0.25,0.05), legend.direction = "horizontal")
  xrange <- xlim(limitz[1:2])
  yrange <- ylim(limitz[3:4])
  if (geom %in% c("bin", "hex", "tile")) { #special handling for (2D) density geometries
    if (identical(density1, density2)) { #densities are not differenced
      FX1 <- subsetFX(FX, density1)
      t <- ggplot(data=FX1, aes(x=px, y=pz_adj))+labelz+xrange+yrange
      if (geom %in% "bin") t <- t + geom_bin2d(...) 
      if (geom %in% "hex") t <- t + geom_hex(...)
      if (geom %in% "tile") t <- t + stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)
      #Contours and strikezones are drawn last
      if (contour) t <- t + geom_density2d(...)
      t <- t + geom_rect(mapping=aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha=0, fill="pink", colour="white")
      return(t+layer)
    } else { #densities are differenced
      if (!is.null(facets)) {
        stuff <- dlply(FX, facets, function(x) { diffDensity(x, density1, density2, limz=limitz) } )
        densities <- ldply(stuff)
      } else {
        densities <- diffDensity(FX, density1, density2, limz=limitz)
      }
      if (!"stand" %in% names(densities)) {
        nhalf <- dim(densities)[1]/2
        densities$stand <- c(rep("Batter Stands: R", nhalf), rep("Batter Stands: L", nhalf))
      }
    }
    densities <- join(densities, boundaries[[2]], by="stand", type="inner")
    t2 <- ggplot(data=densities, aes(x,y))+labelz+xrange+yrange+scale_fill_gradient2(midpoint=0)
    if (geom %in% c("bin", "tile")) t2 <- t2 + stat_summary2d(aes(z=z), ...) #shouldn't use fun=log, since we have differenced densities
    if (geom %in% "hex") t2 <- t2 + stat_summary_hex(aes(z=z), ...)
    #Contours and strikezones are drawn last
    #if (contour) t2 <- t2 + stat_density2d(aes(z=z), ...)
    if (contour) t2 <- t2 + stat_contour(aes(z=z)) #passing binwidth here throws error
    t2 <- t2 + geom_rect(mapping=aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha=0, fill="pink", colour="grey20")
    return(t2+layer)
  }
  if (geom %in% "point") {
    p <- ggplot(data=FX, aes(ymax=top, ymin=bottom, xmax=right, xmin=left)) + legendz + labelz + xrange + yrange + scale_size(guide = "none") + scale_alpha(guide="none")
    p <- p + geom_rect(mapping=aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha=0, fill="pink", colour="black") #draw strikezones
    if (color == "") {
      point_mapping <- aes_string(x = "px", y="pz_adj")
    } else {
      point_mapping <- aes_string(x = "px", y="pz_adj", colour = color)
    }
    if (contour) p <- p + geom_density2d(point_mapping, colour="black")
    p <- p + geom_point(mapping=point_mapping, size=point.size, alpha=point.alpha, ...)
    return(p + layer)
  }
}


#' Special subset handling for density estimates
#' 
#' @param data PITCHf/x data
#' @param density either density1 or density2 passed on from \link{strikeFX}

subsetFX <- function(data, density) {
  if (length(density) == 1) {
    index <- data[,names(density)] %in% density
    subset(data, index)
  } else {
    if (length(density) > 1) warning("The length of each density parameter should be 0 or 1.")
    data
  }
}

#' Differenced 2D Kernel Density Estimates
#'
#' Computes differenced 2D Kernel Density Estimates using MASS::kde2d
#'
#' Computes two densities on the same support and subtracts them according to the \code{density} expression.
#' The expression should look as follows: variable~value1-value2.
#' 
#' @param data PITCHf/x data
#' @param density1 either density1 or density2 passed on from \link{strikeFX}.
#' @param density2 either density1 or density2 passed on from \link{strikeFX}.
#' @param lims MASS::kde2d paramaters passed from \link{strikeFX}.
#' @return Returns a data frame with differenced density estimates as column z.

diffDensity <- function(data, density1, density2, limz){
  data1 <- subsetFX(data, density1)
  data2 <- subsetFX(data, density2)
  est1 <- kde2d(data1[,"px"], data1[,"pz_adj"], n=100, lims=limz)
  grid <- expand.grid(x = est1$x, y = est1$y)
  est2 <- kde2d(data2[,"px"], data2[,"pz_adj"], n=100, lims=limz)
  grid["z"] <- as.vector(est1$z) - as.vector(est2$z)
  return(grid)
}
