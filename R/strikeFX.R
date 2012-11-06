#' Visualize PITCHf/x strikezones
#' 
#' Pitch locations as they crossed home plate.
#' 
#' Scatterplot with "px" on the horizonatl axis and "pz" on the vertical axis.
#'
#' @param data PITCHf/x data to be visualized.
#' @param layer list of other ggplot2 (layered) modifications.
#' @param geom type of geometry used for plotting.
#' @param adjust logical vector. Should vertical locations be adjusted according to batter height?
#' @param point.color variable used to control coloring scheme when \code{geom = "point"}.
#' @param point.alpha variable used to control alpha when \code{geom = "point"}.
#' @param point.size control size of "points".
#' @return Returns a ggplot2 object.
#' @export
#' @examples
#' #value
#' 

strikeFX <- function(data, layer = list(), geom = "point", adjust=TRUE, point.color = aes(color = pitch_types), point.alpha = aes(alpha = 0.5), point.size = 100){ 
  #Add descriptions to pitch_types
  if (!geom %in% c("point", "hex", "density2d", "tile", "tile_diff")) warning("Current functionality is designed to support the following geometries: 'point', 'hex', 'density2d', 'tile'.")
  if ("pitch_type" %in% names(data)) {
    types <- cbind(pitch_type=c("SI", "FF", "IN", "SL", "CU", "CH", "FT", "FC", "PO", "KN", "FS", "FA", NA, "FO"),
                   pitch_types=c("Sinker", "Fastball (four-seam)", "Intentional Walk", "Slider", "Curveball", "Changeup", 
                                 "Fastball (two-seam)", "Fastball (cutter)", "Pitchout", "Knuckleball", "Fastball (split-finger)",
                                 "Fastball", "Unknown", "Fastball ... (FO?)"))
    data <- merge(data, types, by = "pitch_type")
  } else warning("It may be beneficial to have a 'pitch_type' column.")
  locations <- c("px", "pz")
  FX <- data[complete.cases(data[,locations]),] #get rid of records missing the necessary parameters
  for (i in locations)
    FX[,i] <- as.numeric(FX[,i])
  color <- as.list(match.call())$point.color
  if (!is.null(color)){
    colors <- gsub("[)]", "", gsub("aes[(]color = ","", color))[2]
  } else colors <- "pitch_types"
  layers <- as.character(as.list(match.call())$layer)
  facets <- getFacets(layers)
  if ("p_throws" %in% names(FX)) FX$p_throws <- paste("Pitcher Throws:", FX$p_throws) #Add suffixes for context
  if ("stand" %in% names(FX)) FX$stand <- paste("Batter Stands:", FX$stand)
  if ("b_height" %in% names(FX)) {
    boundaries <- getStrikezones(FX, facets, strikeFX = TRUE) #Strikezone boundaries
    if (adjust) {
      FX$pz_adj <- boundaries[[1]] #adjusted vertical locations
    } else FX$pz_adj <- FX$pz
    zones <- geom_rect(data = boundaries[[2]], aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha = 0.2, color="grey20") #draw strikezones
  } else {
    FX$pz_adj <- FX$pz
    zones <- NULL
    warning("Strikezones and location adjustments depend on the stance (and height) of the batter. Make sure these variables are being entered as 'stand' and 'b_height', respectively. Otherwise, strikezones will not appear and locations will not be adjusted.")
  }
  for (i in locations)
    FX[,i] <- as.numeric(FX[,i])
  if (geom %in% "tile_diff") {
    if (!is.null(facets)) {
      stuff <- dlply(FX, facets, DifferencedDensity)
      densities <- ldply(stuff)
    } else {
      densities <- DifferencedDensity(FX, bandwidth, gridsize, range.x, truncate)
    }
    return(ggplot(data=densities, aes(x=x,y=y,z=z))+geom_tile(aes(fill = z))+stat_contour()+layer)
  }
  p <- ggplot() + xlim(-2.5, 2.5) + xlab("Horizontal Pitch Location") + ylim(0, 5) + ylab("Height from Ground") + scale_size(guide = "none") + scale_alpha(guide="none") + theme(legend.position = c(0.25,0.05), legend.direction = "horizontal") + scale_color_brewer(palette="Set2")
  if (geom %in% "point") {
    FX$sizes <- point.size
    p <- p + layer(data = FX, mapping = aes(x = px, y = pz_adj, size = sizes), geom = geom) + point.color + point.alpha #+ aes(...) #+ scale_size_continuous(limits=c(min(sizes), max(sizes)))
  }
  if (geom %in% c("hex", "density2d")) p <- p + layer(data = FX, mapping = aes(x = px, y = pz_adj), geom = geom)
  print(p + zones + layer)
}

#' Differenced 2D Binned Kernel Density Estimates
#'
#' Computes differenced 2D Binned Kernel Density Estimates using bkde2D{KernSmooth}
#'
#' Details go here.
#' @param data PITCHf/x data
#' 
#' @return Returns a data frame with binning values and density estimates. Similar to stat_contour?

DifferencedDensity <- function(data, bandwidth=c(0.1, 0.1), gridsize = c(51L, 51L), range.x = list(c(-2,2),c(1,4)), truncate = TRUE){ #dynamic handling of contours?
  x.s <- subset(data, des == "Called Strike")[,c("px","pz_adj")]
  est.s <- bkde2D(x.s, bandwidth, gridsize, range.x, truncate)
  grid <- expand.grid(x = est.s$x1, y = est.s$x2) #Grid (ie, x and y values) to be used for both balls and strikes
  x.b <- subset(data, des == "Ball")[,c("px","pz_adj")]
  est.b <- bkde2D(x.b, bandwidth, gridsize, range.x, truncate)
  densities <- cbind(grid, z_s=melt(est.s$fhat)$value, z_b=melt(est.b$fhat)$value)
  densities$z <- densities$z_s - densities$z_b
  return(densities)
}
