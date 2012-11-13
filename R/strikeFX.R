#' Visualize PITCHf/x strikezones
#' 
#' Pitch locations as they crossed home plate.
#' 
#' Scatterplot with "px" on the horizonatl axis and "pz" on the vertical axis.
#'
#' @param data PITCHf/x data to be visualized.
#' @param layer list of other ggplot2 (layered) modifications.
#' @param geom type of geometry used for plotting.
#' @param tile.density If geom="tile", this formula describes the (possibly differenced) 2D Binned Kernel Density Estimates.
#' @param adjust logical vector. Should vertical locations be adjusted according to batter height?
#' @param point.color variable used to control coloring scheme when \code{geom = "point"}.
#' @param point.alpha variable used to control alpha when \code{geom = "point"}.
#' @param point.size control size of "points".
#' @return Returns a ggplot2 object.
#' @export
#' @examples
#' #value
#' 

strikeFX <- function(data, layer = list(), geom = "point", tile.density=des~Called.Strike-Ball, adjust=TRUE, point.color = aes(color = pitch_types), point.alpha = aes(alpha = 0.5), point.size = 100){ 
  #Add descriptions to pitch_types
  if (!geom %in% c("point", "hex", "density2d", "tile")) warning("Current functionality is designed to support the following geometries: 'point', 'hex', 'density2d', 'tile'.")
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
  if (geom %in% "tile") {
    if (!is.null(facets)) {
      stuff <- dlply(FX, facets, function(x) { getDensity(x, density=tile.density) } )
      densities <- ldply(stuff)
    } else {
      densities <- getDensity(FX, density=tile.density)
    }
    t <- ggplot() + xlab("Horizontal Pitch Location")+ylab("Height from Ground")+scale_fill_gradient2(midpoint=0)
    return(t+layer(data=densities, mapping = aes(x=x,y=y,fill=z), geom="tile")+zones+layer)
  }
  p <- ggplot() + xlim(-2.5, 2.5) + xlab("Horizontal Pitch Location") + ylim(0, 5) + ylab("Height from Ground") + scale_size(guide = "none") + scale_alpha(guide="none") + theme(legend.position = c(0.25,0.05), legend.direction = "horizontal")
  if (geom %in% "point") {
    FX$sizes <- point.size
    p <- p + layer(data = FX, mapping = aes(x = px, y = pz_adj, size = sizes), geom = geom) + point.color + point.alpha #+ aes(...) #+ scale_size_continuous(limits=c(min(sizes), max(sizes)))
  }
  if (geom %in% c("hex", "density2d")) p <- p + layer(data = FX, mapping = aes(x = px, y = pz_adj), geom = geom)
  return(p + zones + layer)
}

#' 2D Binned Kernel Differenced Density Estimates
#'
#' Computes differenced 2D Binned Kernel Density Estimates using KernSmooth::bkde2D
#'
#' Computes two densities on the same support and subtracts them according to the \code{density} expression.
#' The expression should look as follows: variable~value1-value2.
#' 
#' @param data PITCHf/x data
#' @param density formula that describes the (possibly differenced) 2D Binned Kernel Density Estimates.
#' @param bandwidth KernSmooth::bkde2D parameter
#' @param gridsize KernSmooth::bkde2D parameter
#' @param range.x KernSmooth::bkde2D parameter
#' @param truncate KernSmooth::bkde2D parameter
#' @return Returns a data frame with binning values and density estimates.

getDensity <- function(data, density, bandwidth=c(0.1, 0.1), gridsize = c(51L, 51L), range.x = list(c(-2,2),c(1,4)), truncate = TRUE){ #dynamic handling of contours?
  formula <- as.list(density)
  vars <- formula[-grep("~", formula)]
  elem <- llply(vars, function(x) gsub("\\.", " ", x))
  if (length(elem[[2]]) > 1) { #multivariate density
    values <- elem[[2]][-grep("\\+|\\-|\\*|\\/", elem[[2]])] #throw away math symbols
    symbols <- elem[[2]][grep("\\+|\\-|\\*|\\/", elem[[2]])] #keep math symbols
    ctr <- 1
    for (i in values) {
      x <- data[data[elem[[1]]] == i, c("px", "pz_adj")]
      est <- bkde2D(x, bandwidth, gridsize, range.x, truncate)
      if (ctr == 1) grid <- expand.grid(x = est$x1, y = est$x2)
      grid[i] <- melt(est$fhat)$value
      ctr <- ctr + 1
    }
    grid["z"] <- grid[values[1]] - grid[values[2]]
  } else {
    x <- data[data[elem[[1]]] == elem[[2]], c("px", "pz_adj")]
    est <- bkde2D(x, bandwidth, gridsize, range.x, truncate)
    grid <- expand.grid(x = est$x1, y = est$x2)
    grid["z"] <- melt(est$fhat)$value
  }
  return(grid)
}
