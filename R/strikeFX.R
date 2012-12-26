#' Visualize PITCHf/x strikezones
#' 
#' Pitch locations as they crossed home plate.
#' 
#' Scatterplot with "px" on the horizontal axis and "pz" on the vertical axis.
#'
#' @param data PITCHf/x data to be visualized.
#' @param geom type of geometry used for plotting.
#' @param point.size Size of points
#' @param point.alpha ggplot2 alpha parameter
#' @param color variable used to control coloring scheme.
#' @param tile.density If geom="tile", this formula defines the (possibly differenced) 2D Kernel Density Estimates.
#' @param breaks bin breaks for counts when \code{geom == "hex"}.
#' @param adjust logical vector. Should vertical locations be adjusted according to batter height?
#' @param layer list of other ggplot2 (layered) modifications.
#' @param ... extra options passed onto geom commands
#' @return Returns a ggplot2 object.
#' @export
#' @examples
#' data(pitches)
#' strikeFX(pitches)
#' strikeFX(pitches, layer=facet_grid(pitcher_name~stand))
#' strikeFX(pitches, geom="tile", layer=facet_grid(pitcher_name~stand))
#' strikeFX(pitches, geom="tile", tile.density=des~Called.Strike-Ball, layer=facet_grid(pitcher_name~stand))
#' 

strikeFX <- function(data, geom = "point", point.size=3, point.alpha=1/3, color = "pitch_types", tile.density=des~Called.Strike, breaks = c(0,5,10), adjust=TRUE, layer = list(), ...){ 
  if (!geom %in% c("point", "hex", "density2d", "tile")) warning("Current functionality is designed to support the following geometries: 'point', 'hex', 'density2d', 'tile'.")
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
  xrange <- xlim(-2.5,2.5)
  yrange <- ylim(0,5)
  if (geom %in% "tile") {
    formula <- as.list(tile.density)
    vars <- formula[-grep("~", formula)]
    elem <- llply(vars, function(x) gsub("\\.", " ", x)) 
    if (length(elem[[2]]) == 1) { 
      tile_mapping <- aes_string(x="px",y="pz_adj")
      t <- ggplot(data=FX, mapping=tile_mapping)+labelz+xrange+yrange
      t <- t + stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)
      return(t+layer+geom_rect(mapping=aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha=0, fill="pink", colour="white")) #draw strikezones
    } else { #Special handling for differenced density estimates
      values <- elem[[2]][-grep("\\+|\\-|\\*|\\/", elem[[2]])] #throw away math symbols
      if (!is.null(facets)) {
        stuff <- dlply(FX, facets, function(x) { diffDensity(x, var=elem[[1]], values=values) } )
        densities <- ldply(stuff)
      } else {
        densities <- diffDensity(FX, var=elem[[1]], values=values)
      }
      densities <- join(densities, boundaries[[2]], by="stand", type="inner")
      aes_map2 <- aes_string(x="x",y="y")
      t <- ggplot(data=densities, mapping=aes_map2)+labelz+xrange+yrange
      t <- t + geom_tile(aes(fill=z))+layer+scale_fill_gradient2(midpoint=0)
     return(t+geom_rect(mapping=aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha=0, fill="pink", colour="black")) #draw strikezones
    }
  }
  # Color aesthetic requires special handling, because it can be "none".
  p <- ggplot(data=FX, aes(ymax=top, ymin=bottom, xmax=right, xmin=left)) + legendz + labelz + xrange + yrange #+ scale_size(guide = "none") + scale_alpha(guide="none")
  p <- p + geom_rect(mapping=aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha=0, fill="pink", colour="black") #draw strikezones
  if (color == "") {
    point_mapping <- aes_string(x = "px", y="pz_adj")
  } else {
    point_mapping <- aes_string(x = "px", y="pz_adj", colour = color)
  }
  if (geom %in% "point") p <- p + geom_point(mapping=point_mapping, size=point.size, alpha=point.alpha, ...)
  #if (geom %in% "bin2d") p <- p + geom_bin2d() + scale_fill_continuous(breaks = breaks) #Implement similar to stat_density2d?
  #if (geom %in% "density2d") p <- p + geom_density2d()
  #if (geom %in% "raster") p <- p + geom_raster()
  #if (geom %in% c("hex", "density2d")) p <- p + layer(data = FX, mapping = aes(x = px, y = pz_adj), geom = geom)
  print(p + layer)
}

#' Differenced 2D Kernel Density Estimates
#'
#' Computes differenced 2D Kernel Density Estimates using MASS::kde2d
#'
#' Computes two densities on the same support and subtracts them according to the \code{density} expression.
#' The expression should look as follows: variable~value1-value2.
#' 
#' @param data PITCHf/x data
#' @param var variable of interest
#' @param values values of var used for 2D Kernel Density Estimation.
#' @return Returns a data frame with differenced density estimates.

diffDensity <- function(data, var, values){ #dynamic handling of contours?
  ctr <- 1
  for (i in values) {
    points <- data[data[var] == i, c("px", "pz_adj")]
    est <- kde2d(points[,1], points[,2], n=100, lims=c(-2.5, 2.5, 0, 5))
    if (ctr == 1) grid <- expand.grid(x = est$x, y = est$y)
    grid[i] <- as.vector(est$z)
    ctr <- ctr + 1
  }
  grid["z"] <- grid[values[1]] - grid[values[2]]
  return(grid)
}
