#' Animate PITCHf/x
#' 
#' Pitch trajectories animated on a two-dimensional plot. 
#' 
#' \code{animateFX} plots a series of "snapshots" with pitch locations from the point of release - 
#' when the ball leaves the pitcher's hand - until all of them reach home plate. 
#' The graphic takes on the viewpoint of the umpire; that is, the pitches are getting closer 
#' to the viewer with time. This is relected with the increase in size of the "balls" as the 
#' animation progresses. To reduce the time and thinking required to produce plots, \code{animateFX} 
#' makes several assumptions about the opacity and size associated with each "snapshot" 
#' of pitch locations.
#'
#' @param data PITCHf/x data to be visualized.
#' @param layer list of other ggplot2 (layered) modifications.
#' @param geom type of geometry used for plotting.
#' @param point.color variable used to control coloring scheme when \code{geom = "point"}.
#' @param point.alpha variable used to control alpha when \code{geom = "point"}.
#' @param point.size control size of "points". Theoretically, this should be based on the distance from home plate (ie, \code{snapshot$y})
#' @param breaks bin breaks for counts when \code{geom == "hex"}.
#' @param interval time (in seconds) between plotting the pitch locations.
#' @param sleep passed along to Sys.sleep() to flush current plot.
#' @return Returns a series of ggplot2 objects.
#' @export
#' @examples
#' #First, grab some data
#' 
#' #Now, the fun part
#' 
#' 

animateFX <- function(data, layer = list(), geom = "point", point.color = aes(color = pitch_types), point.alpha = aes(alpha = 0.5), point.size = 100-FX$y, breaks = c(0,5,10), interval = 0.01, sleep = 0.000000000001, ...){ 
  #Add descriptions to pitch_types
  if (!geom %in% c("point", "hex", "density2d", "tile")) warning("Current functionality is designed to support the following geometries: 'point', 'hex', 'density2d', 'tile'.")
  if ("pitch_type" %in% names(data)) {
    types <- cbind(pitch_type=c("SI", "FF", "IN", "SL", "CU", "CH", "FT", "FC", "PO", "KN", "FS", "FA", NA, "FO"),
                   pitch_types=c("Sinker", "Fastball (four-seam)", "Intentional Walk", "Slider", "Curveball", "Changeup", 
                                 "Fastball (two-seam)", "Fastball (cutter)", "Pitchout", "Knuckleball", "Fastball (split-finger)",
                                 "Fastball", "Unknown", "Fastball ... (FO?)"))
    data <- merge(data, types, by = "pitch_type")
  } else warning("Make sure you have the appropriately named 'pitch_type' column.")
  idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
  if (!all(idx %in% names(data))) warning("You must have the following variables in your dataset to animate pitch locations: 'x0', 'y0', 'z0', 'vx0', 'vy0', 'vz0', 'ax', 'ay', 'az'")
  complete <- data[complete.cases(data[,idx]),] #get rid of records with any missing parameters
  color <- as.list(match.call())$point.color
  if (!is.null(color)){
    colors <- gsub("[)]", "", gsub("aes[(]color = ","", color))[2]
  } else colors <- "pitch_types"
  layers <- as.character(as.list(match.call())$layer)
  facets <- getFacets(layers)
  reordered <- ddply(complete, facets, function(x) { #Does this do anything if facets is NULL?
    x[, colors] <- reorder(x[, colors], x[, colors], length)
    x[rev(order(x[, colors])), ]
  })
  parameters <- reordered[, names(reordered) %in% idx]
  snapshots <- getSnapshots(parameters)
  other <- reordered[, !(names(reordered) %in% idx)] #Keep 'other' variables for faceting/coloring
  if ("p_throws" %in% names(other)) other$p_throws <- paste("Pitcher Throws:", other$p_throws) #Add suffixes for context
  if ("stand" %in% names(other)) other$stand <- paste("Batter Stands:", other$stand)
  if ("b_height" %in% names(other)) {
    boundaries <- getStrikezones(other, facets, strikeFX = FALSE) #Strikezone boundaries
    zones <- geom_rect(data = boundaries, aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha = 0.2, color="grey20") #draw strikezones
  } else {
    zones <- NULL
    warning("Strikezones depend on the stance (and height) of the batter. Make sure these variables are being entered as 'stand' and 'b_height', respectively.")
  }
  ctr <- 1 #Used to check whether or not batter has decided to swing
  N <- dim(snapshots)[2] #Number of plots
  swing <- NULL
  for (i in 1:N) {
    if (ctr > (2/5)*N) swing <- annotate("text", label = "SWING!", x = 0, y = 6, size = 2, colour = "red"))
    FX <- data.frame(snapshots[,i,], other)
    names(FX) <- c("x", "y", "z", names(other))
    Sys.sleep(sleep)
    if (geom %in% "tile") {
      if (!is.null(facets)) {
        stuff <- dlply(FX, facets, function(x) { getDensity(x, density=tile.density) } )
        densities <- ldply(stuff)
      } else {
        densities <- getDensity(FX, density=tile.density)
      }
      t <- ggplot() + xlab("Horizontal Pitch Location")+ylab("Height from Ground")+scale_fill_gradient2(midpoint=0)
      return(t+layer(data=densities, mapping = aes(x=x,y=y,fill=z), geom="tile")+zones+swing+layer)
    }
    p <- ggplot() + xlim(-3.5, 3.5) + xlab("Horizontal Pitch Location") + ylim(0, 7) + ylab("Height from Ground") + scale_size(guide="none") + scale_alpha(guide="none") + theme(legend.position = c(0.25,0.05), legend.direction = "horizontal")
    if (geom %in% "point") p <- p + layer(data = FX, mapping = aes(x = x, y = z, size = 100-y), geom = geom) + point.color + point.alpha
    if (geom %in% c("hex", "density2d")) p <- p + layer(data = FX, mapping = aes(x = x, y = z), geom = geom) + scale_fill_continuous(breaks = breaks)
    print(p + swing + zones + layer)
    ctr <- ctr + 1
  }
}