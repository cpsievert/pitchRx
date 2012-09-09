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
#' @param geom type of geometry used for plotting.
#' @param layer list of other ggplot2 (layered) modifications.
#' @param breaks bin breaks for counts when geom == "hex"
#' @param interval time (in seconds - real time) between plotting the pitch locations.
#' @param sleep passed along to Sys.sleep() to flush current plot.
#' @param freeze visualize first or last frame of animation.
#' @return Returns a series of ggplot2 objects.
#' @export
#' @examples
#' #First, grab some data
#' 
#' #Now, the fun part
#' 
#' 

animateFX <- function(data, geom = "point", layer = list(), breaks = c(0,5,10), interval = 0.01, sleep = 0.000000000001, freeze = NULL){ 
  #Add descriptions to pitch_types
  if (!geom %in% c("point", "hex", "density2d", "tile")) warning("Current functionality is designed to support the following geometries: 'point', 'hex', 'density2d', 'tile'.")
  if (!"pitch_type" %in% names(data)) warning("Make sure you have the appropriate 'pitch_type' column. If you don't have 'pitch_type', consider using ggFX()")
  types <- cbind(pitch_type=c("SI", "FF", "IN", "SL", "CU", "CH", "FT", "FC", "PO", "KN", "FS", "FA", NA, "FO"),
                   pitch_types=c("Sinker", "Fastball (four-seam)", "Intentional Walk", "Slider", "Curveball", "Changeup", 
                                 "Fastball (two-seam)", "Fastball (cutter)", "Pitchout", "Knuckleball", "Fastball (split-finger)",
                                 "Fastball", "Unknown", "Fastball ... (FO?)"))
  pitchFX <- merge(data, types, by = "pitch_type")
  idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
  if (!all(idx %in% names(data))) warning("You must have the following variables in your dataset to animate pitch locations: 'x0', 'y0', 'z0', 'vx0', 'vy0', 'vz0', 'ax', 'ay', 'az'")
  complete <- pitchFX[complete.cases(pitchFX[,idx]),] #get rid of records with any missing parameters
  layers <- as.list(match.call())$layer
  facets <- layers[grep("facet", as.list(layers)) + 1]
  facets2 <- llply(str_split(as.character(facets), "~"), str_trim)
  facet <- unlist(llply(facets2, function(x) { x[!x %in% "."] }))
  if (length(facet) == 0) facet <- NULL
  color <- layers[grep("color", layers)]
  if (length(color) == 0) color <- "pitch_type"
  reordered <- ddply(complete, facet, function(x) {
    x[, color] <- reorder(x[, color], x[, color], length)
    x[rev(order(x[, color])), ]
  })
  parameters <- reordered[, names(reordered) %in% idx]
  snapshots <- getSnapshots(parameters)
  other <- reordered[, !(names(reordered) %in% idx)] #Keep 'other' variables for faceting/coloring
  if ("p_throws" %in% names(other)) other$p_throws <- paste("Pitcher Throws:", other$p_throws) #Add suffixes for context
  if ("stand" %in% names(other)) other$stand <- paste("Batter Stands:", other$stand)
  if ("b_height" %in% names(other)) {
    boundaries <- getStrikezones(data = other, facet) #Strikezone boundaries
  } else warning("Strikezones depend on the stance (and height) of the batter. Make sure these variables are being entered as 'stand' and 'b_height', respectively.")
#   if (!is.null(freeze)) {
#     n <- dim(snapshots)[2]
#     if (freeze == "first") snapshot <- data.frame(snapshots[,1,], other)
#     if (freeze == "last") snapshot <- data.frame(snapshots[,n,], other)
#     names(snapshot) <- c("x", "y", "z", names(other))
#     p <- ggplot() + xlim(-3.5, 3.5) + xlab("Horizontal Pitch Location") + ylim(0, 7) + ylab("Height from Ground") + scale_size(guide="none") + scale_alpha(guide="none") + scale_color_brewer(palette="Set2")
#     if (geom %in% "point") p <- p + layer(data = snapshot, mapping = aes(x = x, y = z, size = 100 - y), geom = geom) + aes(alpha = 0.5, color = pitch_type)
#     if (geom %in% c("hex", "density2d")) p <- p + layer(data = snapshot, mapping = aes(x = x, y = z), geom = geom)
#     if (geom %in% "tile") p <- p + geom_tile(data = snapshot, mapping = aes(x = x, y = z, fill = ..count..))
#     print(p + geom_rect(data = boundaries, aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha = 0.2, color="grey20") #draw strikezones
#           + layer)
#   } else {
  for (i in 1:dim(snapshots)[2]) {
    snapshot <- data.frame(snapshots[,i,], other)
    names(snapshot) <- c("x", "y", "z", names(other))
#     snapshot2 <- ddply(snapshot, facet, function(x) {
#       x[, color] <- reorder(x[, color], x[, color], length)
#       x[rev(order(x[, color])), ]
#     })
    Sys.sleep(sleep)
    p <- ggplot() + xlim(-3.5, 3.5) + xlab("Horizontal Pitch Location") + ylim(0, 7) + ylab("Height from Ground") + scale_size(guide="none") + scale_alpha(guide="none") + opts(legend.position = "bottom", legend.direction = "horizontal") + scale_color_brewer(palette="Set2")
    if (geom %in% "point") p <- p + layer(data = snapshot, mapping = aes(x = x, y = z, size = 100 - y), geom = geom) + aes(alpha = 0.5, color = pitch_type)
    if (geom %in% c("hex", "density2d")) p <- p + layer(data = snapshot, mapping = aes(x = x, y = z), geom = geom) + scale_fill_continuous(breaks = c(0, 5, 10))
    if (geom %in% "tile") p <- p + geom_tile(data = snapshot, mapping = aes(x = x, y = z, fill = ..count..))
    print(p + geom_rect(data = boundaries, aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha = 0.2, color="grey20") #draw strikezones
          + layer)
  }
  #}
  #return(snapshots)
}

#' Calculate strikezone boundaries
#' 
#' Strikezone boundaries calculated according to Mike Fast's specifications
#' 
#' @param data PITCHf/x orginally entered into \code{animateFX}
#' @param facets variables used for faceting (passed along from \code{layer})
#' @references \url{http://www.baseballprospectus.com/article.php?articleid=14572}
#' @return Returns a list of boundaries for both right handed batters and left handed batters
#' 
getStrikezones <- function(data, facets) {
  if (is.character(typeof(data$b_height))) {
    h <- ldply(str_split(data$b_height, "-"), function(x) { as.numeric(x) })
    h[,2] <- h[,2]/12
    data$heights <- h[,1] + h[,2]
    bounds <- ddply(data, c("stand", facets), summarize, height=mean(heights))
    righty <- as.numeric(bounds$stand == "Batter Stands: R")
    lefty <- as.numeric(bounds$stand == "Batter Stands: L")
    bounds$top <- righty*(2.6 + bounds$height*0.136) + lefty*(2 + bounds$height*0.229)
    bounds$bottom <- righty*(0.92 + bounds$height*0.136) + lefty*(0.35 + bounds$height*0.229)
    bounds$left <- righty*-1.03 + lefty*-1.20
    bounds$right <- righty + lefty*0.81
    return(bounds)
  }
  if (is.numeric(typeof(data$b_height))) {
    warning("Since b_height is numeric, I will assume inches have been converted to feet.")
    bounds <- ddply(data, c("stand", facets), summarize, height=mean(b_height))
    righty <- as.numeric(bounds$stand == "Batter Stands: R")
    lefty <- as.numeric(bounds$stand == "Batter Stands: L")
    bounds$top <- righty*(2.6 + bounds$height*0.136) + lefty*(2 + bounds$height*0.229)
    bounds$bottom <- righty*(0.92 + bounds$height*0.136) + lefty*(0.35 + bounds$height*0.229)
    bounds$left <- righty*-1.03 + lefty*-1.20
    bounds$right <- righty + lefty*0.81
    return(bounds)
  }
}