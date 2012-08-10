#' Animate Pitch F/X
#' 
#' Pitch trajectories animated on a two-dimensional plot.
#' 
#' Details to go here.
#' 
#' @param data pitch F/X data to be visualized.
#' @param layer list of ggplot2 modifications to the plot
#' @param interval time (in seconds - real time) between plotting the pitch locations
#' @param sleep passed along to Sys.sleep() to flush current plot
#' @return ggplot2 object
#' @export
#' @examples
#' #Simple scraping example
#' #data <- scrapePitchFX(start = "2011-10-01", end = "2011-10-02")
#' #pitches <- data$pitch
#' #atbats <- data$atbat
#' #pitchFX <- join(pitches, atbats, by = c("num", "url"))
#' #Subset data by pitch type
#' #pitchFX2 <- pitchFX[pitchFX$zone < 4 & pitchFX$pitch_type == c("FF", "CU", "SL"), ]
#' #animateFX(pitchFX2)
#' #animateFX(pitchFX2, layer = facet_grid(stand~p_throws))#How do I add titles to the facets (stand vs. p_throws)

animateFX <- function(data, layer=NULL, interval = 0.01, sleep = 0.000001){ 
  #Add descriptions to pitch_types
  if (!"pitch_type" %in% names(data)) warning("Make sure you have the appropriate 'pitch_type' column. If you don't have 'pitch_type', consider using ggFX()")
  types <- cbind(pitch_type=c("SI", "FF", "IN", "SL", "CU", "CH", "FT", "FC", "PO", "KN", "FS", "FA", NA, "FO"),
                   pitch_types=c("Sinker", "Fastball (four-seam)", "Intentional Walk", "Slider", "Curveball", "Changeup", 
                                 "Fastball (two-seam)", "Fastball (cutter)", "Pitchout", "Knuckleball", "Fastball (split-finger)",
                                 "Fastball", "Unknown", "Fastball ... (FO?)"))
  pitchFX <- merge(data, types, by = "pitch_type")
  if ("p_throws" %in% names(pitchFX)) pitchFX$p_throws <- paste("Pitcher Throws:", pitchFX$p_throws)
  if ("stand" %in% names(pitchFX)) pitchFX$stand <- paste("Batter Stands:", pitchFX$stand)
  idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
  if (!all(idx %in% names(pitchFX))) warning("You must have the following variables in your dataset to animate pitch locations: 'x0', 'y0', 'z0', 'vx0', 'vy0', 'vz0', 'ax', 'ay', 'az'")
  complete <- pitchFX[complete.cases(pitchFX[,idx]),] #get rid of records with any missing parameters
  #Keep parameters and other reasonable faceting/coloring variables
  idx2 <- c("des", "type", "event", "zone", "stand", "batter_name", "p_throws", "pitcher_name", "pitch_types")
  all.info <- complete[, names(pitchFX) %in% c(idx, idx2)]
  snapshots <- getSnapshots(all.info)
  browser()
  for (i in dim(snapshots)[3]) {
    snapshot <- data.frame(snapshots[,,i])
    names(snapshot) <- c("x", "y", "z", idx2)
    Sys.sleep(sleep)
    print(ggplot()
          + layer(data = snapshot, mapping = aes(x=x, y=z, color=pitch_types, size=10*-y, alpha=0.5), geom = "point") 
          + xlim(-3.5, 3.5) + xlab("Horizontal Pitch Location") 
          + ylim(0, 10) + ylab("Height from Ground") 
          + scale_size(guide="none") + scale_alpha(guide="none") 
          + scale_color_brewer(palette="Set2") + layer)
  }
  #return(head(snapshot))
}

#' Produce time sequenced 
#' 
#' Pitch trajectories animated on a two-dimensional plot.
#' 
#' Details to go here.
#' 
#' @param data The nine PITCHf/x parameters used to determine the location of a pitch at a given time.
#' @param interval the amount of time between 'snapshots'
#' @return Return a three dimensinal array. The third dimension corresponds to different 'snapshots' of locations.

getSnapshots <- function(data, interval = 0.01) {
  library(abind)
  idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
  parameters <- data[, c(idx)]
  #arranged <- data[,c(parameters, other)] #rearrange columns for computation later on
  for (i in idx)
    parameters[,i] <- as.numeric(parameters[,i]) #Coerce the pitchFX parameters to numerics
  times <- with(parameters[,c("y0", "vy0", "ay")], (-1*vy0-sqrt(vy0^2 - 2*ay*(y0 - 1.417)))/ay) #Figure out how long it takes each pitch to reach home plate
  nplots <- ceiling(max(times/interval)) #Number of 'snapshots' required
  t <- matrix(seq(from = 0, to = max(times), by = interval), ncol = 1)
  npitches <- dim(parameters)[1]
  snapshots <- array(rep(c(parameters, recursive = TRUE), nplots), dim = c(dim(parameters), nplots))
  velocties <- array(apply(snapshots[,4:6,], c(1,2), function(x) { x*t }), dim = c(npitches, 3, nplots))
  as <- array(apply(snapshots[,7:9,], c(1,2), function(x) { 0.5*x*t^2 }), dim = c(npitches, 3, nplots))
  locations <- snapshots[,1:3,] + velocties + as
  other <- data[, names(data)[!(names(data) %in% idx)]]
  other.array <- array(rep(c(other, recursive = TRUE), nplots), dim = c(dim(other), nplots))
  everything <- abind(locations, other.array, along = 2)
}

#test <- getSnapshots(pitches)
#dim(test) #note we dropped six variables here on purpose (second dimension)


#parameters.x <- array(rep(c(parameters[c("x0", "vx0", "ax")], recursive = TRUE), nplots), dim = c(npitches, 3, nplots))
#parameters.y <- array(rep(c(parameters[c("y0", "vy0", "ay")], recursive = TRUE), nplots), dim = c(npitches, 3, nplots))
#parameters.z <- array(rep(c(parameters[c("z0", "vz0", "az")], recursive = TRUE), nplots), dim = c(npitches, 3, nplots))

#   while (any(q)) {
#     data$y <- with(data, pmax(1.417, y0 + vy0*t + .5*ay*t^2)) #Distance from home plate
#     q <- as.numeric(data$y > 1.417)
#     t <- t + q*interval #Only increment time for those that have yet to reach home plate
#     nplots <- nplots + 1
#   }
#   ?sweep
#   data <- as.array(data, dim = c(dim(data), 1))
#   while (any(as.numeric(data$y) > 1.417)) { 
#     data$x <- with(data, x0 + vx0*t + .5*ax*t^2) #Inside/Outside location (at time t)
#     data$y <- with(data, pmax(1.417, y0 + vy0*t + .5*ay*t^2)) #Distance from home plate
#     data$z <- with(data, z0 + vz0*t + .5*az*t^2) #Height from ground
#     q <- as.numeric(snapshot$y > 1.417)
#     t <- t + q*interval #Only increment time for those that have yet to reach home plate
#   }
  
#   matrices <- list(x0 = matrix(as.numeric(snapshots[,1,]), ncol = nplots),
#                 y0 = matrix(as.numeric(snapshots[,2,]), ncol = nplots),
#                 z0 = matrix(as.numeric(snapshots[,3,]), ncol = nplots),
#                 vx0 = matrix(as.numeric(snapshots[,1,]), ncol = nplots),
#                 vy0 = matrix(as.numeric(snapshots[,2,]), ncol = nplots),
#                 vz0 = matrix(as.numeric(snapshots[,3,]), ncol = nplots),
#                 ax = matrix(as.numeric(snapshots[,1,]), ncol = nplots),
#                 ay = matrix(as.numeric(snapshots[,2,]), ncol = nplots),
#                 az = matrix(as.numeric(snapshots[,3,]), ncol = nplots))




