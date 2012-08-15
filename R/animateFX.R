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
#' @param color.layer variable used to control different colors
#' @param layer list of ggplot2 modifications to the plot.
#' @param strikezones logical parameter determining the presence of strikezones.
#' @param interval time (in seconds - real time) between plotting the pitch locations.
#' @param sleep passed along to Sys.sleep() to flush current plot.
#' @return Returns a ggplot2 object.
#' @export
#' @examples
#' #Simple scraping example
#' 

animateFX <- function(data, geom = "point", color.layer = aes(color = pitch_types), layer = NULL, strikezones = TRUE, interval = 0.01, sleep = 0.000000000001){ 
  #Add descriptions to pitch_types
  if (!"pitch_type" %in% names(data)) warning("Make sure you have the appropriate 'pitch_type' column. If you don't have 'pitch_type', consider using ggFX()")
  types <- cbind(pitch_type=c("SI", "FF", "IN", "SL", "CU", "CH", "FT", "FC", "PO", "KN", "FS", "FA", NA, "FO"),
                   pitch_types=c("Sinker", "Fastball (four-seam)", "Intentional Walk", "Slider", "Curveball", "Changeup", 
                                 "Fastball (two-seam)", "Fastball (cutter)", "Pitchout", "Knuckleball", "Fastball (split-finger)",
                                 "Fastball", "Unknown", "Fastball ... (FO?)"))
  pitchFX <- merge(data, types, by = "pitch_type")
  idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
  if (!all(idx %in% names(pitchFX))) warning("You must have the following variables in your dataset to animate pitch locations: 'x0', 'y0', 'z0', 'vx0', 'vy0', 'vz0', 'ax', 'ay', 'az'")
  complete <- pitchFX[complete.cases(pitchFX[,idx]),] #get rid of records with any missing parameters
  boundaries <- getStrikezones(complete) #Strikezone boundaries
  parameters <- complete[, names(pitchFX) %in% idx]
  snapshots <- getSnapshots(parameters)
  #Keep 'other' variables for faceting/coloring 
  other <- complete[, !(names(pitchFX) %in% idx)]
  #Add suffixes for context
  if ("p_throws" %in% names(other)) other$p_throws <- paste("Pitcher Throws:", other$p_throws)
  if ("stand" %in% names(other)) other$stand <- paste("Batter Stands:", other$stand)
  for (i in 1:dim(snapshots)[2]) {
    snapshot <- data.frame(snapshots[,i,], other)
    names(snapshot) <- c("x", "y", "z", names(other))
    Sys.sleep(sleep)
    p <- ggplot() + layer(data = snapshot, mapping = aes(x = x, y = z, size = -1000 * y, alpha = 0.5), geom = geom) + xlim(-3.5, 3.5) + xlab("Horizontal Pitch Location") + ylim(0, 7) + ylab("Height from Ground") + scale_size(guide="none") + scale_alpha(guide="none") + scale_color_brewer(palette="Set2")
    if (geom != "hex") p <- p + color.layer
    if (strikezones == TRUE) {
      print(p + geom_rect(data = boundaries$RHB, aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha = 0.2, color="grey20") #draw right-handed strikezone
            + geom_rect(data = boundaries$LHB, aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha = 0.2, color="blue") #draw right-handed strikezone
            #+ facet_grid(. ~ stand) for some reason facet_wrap works, but this doesn't????
            + layer)
    } else {
      print(p + layer)
    }
  }
  #return(snapshots)
}

#' Calculate strikezone boundaries
#' 
#' Strikezone boundaries calculated according to Mike Fast's specifications
#' 
#' @param data PITCHf/x orginally entered into \code{animateFX}
#' @references \url{http://www.baseballprospectus.com/article.php?articleid=14572}
#' @return Returns a list of boundaries for both right handed batters and left handed batters
#' 

getStrikezones <- function(data) {
  if (!"b_height" %in% names(data)) warning("This data set doesn't have 'b_height'. Thus, strikezones will not be included in the animations.")
  if (!"stand" %in% names(data)) {
    warning("This data set doesn't have 'stand'. Thus, strikezones will not be included in the animations.")
  } else {
    if (is.character(typeof(data$b_height))) {
      h <- ldply(str_split(data$b_height, "-"), function(x) { as.numeric(x) })
      h[,2] <- h[,2]/12
      data$heights <- h[,1] + h[,2]
      averages <- with(data, tapply(heights, INDEX = stand, mean))
      avg.R <- as.numeric(averages["R"])
      avg.L <- as.numeric(averages["L"])
      #strikezone boundaries according to Mike Fast (using mean height among different stances)
      righties <- data.frame(top = 2.6 + avg.R * 0.136, bottom = 0.92 + avg.R * 0.136, left = -1.03, right = 1, stand = "Batter Stands: R")
      lefties <- data.frame(top = 2 + avg.L * 0.229, bottom = 0.35 + avg.L * 0.229, left = -1.20, right = 0.81, stand = "Batter Stands: L")
      return(list(RHB = righties, LHB = lefties))
    }
    if (is.numeric(typeof(data$b_height))) {
      warning("Since b_height is numeric, I will assume inches have been converted to feet.")
      averages <- with(data, tapply(b_height, INDEX = stand, mean))
      avg.R <- as.numeric(averages["R"])
      avg.L <- as.numeric(averages["L"])
      #strikezone boundaries according to Mike Fast (using mean height among different stances)
      righties <- data.frame(top = 2.6 + avg.R * 0.136, bottom = 0.92 + avg.R * 0.136, left = -1.03, right = 1, stand = "Batter Stands: R")
      lefties <- data.frame(top = 2 + avg.L * 0.229, bottom = 0.35 + avg.L * 0.229, left = -1.20, right = 0.81, stand = "Batter Stands: L")
      return(list(RHB = righties, LHB = lefties))
    }
  }
}
#getStrikezones(Rivera)