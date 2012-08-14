#' Animate PITCHf/x
#' 
#' More flexible animation than \code{animateFX}. This function requires the user to specify a geometry 
#' (or, a statistical transfromation) to be made to the data before plotting. It allows allows for 
#' coloring by 
#' 
#' @param data PITCHf/x data to be visualized.
#' @param layer list of ggplot2 modifications to the plot
#' @param interval time (in seconds - real time) between plotting the pitch locations
#' @param sleep passed along to Sys.sleep() to flush current plot
#' @return ggplot2 object
#' @export

ggFX <- function(data, layer = list(), interval = 0.01, sleep = 0.000001){ 
  #Add descriptions to pitch_types
  if ("pitch_type" %in% names(data)) {
    types <- cbind(pitch_type=c("SI", "FF", "IN", "SL", "CU", "CH", "FT", "FC", "PO", "KN", "FS", "FA", NA, "FO"),
                   pitch_types=c("Sinker", "Fastball (four-seam)", "Intentional Walk", "Slider", "Curveball", "Changeup", 
                                 "Fastball (two-seam)", "Fastball (cutter)", "Pitchout", "Knuckleball", "Fastball (split-finger)",
                                 "Fastball", "Unknown", "Fastball (FO?)"))
    pitchFX <- merge(data, types, by = "pitch_type")
  }
  if ("p_throws" %in% names(pitchFX)) pitchFX$p_throws <- paste("Pitcher Throws:", pitchFX$p_throws)
  if ("stand" %in% names(pitchFX)) pitchFX$stand <- paste("Batter Stands:", pitchFX$stand)
  idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
  if (!all(idx %in% names(pitchFX))) warning("You must have the following variables in your dataset to animate pitch locations: 'x0', 'y0', 'z0', 'vx0', 'vy0', 'vz0', 'ax', 'ay', 'az'")
  complete <- pitchFX[complete.cases(pitchFX[,idx]),] #get rid of records with missing parameters
  parameters <- complete[, names(pitchFX) %in% idx]
  snapshots <- getSnapshots(parameters)
  #Keep parameters and other reasonable faceting/coloring variables
  idx2 <- c("type", "event", "zone", "stand", "batter_name", "p_throws", "pitcher_name", "pitch_types")
  other <- complete[, names(pitchFX) %in% idx2]
  for (i in 1:dim(snapshots)[3]) {
    snapshot <- data.frame(snapshots[,,i], other)
    names(snapshot) <- c("x", "y", "z", names(other))
    Sys.sleep(sleep)
    print(ggplot(data = snapshot, aes(x=x, y=z)) + xlim(-3.5, 3.5) + xlab("Horizontal Pitch Location")
          + ylim(0, 10) + ylab("Height from Ground") + scale_size(guide="none") + scale_alpha(guide="none") #suppress legends for size and alpha
          + layer)
  }
}