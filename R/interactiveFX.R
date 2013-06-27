#' Use rgl to visualize PITCHf/x
#' 
#' Three-dimensional plot of pitch trajectories.
#'
#' @param data data.frame with appropriately named PITCHf/x variables
#' @param spheres Use \link{rgl::spheres3d} or \link{rgl::plot3d}?
#' @param color variable used to control coloring scheme.
#' @param interval the amount of time between 'snapshots'
#' @param alpha color transparency
#' @param show.legend print coloring legend in R console?
#' @param ... other param passed onto rgl::spheres3d or rgl::plot3d
#' @return rgl object is returned.
#' @export
#' @examples
#' 
#' data(pitches)
#' Rivera <- subset(pitches, pitcher_name=="Mariano Rivera")
#' \dontrun{interactiveFX(Rivera, interval=.05)}
#' 

interactiveFX <- function(data, spheres=TRUE, color="pitch_types", interval=0.01, alpha=1, show.legend=TRUE, ...){
  if ("pitch_type" %in% names(data)) { #Add descriptions as pitch_types
    data$pitch_type <- factor(data$pitch_type)
    types <- data.frame(pitch_type=c("SI", "FF", "IN", "SL", "CU", "CH", "FT", "FC", "PO", "KN", "FS", "FA", NA, "FO"),
                        pitch_types=c("Sinker", "Fastball (four-seam)", "Intentional Walk", "Slider", "Curveball", "Changeup", 
                                      "Fastball (two-seam)", "Fastball (cutter)", "Pitchout", "Knuckleball", "Fastball (split-finger)",
                                      "Fastball", "Unknown", "Fastball ... (FO?)"))
    data <- join(data, types, by = "pitch_type", type="inner")
  } 
  idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
  if (!all(idx %in% names(data))) warning("You must have the following variables in your dataset to animate pitch locations: 'x0', 'y0', 'z0', 'vx0', 'vy0', 'vz0', 'ax', 'ay', 'az'")
  complete <- data[complete.cases(data[,idx]),] #get rid of records with any missing parameter values
  snaps <- getSnapshots(complete, interval)
  nplots <- length(snaps[1,,1]) #Number of 'snapshots' required for EVERY pitch to reach home plate
  if (isTRUE(!color %in% names(data))) { #convert types to colors!
    warning(paste(color, "is the variable that defines coloring but it isn't in the dataset!"))
    full.pal <- rgb(0, 0, 0, alpha)
  } else {
    types <- as.character(data[,color])
    ncolors <- length(unique(types))
    if (ncolors > 3) pal <- terrain.colors(ncolors)
    if (ncolors == 3) pal <- c(rgb(0, 0, 1), rgb(0, 1, 0), rgb(1, 0, 0))
    if (ncolors == 2) pal <- c(rgb(0, 0, 1), rgb(1, 0, 0))
    if (ncolors == 1) pal <- rgb(0, 0, 0)
    if (show.legend) {
      legend <- data.frame(unique(types), pal)
      names(legend) <- c(color, "colors")
      cat("Here is the coloring scheme for your plot. Use http://www.colorhexa.com/ to translate color codes.", "/n")
      print(legend)
    }
    full.pal <- factor(types)
    levels(full.pal) <- pal
  }
  open3d()
  if (spheres){
    spheres3d(x=as.vector(snaps[,,1]), y=as.vector(snaps[,,2]), z=as.vector(snaps[,,3]),
           col=as.character(full.pal), radius=.12, alpha=alpha, ...)
    axes3d(c('x', 'y', 'z')) 
    title3d(xlab='Horizontal Axis', ylab='Distance from Home Plate', zlab='Height From Ground')
  } else {
    plot3d(x=as.vector(snaps[,,1]), y=as.vector(snaps[,,2]), z=as.vector(snaps[,,3]),
           xlab="Horizontal Axis", ylab="Distance from Home Plate", zlab="Height From Ground",
           col=as.character(full.pal), alpha=alpha, ...)
  }
}
