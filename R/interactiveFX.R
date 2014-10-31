#' Use rgl to visualize PITCHf/x
#' 
#' Three-dimensional plot of pitch trajectories.
#'
#' @param data data.frame with appropriately named PITCHf/x variables
#' @param spheres Use rgl::spheres3d or rgl::plot3d?
#' @param color variable used to control coloring scheme.
#' @param avg.by variable used as an index for averaging over PITCHf/x parameters
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
#' \dontrun{
#'  interactiveFX(Rivera, interval=.05)
#'  interactiveFX(Rivera, avg.by="pitch_types")
#'  }
#' 

interactiveFX <- function(data, spheres=TRUE, color="pitch_types", avg.by, interval=0.01, alpha=1, show.legend=TRUE, ...){
  if (!requireNamespace('rgl')) warning("This function requireNamespaces the rgl package. Please try to install.packages('rgl') before using.")
  if ("pitch_type" %in% names(data)) { #Add descriptions as pitch_types
    data$pitch_type <- factor(data$pitch_type)
    pitch.type <- c("SI", "FF", "IN", "SL", "CU", "CH", "FT", "FC", "PO", "KN", "FS", "FA", NA, "FO")
    pitch.types <- c("Sinker", "Fastball (four-seam)", "Intentional Walk", "Slider", "Curveball", "Changeup", 
                     "Fastball (two-seam)", "Fastball (cutter)", "Pitchout", "Knuckleball", "Fastball (split-finger)",
                     "Fastball", "Unknown", "Forkball")
    types <- data.frame(pitch_type=factor(pitch.type, levels=sort(pitch.type)),
                        pitch_types=factor(pitch.types, levels=sort(pitch.types)))
    data <- join(data, types, by = "pitch_type", type="inner")
  } 
  idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
  if (!all(idx %in% names(data))) warning("You must have the following variables in your dataset to animate pitch locations: 'x0', 'y0', 'z0', 'vx0', 'vy0', 'vz0', 'ax', 'ay', 'az'")
  complete <- data[complete.cases(data[,idx]),] #get rid of records with any missing parameter values
  for (i in idx) complete[,i] <- as.numeric(complete[,i])
  if (!missing(avg.by)) complete <- ddply(complete, avg.by, numcolwise(mean))
  snaps <- getSnapshots(complete, interval)
  nplots <- length(snaps[1,,1]) #Number of 'snapshots' requireNamespaced for EVERY pitch to reach home plate
  if (isTRUE(!color %in% names(data))) { #convert types to colors!
    warning(paste(color, "is the variable that defines coloring but it isn't in the dataset!"))
    full.pal <- rgb(0, 0, 0, alpha)
  } else {
    types <- as.character(complete[,color])
    ncolors <- length(unique(types))
    if (ncolors > 3) pal <- terrain.colors(ncolors)
    if (ncolors == 3) pal <- c(rgb(1, 0, 0), rgb(0, 1, 0), rgb(0, 0, 1))
    if (ncolors == 2) pal <- c(rgb(1, 0, 0), rgb(0, 0, 1))
    if (ncolors == 1) pal <- rgb(0, 0, 0)
    if (show.legend) {
      legend <- data.frame(unique(types), pal)
      names(legend) <- c(color, "colors")
      cat("Here is the coloring scheme for your plot. Use http://www.colorhexa.com/ to translate color codes.", "\n")
      print(legend)
    }
    full.pal <- factor(types)
    levels(full.pal) <- pal
  }
  rgl::open3d()
  if (spheres){
    rgl::spheres3d(x=as.vector(snaps[,,1]), y=as.vector(snaps[,,2]), z=as.vector(snaps[,,3]),
           col=as.character(full.pal), radius=.12, alpha=alpha, ...)
    rgl::axes3d(c('x', 'y', 'z')) 
    rgl::title3d(xlab='Horizontal Axis', ylab='Distance from Home Plate', zlab='Height From Ground')
  } else {
    rgl::plot3d(x=as.vector(snaps[,,1]), y=as.vector(snaps[,,2]), z=as.vector(snaps[,,3]),
           xlab="Horizontal Axis", ylab="Distance from Home Plate", zlab="Height From Ground",
           col=as.character(full.pal), alpha=alpha, ...)
  }
}
