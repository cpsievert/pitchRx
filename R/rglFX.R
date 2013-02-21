#' Use rgl to visualize PITCHf/x
#' 
#' Three-dimensional plot of pitch trajectories
#' 
#' Details go here.
#'
#' @param data data frame with appropriately named PITCHf/x variables
#' @param interval the amount of time between 'snapshots'
#' @param print.legend print coloring legend in R console?
#' @return rgl plot3d() object is returned.
#' @export
#' 

rglFX <- function(data, color="pitch_types", interval=0.01, print.legend=TRUE){
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
  if (!color %in% names(data) || is.null(color)) { #convert types to colors!
    warning(paste(color, "is the variable that defines coloring but it isn't in the dataset!"))
    full.pal <- "#000000"
  } else {
    #types <- rep(as.character(data[,color]), times=nplots)
    types <- as.character(data[,color])
    ncolors <- length(unique(types))
    if (ncolors > 3) pal <- terrain.colors(ncolors, alpha = 1)
    if (ncolors == 3) pal <- c("#ff0000", "#00ff00", "#0000ff") 
    if (ncolors == 2) pal <- c("#ff0000", "#0000ff") 
    if (ncolors == 1) pal <- "#000000"
    if (print.legend) {
      legend <- data.frame(unique(types), pal)
      names(legend) <- c(color, "colors")
      print("Here is the coloring scheme for your plot. Use http://www.colorhexa.com/ to translate color codes.")
      print(legend)
    }
    full.pal <- factor(types)
    levels(full.pal) <- pal
  }
  plot3d(x=as.vector(snaps[,,1]), y=as.vector(snaps[,,2]), z=as.vector(snaps[,,3]),
         xlab="Horizontal Axis", ylab="Distance from Home Plate", zlab="Height From Ground",
         col=as.character(full.pal))
}