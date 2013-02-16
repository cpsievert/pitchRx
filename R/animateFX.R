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
#' @param data data frame with appropriately named PITCHf/x variables
#' @param point.size Size of points
#' @param point.alpha ggplot2 alpha parameter
#' @param limitz limits for horizontal and vertical axes. 
#' @param color variable used to control coloring scheme.
#' @param flag indicate whether or not batter has decided to swing.
#' @param interval time (in seconds) between plotting the pitch locations.
#' @param sleep passed along to Sys.sleep() to flush current plot.
#' @param layer list of ggplot2 layer modifications.
#' @param parent is the function being called from a higher-level function? (experimental)
#' @param ... extra options passed onto geom commands
#' @return Returns a series of ggplot2 objects.
#' @export
#' @examples
#' data(pitches)
#' animateFX(pitches)
#' animateFX(pitches, layer = facet_grid(pitcher_name~stand))
#' 

animateFX <- function(data, color = "pitch_types", point.size=3, point.alpha=1/3, limitz=c(-3.5, 3.5, 0, 7), flag=FALSE, interval = 0.01, sleep = 0.000000000001, layer = list(), parent=FALSE, ...){ 
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
  layers <- NULL
  if (parent) { #ugly workaround for shiny implementation
    for (i in layer)
      layers <- list(layers, eval(i)) 
  } else {
    layers <- layer
  }
  facets <- getFacets(layers)
  idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
  if (!all(idx %in% names(data))) warning("You must have the following variables in your dataset to animate pitch locations: 'x0', 'y0', 'z0', 'vx0', 'vy0', 'vz0', 'ax', 'ay', 'az'")
  complete <- data[complete.cases(data[,idx]),] #get rid of records with any missing parameter values
  if (color!="") { #Special aesthetic handling if coloring exists. Less prevalent cases should be plotted last.
    reordered <- ddply(complete, facets, function(x) { #Does this do anything if facets is NULL?
      x[, color] <- reorder(x[, color], x[, color], length)
      x[rev(order(x[, color])), ]
    })
    aes_mapping <- aes_string(x = "x", y="z", colour = color)
  } else {
    reordered <- complete
    aes_mapping <- aes_string(x = "x", y="z")
  }
  parameters <- reordered[, names(reordered) %in% idx]
  snapshots <- getSnapshots(parameters)
  other <- reordered[, !(names(reordered) %in% idx)] #Keep 'other' variables for faceting/coloring
  if ("b_height" %in% names(other)) {
    boundaries <- getStrikezones(other, facets, strikeFX = FALSE) #Strikezone boundaries
    other <- join(other, boundaries, by="stand", type="inner")
  } else {
    zones <- NULL
    warning("Strikezones depend on the stance (and height) of the batter. Make sure these variables are being entered as 'stand' and 'b_height', respectively. Also, 'b_height' must be numeric; otherwise, strikezones will not appear.")
  }
  xrange <- xlim(limitz[1:2])
  yrange <- ylim(limitz[3:4])
  ctr <- 1 #Used to check whether or not batter has decided to swing
  N <- dim(snapshots)[2] #Number of plots in animation
  swing <- NULL
  for (i in 1:N) {
    if (flag & ctr > (2/5)*N) swing <- annotate("text", label = "SWING!", x = 0, y = 6, size = 2, colour = "red")
    FX <- data.frame(snapshots[,i,], other)
    names(FX) <- c("x", "y", "z", names(other))
    Sys.sleep(sleep)
    p <- ggplot(data=FX) + xrange + yrange + xlab("Horizontal Pitch Location") + ylab("Height from Ground") + scale_size(guide="none") + scale_alpha(guide="none") + theme(legend.position = c(0.25,0.05), legend.direction = "horizontal")
    p <- p + geom_rect(mapping=aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha=0, fill="pink", colour="black") #draw strikezones
    p <- p + geom_point(mapping=aes_mapping, size=point.size, alpha=point.alpha, ...)
    print(p+swing+layers)
    ctr <- ctr + 1
  }
}