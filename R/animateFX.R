#' Animate PITCHf/x
#' 
#' Pitch trajectories animated on a two-dimensional plot. 
#' 
#' \code{animateFX} plots a series of "snapshots" that represent pitch trajectories 
#' from the point of release until all of them reach home plate. 
#' The graphic takes on the viewpoint of the umpire; that is, the pitches are getting closer 
#' to the viewer with time. This is relected with the increase in size of the "balls" as the 
#' animation progresses.
#'
#' @param data data frame with appropriately named PITCHf/x variables
#' @param color variable used to control coloring scheme.
#' @param avg.by variable used as an index for averaging over PITCHf/x parameters
#' @param point.alpha ggplot2 alpha parameter
#' @param limitz limits for horizontal and vertical axes. 
#' @param flag indicate whether or not batter has decided to swing.
#' @param interval time (in seconds) between plotting the pitch locations.
#' @param layer list of ggplot2 layer modifications.
#' @param parent is the function being called from a higher-level function? (experimental)
#' @param ... extra options passed onto geom commands
#' @return Returns a series of ggplot2 objects.
#' @export
#' @examples
#' data(pitches)
#' #generate animation and prompt default web browser to view the sequence of plots
#' \dontrun{ 
#' animation::saveHTML({ animateFX(pitches, layer = facet_grid(pitcher_name~stand)) })
#' animation::saveHTML({ animateFX(pitches, avg.by="pitch_types", 
#'                          layer = facet_grid(pitcher_name~stand)) 
#'                    }) 
#' }
#' 

animateFX <- function(data, color = "pitch_types", avg.by, point.alpha=1/3, limitz=c(-3.5, 3.5, 0, 7), flag=FALSE, interval = 0.01, layer = list(), parent=FALSE, ...){ 
  top=bottom=right=left=NULL #ugly hack to comply with R CMD check
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
  if (!"b_height" %in% names(data)) {
    warning("pitchRx assumes the height of each batter is recorded as 'b_height'. Since there is no such column, we will assume each batter has a height of 6'2''")
    data$b_height <- "6-2"
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
  for (i in idx) data[,i] <- as.numeric(data[,i]) #Coerce the pitchFX parameters to numerics
  complete <- data[complete.cases(data[,idx]),] #get rid of records with any missing parameter values
  color.exists <- isTRUE(color %in% c(names(data), "pitch_types"))
  if (!missing(avg.by)) { #Average PITCHf/x parameters for every unique combination of facet and avg.by variable(s)
    index <- c(facets, avg.by) 
    reordered <- ddply(complete, index, numcolwise(mean))
    if (color.exists) {
      if (avg.by != color) {
        warning("You can't average by one variable and color by another!")
        aes_mapping <- aes_string(x = "x", y="z", size="scale_y")
      } else {
        aes_mapping <- aes_string(x = "x", y="z", size="scale_y", colour = color)
      }
    }
  } else {
    if (color.exists) { #Special aesthetic handling if coloring exists (Less prevalent cases are plotted last.)
      reordered <- ddply(complete, facets, function(x) { #Does this do anything if facets is NULL?
        x[, color] <- reorder(x[, color], x[, color], length)
        x[rev(order(x[, color])), ]
      })
      #reorder does funny stuff to factor levels...restore them to the original
      if (is.factor(reordered[,color])) reordered[color] <- factor(reordered[,color], levels=levels(data[,color])) 
      aes_mapping <- aes_string(x = "x", y="z", size="scale_y", colour = color)
    } else {
      reordered <- complete
      aes_mapping <- aes_string(x = "x", y="z", size="scale_y")
    }
  }
  parameters <- reordered[, names(reordered) %in% idx]
  snapshots <- getSnapshots(parameters, interval)
  other <- reordered[, !(names(reordered) %in% idx)] #Keep 'other' variables for faceting/coloring
  boundaries <- getStrikezones(data, facets, strikeFX = FALSE) #Strikezone boundaries
  joinby <- unique(c("stand", facets))
  other <- join(other, boundaries, by=joinby, type="inner")
  xrange <- xlim(limitz[1:2])
  yrange <- ylim(limitz[3:4])
  ctr <- 1 #Used to check whether or not batter has decided to swing
  N <- dim(snapshots)[2] #Number of plots in animation
  release <- max(as.numeric(parameters$y0))
  max.dist <- release - 1.417 #maximum distance a baseball can be from the pitcher (1.417 is start of home plate)
  swing <- NULL
  #browser()
  for (i in 1:(N-1)) {
    if (flag & ctr > (2/5)*N) swing <- annotate("text", label = "SWING!", x = 0, y = 6, size = 2, colour = "red")
    frame <- data.frame(snapshots[,i,], other)
    names(frame) <- c("x", "y", "z", names(other))
    Sys.sleep(.000000001)
    frame$scale_y <- abs(frame$y - release)/max.dist #rescale to increase size as y decreases
    p <- ggplot(data=frame) + xrange + yrange + xlab("Horizontal Pitch Location") + ylab("Height from Ground") + scale_alpha(guide="none") +
      scale_size(guide="none", range=(ctr/N+1)*c(2,3)) + theme(legend.position = c(0.25,0.05), legend.direction = "horizontal")
    p <- p + geom_rect(mapping=aes(ymax = top, ymin = bottom, xmax = right, xmin = left), alpha=0, fill="pink", colour="black") #draw strikezones
    p <- p + geom_point(mapping=aes_mapping, alpha=point.alpha, ...)
    print(p+swing+layers)
    ctr <- ctr + 1
  }
}
