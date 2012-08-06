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
  p.types <- cbind(pitch_type=c("SI", "FF", "IN", "SL", "CU", "CH", "FT", "FC", "PO", "KN", "FS", "FA", NA, "FO"),
                   pitch_types=c("Sinker", "Fastball (four-seam)", "Intentional Walk", "Slider", "Curveball", "Changeup", 
                                 "Fastball (two-seam)", "Fastball (cutter)", "Pitchout", "Knuckleball", "Fastball (split-finger)",
                                 "Fastball", "Unknown", "Fastball ... (FO?)"))
  pitchFX <- merge(data, p.types, by = c("pitch_type"), sort = T)
  idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
  snapshot <- pitchFX[complete.cases(pitchFX[,idx]),] #get rid of records with missing parameters
  snapshot$p_throws <- paste("Pitcher Throws:", snapshot$p_throws)
  snapshot$stand <- paste("Batter Stands:", snapshot$stand)
  for (i in idx) 
    snapshot[,i] <- as.numeric(snapshot[,i]) #parameters as numeric
  idx2 <- c("des", "bats", "stand", "throws", "full_name", "pitch_types") #Keep these variables for faceting/coloring. Figure out way to automate/pass through plotting options???
  snapshot <- cbind(snapshot[,idx], snapshot[,idx2])
  t <- rep(0, dim(snapshot)[1]) #Initial time (at point of release)
  snapshot$y <- rep(50, dim(snapshot)[1]) #Initial distance from home plate
  snapshot$x <- with(snapshot, x0 + vx0*t + .5*ax*t^2) #Horizontal location (at time t)
  snapshot$z <- with(snapshot, z <- z0 + vz0*t + .5*az*t^2)
  p <- ggplot()
  while (any(as.numeric(snapshot$y) > 1.417)) { 
    snapshot$x <- with(snapshot, x0 + vx0*t + .5*ax*t^2) #Inside/Outside location (at time t)
    snapshot$y <- with(snapshot, pmax(1.417, y0 + vy0*t + .5*ay*t^2)) #Distance from home plate
    snapshot$z <- with(snapshot, z0 + vz0*t + .5*az*t^2) #Height from ground
    Sys.sleep(sleep)
    print(p + layer(data = snapshot, mapping = aes(x=x, y=z, color=pitch_types, size=10*-y, alpha=0.5), geom = "point") 
          + xlim(-3.5, 3.5) + xlab("Horizontal Pitch Location") 
          + ylim(0, 10) + ylab("Height from Ground") 
          + scale_size(guide="none") + scale_alpha(guide="none") 
          + scale_color_brewer(palette="Set2") + layer)
    q <- as.numeric(snapshot$y > 1.417)
    t <- t + q*interval #Only increment time for those that have yet to reach home plate
  }
  return(head(snapshot))
}