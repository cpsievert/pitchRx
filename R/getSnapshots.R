#' Produce time sequenced pitch locations from PITCHf/x parameters
#' 
#' Pitch trajectories animated on a two-dimensional plot.
#' 
#' Details to go here.
#' 
#' @param data The nine PITCHf/x parameters used to determine the location of a pitch at a given time.
#' @param interval the amount of time between 'snapshots'
#' @return Return a three dimensinal array. The third dimension corresponds to different 'snapshots' of locations.

getSnapshots <- function(data, interval = 0.01) {
  idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
  parameters <- data[, idx]
  for (i in idx)
    parameters[,i] <- as.numeric(parameters[,i]) #Coerce the pitchFX parameters to numerics
  times <- with(parameters[,c("y0", "vy0", "ay")], (-1*vy0-sqrt(vy0^2 - 2*ay*(y0 - 1.417)))/ay) #Figure out how long it takes each pitch to reach home plate
  nplots <- ceiling(max(times/interval)) + 1 #Number of 'snapshots' required for EVERY pitch to reach home plate
  npitches <- dim(data)[1] #Number of pitches (within each plot)
  t.matrix <- matrix(rep(0:(nplots - 1)*interval, times = npitches), byrow = TRUE, nrow = npitches) 
  t <- pmin(t.matrix, times) #Restrict time if ball already crossed home plate
  snapshots <- array(rep(c(parameters, recursive = TRUE), nplots), dim = c(dim(parameters), nplots)) #Rep the PITCHf/x parameters for the total amount of plots needed
  x <- snapshots[,1,] + snapshots[,4,] * t + 0.5 * snapshots[,7,] * t^2 #Height from ground at time t
  y <- snapshots[,2,] + snapshots[,5,] * t + 0.5 * snapshots[,8,] * t^2 #Distance from batter at time t
  z <- snapshots[,3,] + snapshots[,6,] * t + 0.5 * snapshots[,9,] * t^2 #Horizontal location at time t
  locations <- array(c(x, y, z), dim = c(npitches, nplots, 3))
}