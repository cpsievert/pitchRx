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
  nplots <- ceiling(max(times/interval)) #Number of 'snapshots' required
  t <- seq(from = 0, to = max(times), by = interval)
  snapshots <- array(rep(c(parameters, recursive = TRUE), nplots), dim = c(dim(parameters), nplots))
  velocities <- aperm(apply(snapshots[,4:6,], c(1,2), function(x) { x*t }), perm = c(2,3,1))
  as <- aperm(apply(snapshots[,7:9,], c(1,2), function(x) { 0.5*x*t^2 }), perm = c(2,3,1))
  locations <- snapshots[,1:3,] + velocities + as
}