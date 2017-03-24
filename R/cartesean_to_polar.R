#' Get polar coordinates froma a cartesean grid
#'
#' @param xy.grid A list containing the 'x' and 'y' matrices for the cartesian cooridantes
#' returns a list of 'theta' the  radian angle (range, -pi to pi) and corresponding 'distance' matrices
#' @export
#' @examples
#' xycoord = meshgrid(x = 1:5, y = 1:5)
#' cartesianToPolar(xycoord)
#'
 cartesianToPolar <- function(xy.grid){
       x = xy.grid$x
       y = xy.grid$y
    distance <- sqrt(x^2 + y^2)
    # theta <- atan(y/x)
    theta = atan2(y = y, x = x)
    # convert radians to degrees
     # theta<- round(theta*180/pi,2)

    return(list(theta = theta, distance = distance))
 }
