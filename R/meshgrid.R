#' Get a mesh grid defined by the ranges of input parameters x and y
#' @param x a vector of numerica values
#' @param y a vector of numeric values
#'
#' @export
#'
#' @examples
#' x = 1:5
#' y = 1:5
#' meshgrid(x, y)
#'
#'
meshgrid = function (x, y) {
    return(list(x = outer(X = y*0, Y = x, FUN = "+"),
      y = outer(X = y, Y = x*0, FUN =  "+")))
  }
