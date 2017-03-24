#' Moves zero frequency elements (of fft() output) to the center of the matrix
#' this makes easier to visualize the fft output.
#'
#' @param x input matrix
#' @export
#' @examples
#' a = matrix(1:25, 5, 5)
#' fftshift(a)
fftshift <- function(x) {

 if(!is.matrix(x)){
      stop("ERROR!x must be a matrix...")
 }

    N <- nrow(x)
    M <- ncol(x)

    flipud <- function(x) {
       half_N <- ceiling(N/2)
        return(rbind(x[((half_N+1):N), (1:M)], x[(1:half_N), (1:M)]))
    }

   fliplr <- function(x) {
        half_M <- ceiling(M/2)
        return(cbind(x[1:N, ((half_M+1):M)], x[1:N, 1:half_M]))
    }

   udflipped_x = flipud(x)

   return(fliplr(udflipped_x))

}

