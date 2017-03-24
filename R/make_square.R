#' Convert a rectangular matrix object to a square on by addign rows/columns
#'
#' @param x input matrix
#' @param pad.values values to be applied to the new rows/columns. devault = NaN
#'
#' @export
#' @examples
#'
#' x = matrix(0, 6, 3)
#' make.square(x)

make.square = function(x, pad.values = NaN) {

 stopifnot(class(x) =="matrix")

 N = nrow(x)
 M = ncol(x)

 dim_diff = abs(N - M)

 if(N > M) {
    if(dim_diff %% 2 == 0) {
    pad_matrix = matrix(pad.values, nrow = N, ncol = dim_diff)
    pad_x= cbind(pad_matrix, x, pad_matrix)
    } else {

      pad_matrix1 = matrix(pad.values, nrow = N, ncol = floor(dim_diff/2))
     pad_matrix2 = matrix(pad.values, nrow = N, ncol = floor(dim_diff/2)+1)

      pad_x = cbind(pad_matrix1, x, pad_matrix2)
     }

  } else if(N < M) {
    if(dim_diff %% 2 == 0) {
      pad_matrix = matrix(pad.values, nrow = dim_diff/2, ncol = M)
     pad_x = rbind(pad_matrix,x, pad_matrix)
    } else {
      pad_matrix1 = matrix(pad.values, nrow = floor(dim_diff/2), ncol = M)
      pad_matrix2 = matrix(pad.values, nrow = floor(dim_diff/2 + 1), ncol = M)
      pad_x = rbind(pad_matrix1, x, pad_matrix2)

    }

  }
return(pad_x)
}