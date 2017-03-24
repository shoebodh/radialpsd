#########
ifftshift <- function(x, dim = -1) {

    N <- nrow(x)
    M <- ncol(x)

     swap_up_down <- function(x) {
        N_half <- floor(N/2)
        return(rbind(x[((N_half+1):N), (1:M)], x[(1:N_half), (1:M)]))
    }

    swap_left_right <- function(x) {
        M_half <- floor(M/2)
        return(cbind(x[1:N, ((M_half+1):M)], x[1:N, 1:M_half]))
    }

    if (dim == -1) {
        x <- swap_left_right(x)
        return(swap_up_down(x))
    }
    else if (dim == 1) {
        return(swap_up_down(x))
    }
    else if (dim == 2) {
        return(swap_left_right(x))
    }
    else {
        stop("Invalid dimension parameter")
    }
}
