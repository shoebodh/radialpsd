#'Computes directional, radially averaged spectral density of a matrix
#'Used the base::fft() function to get fourier coefficients of the input matrix

#' @param x a matrix
#' @param alpha  the angle at which the rspectrum is calculated (0 - 90)
#' @param offset offset to (alpha + - offset is be used to calculate teh rspectrum)
#' @param res  resolution of the image represented by the matrix)
#' @param Log Logical, whether the fft(x) values should be log-transformed, default  = TRUE
#' @param normalized Logical, whetther the fft (x) values should be normalized, default = FALSE
#' @param correct.mean  Logical, whether the input x should be subject to mean correction, default = FALSE
#' @param pad.values In case x is not a square, the values to apply to the new cells added in the periphery, default is NaN
#' @param plot Logical, whether the respectrum output should be ploted, default = TRUE

#' @export
#'
#' @examples
#' data(patches2)
#'directional.spectrum(x = patches2, alpha = 90, offset = 10, Log = T)

directional.spectrum<- function(x, alpha, offset = 10, res = 1, Log = TRUE, normalized = FALSE,
                        correct.mean = FALSE, pad.values = NaN, plot = TRUE, ...){

      if(!is.matrix(x)) {
            stop("ERROR! x must be a matrix.")
      }

      if(is.raster(x)){
            x = as.matrix(x)
      }

      if(isTRUE(correct.mean)){
            meanx = mean(x, na.rm = TRUE)
            x = x - meanx
      }

      N = nrow(x)
      M = ncol(x)

      fft_x= fft(x)
      fftshift_x = fftshift(fft_x)

      if(isTRUE(Log)) {
            ffs_x = log(abs(fftshift_x))
      } else if(isTRUE(normalized)){
            ffs_x = 2*(abs(fftshift_x)/(N*M))

      }else if (isTRUE(Log) & (isTRUE(normalized))){
            ffs_log =  log(abs(fftshift_x))
            ffs_x = (abs(ffs_log)/(N*M))
      }else {
            ffs_x = abs(fftshift_x)
      }


      dim_diff = abs(N - M)
      dim_max = max(N, M)

      ######### Pad NaN values to make the domain a squiare,if it's not
      is.square = function(x) ifelse(identical(nrow(x), ncol(x)), TRUE, FALSE)

      if(!is.square(ffs_x)) {
            ffs_x = make.square(ffs_x, pad.values = pad.values)
      }

        ######

      half_length = floor(dim_max/2)
      #
      midN= dim_max/2
      midM = dim_max/2

       xy_grid<- meshgrid(-midN:(midN-1),-midM:(midM-1))

      polar_coord = cartesianToPolar(xy.grid = xy_grid)
      dist_mat = round(polar_coord$distance)
      theta_rad =polar_coord$theta  ## atan2 gives range of -pi to pi
      theta_deg = round(theta_rad*180/pi, 2)

############
     get.pixIndex<- function(angle.mat, alpha, offset) {

            if(offset > alpha){
                  alpha2_upper =  alpha + offset
                  alpha2_lower = alpha - offset

                  alpha3_lower = alpha2_upper-180
                  alpha3_upper = 180 + alpha2_lower

                  pos_ind = which(angle.mat >= alpha2_lower & angle.mat <= alpha2_upper)
                  neg_ind = which(angle.mat <= alpha3_lower | angle.mat >= alpha3_upper)

            } else if ((alpha + offset)> 90) {
                  alpha2_upper =  alpha + offset
                  alpha2_lower = alpha - offset
                  alpha3_upper = -alpha2_lower
                  alpha3_lower  = -alpha2_upper

                  pos_ind = which(angle.mat >= alpha2_lower & angle.mat <= alpha2_upper)
                  neg_ind = which(angle.mat >= alpha3_lower & angle.mat <= alpha3_upper)

            } else {
                  alpha2_upper =  alpha + offset
                  alpha2_lower = alpha - offset

                  alpha3_upper = alpha2_upper - 180
                  alpha3_lower  = alpha2_lower - 180

                  pos_ind = which(angle.mat >= alpha2_lower & angle.mat <= alpha2_upper)
                  neg_ind = which(angle.mat >= alpha3_lower & angle.mat <= alpha3_upper)

            }

            pix_ind_d = c(pos_ind, neg_ind)

            return(pix_ind_d)
      }
############
      # px = get.pixIndex(theta_deg, 90, 20)
      # aat = theta_deg
      # aat[px] = 1000
      # plot(raster(aat))

      cell_index = get.pixIndex(angle.mat = theta_deg,
                                alpha = alpha, offset = offset)

      cell_dists = dist_mat[cell_index]
      fft_vals = ffs_x[cell_index]

      ff_data = data.frame(d = cell_dists, fft_value = fft_vals)

      dist_levels = factor(ff_data$d)

      ff_agg = aggregate(x = ff_data, by = list(dist_levels), FUN = "mean", na.rm = T)

      dd = ff_agg$d
      r_spectrum = ff_agg$fft_value
      wavenumber = round(sqrt(2*dd^2))

      rspectrum_data = data.frame(wavenumber = wavenumber,
                                  r_spectrum = r_spectrum)

      if(plot == T){
            plot((rspectrum_data$wavenumber), rspectrum_data$r_spectrum, type = "l",
                 lwd = 3, log = "y", col = "dodgerblue", ylab = "R-spectrum",
                 xlab = "Wavenumber")
            lines((rspectrum_data$wavenumber), rspectrum_data$r_spectrum, lwd = 2, col = "gray60")
      }

      return(rspectrum_data)

}
