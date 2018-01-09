#'Computes radially averaged spectral density of a matrix
#'
#' @param x a matrix
#' @param normalize Logical, whether the fft outputs should be normalized, default = TRUE
#' @param scaled Logical, whether the r_spectrum  should be scaled(0, 1), default = TRUE
#' @param plot Logical, whether the respectrum output should be ploted, default = TRUE
#' @export
#'
#' @examples
#' data(patches)
#' radial.spectrum(x = patches, Log = T)

radial.psd = function (x, scaled = TRUE, normalized = TRUE, plot = TRUE, ...)
{
      if (!is.matrix(x)) {
            stop("ERROR! x must be a matrix.")
      }
      if (is.raster(x)) {
            x = as.matrix(x)
      }

      N = nrow(x)
      M = ncol(x)

      if(N!=M) stop("ERROR! x must be a square matrix!")

      Nc = floor(N/2) + 1
      Mc = floor(M/2) + 1

      fft_x = fft(x)

      fftshift_x = fftshift(fft_x)

      fftshift_x[Nc, Mc] = 0


      ######Normalize
      if(normalized == TRUE){


            ffs_x = abs(fftshift_x)^2
            # ffs_x = ffs_x/(Nc*Mc)^4
            ffs_x = ffs_x/(Nc*Mc)^2
      } else {
            ffs_x = abs(fftshift_x)
      }

      xy_grid <- meshgrid(-(Nc-1):(ceiling(N/2) - 1), -(Mc-1):(ceiling(M/2) - 1))
      polar_coord = cartesianToPolar(xy.grid = xy_grid)
      dist = polar_coord$distance


      if(isTRUE(scaled)){

            # min_d = 1
            # md_index = dist >= min_d & dist <= Nc
            # sum_ffs = sum(ffs_x[md_index], na.rm = TRUE)


            sum_ffs = sum(ffs_x, na.rm = TRUE)
            # sum_ffs = 1

            #### Scale the power from 0 to 1.0
            ffs_x = ffs_x/sum_ffs
      }else {
            ffs_x = ffs_x

      }

      dist_ind = vector("list", length = (Nc))

      get.distIndex = function(d, dist.matrix, res = 1, ...){
            dist_ind = which(dist.matrix > d - res/2 & dist.matrix <= d + res/2)
            # dist_ind = which(round(dist.matrix) > (d-res) & round(dist.matrix) <= d)

            return(dist_ind)
      }

      get.rspectrum =  function(dist.index, fft.matrix) {
            r_spectrum = mean(fft.matrix[dist.index], na.rm = TRUE)
            return(r_spectrum)
      }

      dist_ind = mapply(FUN = get.distIndex,
                        1:length(dist_ind), MoreArgs = list(dist.matrix = dist, res = 1))

      # dist = round(polar_coord$distance)
      r_spectrum = sapply(X = dist_ind, FUN = get.rspectrum, fft.matrix = ffs_x)

      p = xy_grid$x[1, ]
      q = xy_grid$y[, 1]
      wavenumber = (sqrt(q^2 + p^2))
      wavenumber = wavenumber[Nc:1]

      rspectrum_data = data.frame(wavenumber = wavenumber, r_spectrum = r_spectrum)
      rspectrum_data = na.omit(rspectrum_data)
      if (plot == T) {
            plot((rspectrum_data$wavenumber), rspectrum_data$r_spectrum,
                 type = "l", lwd = 3, col = "dodgerblue",
                 ylab = "R-spectrum", xlab = "Wavenumber")
            lines((rspectrum_data$wavenumber), rspectrum_data$r_spectrum,
                  lwd = 2, col = "gray60")
      }
      return(rspectrum_data)
}
