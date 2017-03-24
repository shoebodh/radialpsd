#'Computes radially averaged spectral density of a matrix
#'
#' @param x a matrix
#' @param res  resolution of the image represented by the matrix)
#' @param Log Logical, whether the fft(x) values should be log-transformed, default  = TRUE
#' @param normalized Logical, whetther the fft (x) values should be normalized, default = FALSE
#' @param correct.mean  Logical, whether the input x should be subject to mean correction, default = FALSE
#' @param pad.values In case x is not a square, the values to apply to the new cells added in the periphery, default is NaN
#' @param plot Logical, whether the respectrum output should be ploted, default = TRUE

#' @export
#'
#' @examples
#' data(patches)
#' radial.spectrum(x = patches, Log = T)

radial.spectrum<- function(x, res = 1, Log = TRUE, normalized = FALSE,
            correct.mean = FALSE, pad.values = NaN, plot = TRUE,  ...){

if(!is.matrix(x)) {
  stop("ERROR! x must be a matrix.")
}

if(is.raster(x)){
  x = as.matrix(x)
}

 if(isTRUE(correct.mean)){
       meanx = mean(x, na.rm = TRUE)
      x = (x - meanx)
 }

  N = nrow(x)
  M = ncol(x)

   fft_x= fft(x)
   fftshift_x = fftshift(fft_x)

 if(isTRUE(Log)) {
     ffs_x = log(abs(fftshift_x))
 } else if(isTRUE(normalized)){
       ffs_x = 2*(abs(fftshift_x)/(N*M))
       ### As far as I undestand,
       ######## there is more than one way to normalized the FFT output.
       ######## This is one way (found in some discussion of R mailing list)
       ######## I have also seen the following
       ########  ffs_x = (abs(fftshift_x)/(N*M))^2

 }else if (isTRUE(Log) & (isTRUE(normalized))){
     ffs_log =  log(abs(fftshift_x))
       ffs_x = (abs(ffs_log)/(N*M))
 }else {
       ffs_x = abs(fftshift_x)
 }


   # imgfp = log(abs(imgf))

  dim_diff = abs(N - M)
  dim_max = max(N, M)

  ######### Pad NaN values to make the domain a squiare,if it's not
is.square = function(x) ifelse(identical(nrow(x), ncol(x)), TRUE, FALSE)

if(!is.square(ffs_x)) {
      ffs_x = make.square(ffs_x, pad.values = pad.values)
}

######

######

 half_length = floor(dim_max/2)
#
  midN= dim_max/2
  midM = dim_max/2

   xy_grid<- meshgrid(-midN:(midN-1),-midM:(midM-1))

 polar_coord = cartesianToPolar(xy.grid = xy_grid)

 dist = round(polar_coord$distance)

 dist_ind = vector("list", length = floor(midN))

 get.distIndex = function(d, dist.matrix){
      dist_ind = which(dist.matrix == d)
     return(dist_ind)
 }


dist_ind = mapply(FUN = get.distIndex,
    1:length(dist_ind), MoreArgs = list(dist.matrix = dist))

get.rspectrum = function(dist.index, fft.matrix){

      # psf = mean(fft.matrix[which(dist.matrix <= d)], na.rm = TRUE)
      r_spectrum = mean(fft.matrix[dist.index], na.rm = TRUE)


      return(r_spectrum)
}

r_spectrum = sapply(X = dist_ind, FUN = get.rspectrum, fft.matrix = ffs_x)

waveNumFun = function(half.length){
       p = q = -(half.length):(half.length-1)
      wavenumber = (sqrt(q^2 + p^2))
      wavenumber = wavenumber[half.length:1]
return(wavenumber)
}

 wavenumber = round(waveNumFun(half_length))

rspectrum_data = data.frame(wavenumber = wavenumber, r_spectrum = r_spectrum)
rspectrum_data = na.omit(rspectrum_data)

if(plot == T){
      plot((rspectrum_data$wavenumber), rspectrum_data$r_spectrum, type = "l",
          lwd = 3, log = "y", col = "dodgerblue", ylab = "R-spectrum",
          xlab = "Wavenumber")
      lines((rspectrum_data$wavenumber), rspectrum_data$r_spectrum, lwd = 2, col = "gray60")
}

return(rspectrum_data)

}
