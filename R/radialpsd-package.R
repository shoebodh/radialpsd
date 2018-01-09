#'radialpsd: radially averaged periodograms (or r-spectrum) of 2D matrices.
#'
#'This package is based on the methods described by Mugglestone and Rainshaw (1998).
#'The r-spectra are calculated by averaging the fourier coefficients produced by base R's
#'fft() after transforming the cartesian coordinates in to polar. Both omni-directional
#'(i.e. over a circular region defined the distance) and
#' directinal (averaged over a slice defined by an angle +- offset) can be calculated
#'
#'
#'
#' References: Mugglestone, M. A., and E. Rainshaw. 1998.Detection of
#' geological lineations on aerial photographs using two-dimensional
#' spectral analysis. Computers and Geosciences, 24(8):771-784
#'
#' @docType package
#' @name radialpsd

NULL
