# radialpsd: An R package to calculate radially averaged periodograms (or r-spectrum) of matrices.

This package is based on the methods described by Mugglestone and Rainshaw (1998). 
The r-spectra are calculated by averaging the fourier coefficients produced by base R's
fft() after transforming the cartesian coordinates into polar. Both omni-directional
(i.e., over a circular region defined the distance) and directinal (averaged over a slice 
confined by an angle +- offset) can be calculated 

This package is in part also inspired from the following Matalb code.
https://www.mathworks.com/matlabcentral/fileexchange/23636-radially-averaged-power-spectrum-of-2d-real-valued-matrix

 References: Mugglestone, M. A., and E. Rainshaw. 1998.Detection of geological lineations on aerial photographs using 
 two-dimensional spectral analysis. Computers and Geosciences, 24(8):771-784
