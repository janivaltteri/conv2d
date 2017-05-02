# conv2d
A very simple 2d convolution package for R. Useful for image smoothing and so on.

Contains two functions: csmf(matrix,kernel) calls a fortran subroutine which returns
a 2d convolution of the matrix with the kernel. csm(matrix,kernel) does the same thing
in R without external code.

TODO: functions for generating kernels for smoothing.

Under development.
