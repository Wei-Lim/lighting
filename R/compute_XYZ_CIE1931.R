#' Compute CIE-XYZ-1931 color space values
#'
#' @param spectrum spectral power distribution of light source per wavelength
#' @param wavelength array in nm corresponding to spectrum
#' @param cmf2 spectral sensitivity of CIE 2° colour matching functions
#' @param K constant for standardization
#' @param R spectral reflection factor
#'
#' @noRd
compute_XYZ_CIE1931 <- function(spectrum, wavelength, cmf2, K, R) {
  wl_diff <- mean(diff(wavelength))
  X_2 <- K * sum(spectrum * R * cmf2$x_cmf_2) * wl_diff
  Y_2 <- K * sum(spectrum * R * cmf2$y_cmf_2) * wl_diff
  Z_2 <- K * sum(spectrum * R * cmf2$z_cmf_2) * wl_diff

  x_2 <- X_2 / (X_2 + Y_2 + Z_2)
  y_2 <- Y_2 / (X_2 + Y_2 + Z_2)
  z_2 <- Z_2 / (X_2 + Y_2 + Z_2)

  df <- data.frame(x_2, y_2, z_2, X_2, Y_2, Z_2)
  return(df)
}
