#' Compare x to 1
#'
#' @param spectra
#' @param str_wavelength
#' @param R
#' @param cmf2
#' @param K
#'
#' @NoRd
compute_XYZ_CIE1931 <- function(spectra, wavelength, cmf2, K, R) {
  wl_diff <- mean(diff(wavelength))
  X_2 <- K * sum(spectra * R * cmf2$x_cmf_2) * wl_diff
  Y_2 <- K * sum(spectra * R * cmf2$y_cmf_2) * wl_diff
  Z_2 <- K * sum(spectra * R * cmf2$z_cmf_2) * wl_diff

  x_2 <- X / (X + Y + Z)
  y_2 <- Y / (X + Y + Z)
  z_2 <- Z / (X + Y + Z)

  df <- data.frame(x_2, y_2, z_2, X_2, Y_2, Z_2)
  return(df)
}




