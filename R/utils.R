#' Compare x to 1
#'
#' @param spectra
#' @param str_wavelength
#' @param R
#' @param cmf2
#' @param K
#'
#' @NoRd
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

compute_UCS_CIE1976 <- function(X_2, Y_2, Z_2) {
  u_prime = 4 * X_2 / (X_2 + 15 * Y_2 + 3 * Z_2)
  v_prime = 9 * Y_2 / (X_2 + 15 * Y_2 + 3 * Z_2)
  #w_prime = 1 - u_prime - v_prime
  df <- data.frame(u_prime, v_prime)
  return(df)
}

find_CCT <- function(current_CCT, wavelength, u_prime, v_prime, cmf2, Km) {
  spectrum_P <- planck_law(current_CCT, wavelength)

  XYZ_P <- compute_XYZ_CIE1931(spectrum_P, wavelength, cmf2, Km, 1)
  uv_P <- compute_UCS_CIE1976(XYZ_P$X_2, XYZ_P$Y_2, XYZ_P$Z_2)

  delta_uv <- sqrt((u_prime - uv_P$u_prime)^2 + 4 / 9
                   * (v_prime - uv_P$v_prime)^2)
  return(delta_uv)
}
