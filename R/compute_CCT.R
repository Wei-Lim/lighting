#' Title
#'
#' @param current_CCT
#' @param wavelength
#' @param u_prime
#' @param v_prime
#' @param cmf2
#' @param Km
#'
#' @NoRd
find_CCT <- function(current_CCT, wavelength, u_prime, v_prime, cmf2, Km) {
  spectrum_P <- planck_law(current_CCT, wavelength)

  XYZ_P <- compute_XYZ_CIE1931(spectrum_P, wavelength, cmf2, Km, 1)
  uv_P <- compute_UCS_CIE1976(XYZ_P$X_2, XYZ_P$Y_2, XYZ_P$Z_2)

  delta_uv <- sqrt((u_prime - uv_P$u_prime)^2 + 4 / 9
                   * (v_prime - uv_P$v_prime)^2)
  return(delta_uv)
}


#' Title
#'
#' @param spectrum
#' @param wavelength
#' @param cmf2
#' @param Km
#' @param R
#'
#' @NoRd
compute_CCT <- function(
  spectrum,
  wavelength,
  cmf2,
  Km = 683.002,
  R = 1
) {
  xyzXYZ <- compute_XYZ_CIE1931(spectrum, wavelength, cmf2, Km, R)
  uv <- compute_UCS_CIE1976(xyzXYZ$X_2, xyzXYZ$Y_2, xyzXYZ$Z_2)

  # Defining optimisation interval using McCamy Method
  n <- (xyzXYZ$x_2 - 0.3320) / (xyzXYZ$y_2 - 0.1858)
  current_CCT <- -449 * n^3 + 3525 * n^2 - 6823.3 * n + 5520.33
  CCTmin <- current_CCT - 500
  CCTmax <- current_CCT + 500

  result <- optimize(
    find_CCT,
    wavelength = wavelength,
    u_prime = uv$u_prime,
    v_prime = uv$v_prime,
    cmf2 = cmf2,
    Km = Km,
    interval = c(CCTmin, CCTmax)
  )
  CCT <- result$minimum

  # compute color rendering index
  if (CCT < 5000) {
    spectrum_ref <- planck_law(CCT, wavelength)
  } else if (CCT >= 5000 & CCT <= 7000) {
    xD <- -4.6070e9 / CCT^3 + 2.9678e6 / CCT^2 + 0.09911e3 / CCT + 0.244063
  } else if (CCT > 7000 & CCT <= 25000) {
    xD <- -2.0064e9 / CCT^3 + 1.9018e6 / CCT^2 + 0.24748e3 / CCT + 0.237040
  }

  df <- data.frame(CCT) %>%
    cbind(xyzXYZ) %>%
    cbind(uv)
  return(df)
}
