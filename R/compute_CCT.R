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

  df <- data.frame(CCT) %>%
    cbind(xyzXYZ) %>%
    cbind(uv)
  return(df)
}
