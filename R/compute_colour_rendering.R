#' Title
#'
#' @param u
#' @param v
#'
#' @NoRd
apply_von_Kries_color_shift <- function(u, v) {
  c = (4 - u - 10 * v) / v
  d = (1.708 * v + 0.404 - 1.481 * u) / v
  df <- data.frame(c, d)
  return(df)
}

#' Title
#'
#' @param Y
#' @param u
#' @param v
#' @param u_r
#' @param v_r
#'
#' @NoRd
compute_WUV_CIE1964 <- function(Y, u, v, u_r, v_r) {
  W <- 25 * Y^(1 / 3) - 17
  U <- 13 * W * (u - u_r)
  V <- 13 * W * (v - v_r)
  df <- data.frame(W, U, V)
  return(df)
}

#' Title
#'
#' @param spectrum
#' @param wavelength
#' @param cmf2
#' @param Km
#' @param R
#' @param S
#'
#' @NoRd
compute_colour_rendering <- function(
  spectrum,
  wavelength,
  cmf2,
  Km = 683.002,
  R = 1,
  S
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
  CCT <- 5455.877400278919
  CCT <- 5455

  # compute color rendering index after ASSIST recommends 2010
  ## step 2
  if (CCT < 5000) {
    spectrum_ref <- planck_law(CCT, wavelength)
  } else if (CCT >= 5000 & CCT <= 7000) {
    xD <- -4.6070e9 / CCT^3 + 2.9678e6 / CCT^2 + 0.09911e3 / CCT + 0.244063
  } else if (CCT > 7000 & CCT <= 25000) {
    xD <- -2.0064e9 / CCT^3 + 1.9018e6 / CCT^2 + 0.24748e3 / CCT + 0.237040
  }
  if (CCT >= 5000 & CCT <= 25000) {
    yD <- -3.0000 * xD^2 + 2.870 * xD - 0.275
    M1 <- (-1.3515 -  1.7703 * xD +  5.9114 * yD) /
      ( 0.0241 +  0.2562 * xD -  0.7341 * yD)
    M2 <- ( 0.0300 - 31.4424 * xD + 30.0717 * yD) /
      ( 0.0241 +  0.2562 * xD -  0.7341 * yD)
    spectrum_ref <- S$S_0 + M1 * S$S_1 + M2 * S$S_2
  }
  spectrum_ref <- (spectrum_ref ) /  max(spectrum_ref)
  wl_diff <- mean(diff(wavelength))

  K_k <- 100 / sum(spectrum     * R * cmf2$y_cmf_2) / wl_diff
  K_r <- 100 / sum(spectrum_ref * R * cmf2$y_cmf_2) / wl_diff

  # Remark: Using K = 1 leads to results in ASSIST recommend (ees p.13) (p.14)
  xyzXYZ_k <- compute_XYZ_CIE1931(spectrum    , wavelength, cmf2, K_k, R)
  xyzXYZ_r <- compute_XYZ_CIE1931(spectrum_ref, wavelength, cmf2, K_r, R)

  # Step 3
  # Step 3a
  xyzXYZ_ki <- TCS %>%
    select(-nm) %>%
    map_df(
      compute_XYZ_CIE1931,
      spectrum = spectrum,
      wavelength = wavelength,
      cmf2 = cmf2,
      K = K_k # K = 1 for values as in Assist recommends document
    )
  xyzXYZ_ri <- TCS %>%
    select(-nm) %>%
    map_df(
      compute_XYZ_CIE1931,
      spectrum = spectrum_ref,
      wavelength = wavelength,
      cmf2 = cmf2,
      K = K_r # K = 1 for values as in Assist recommends document
    )

  # Step 3b
  uv_k <- compute_UCS_CIE1976(xyzXYZ_k$X_2, xyzXYZ_k$Y_2, xyzXYZ_k$Z_2) %>%
    rename(u = u_prime, v = v_prime)
  uv_r <- compute_UCS_CIE1976(xyzXYZ_r$X_2, xyzXYZ_r$Y_2, xyzXYZ_r$Z_2) %>%
    rename(u = u_prime, v = v_prime)
  uv_ki <- compute_UCS_CIE1976(xyzXYZ_ki$X_2, xyzXYZ_ki$Y_2, xyzXYZ_ki$Z_2) %>%
    rename(u = u_prime, v = v_prime)
  uv_ri <- compute_UCS_CIE1976(xyzXYZ_ri$X_2, xyzXYZ_ri$Y_2, xyzXYZ_ri$Z_2) %>%
    rename(u = u_prime, v = v_prime)

  # transform to CIE-UCS-1960
  uv_k$v <- 2 / 3 * uv_k$v
  uv_r$v <- 2 / 3 * uv_r$v
  uv_ki$v <- 2 / 3 * uv_ki$v
  uv_ri$v <- 2 / 3 * uv_ri$v

  # step 4 von Kries color shift
  cd_k <- apply_von_Kries_color_shift(uv_k$u, uv_k$v)
  cd_r <- apply_von_Kries_color_shift(uv_r$u, uv_r$v)
  cd_ki <- apply_von_Kries_color_shift(uv_ki$u, uv_ki$v)

  uv_ki$u <- (10.872 + 0.404 * cd_r$c / cd_k$c * cd_ki$c - 4 * cd_r$d /
                cd_k$d * cd_ki$d) /
    (16.518 + 1.481 * cd_r$c / cd_k$c * cd_ki$c -     cd_r$d /
       cd_k$d * cd_ki$d)
  uv_ki$v <-   5.520 /
    (16.518 + 1.481 * cd_r$c / cd_k$c * cd_ki$c -     cd_r$d /
       cd_k$d * cd_ki$d)

  # step 5 Determine CIE 1964 W*U*V* values
  WUV_ki <- compute_WUV_CIE1964(xyzXYZ_ki$Y_2, uv_ki$u, uv_ki$v, uv_r$u, uv_r$v)
  WUV_ri <- compute_WUV_CIE1964(xyzXYZ_ri$Y_2, uv_ri$u, uv_ri$v, uv_r$u, uv_r$v)

  dE <- sqrt((WUV_ri$U - WUV_ki$U)^2 + (WUV_ri$V - WUV_ki$V)^2 +
               (WUV_ri$W - WUV_ki$W)^2)

  R_i <- 100 - 4.6 * dE

  R_a <- sum(R_i[1:8]) / 8

  df <- data.frame(R_a, t(R_i)) %>%
    rename_with(~ c("R_a", "R_1", "R_2", "R_3", "R_4", "R_5", "R_6", "R_7",
                    "R_8", "R_9", "R_10", "R_11", "R_12", "R_13", "R_14"))
  return(df)
}
