#' Computes nonvisual quantities defined by CIE, DIN and IWBI
#'
#' @param spectrum spectral power distribution
#' @param wavelength array in nm
#' @param V_pho photopic luminosity function
#' @param K_m luminous efficacy
#' @param s_alpha spectral sensitivty functions for alpha-values
#'
#' @return nonvisual quantities
#'
#' @noRd
compute_nonvisual_standards <- function(
  spectrum,
  wavelength,
  V_pho,
  K_m,
  s_alpha
  ) {

  E_v <- integrate_spectrum(spectrum, wavelength, V_pho, K_m)

  E_alpha <- map_df(
    s_alpha,
    integrate_spectrum,
    spectrum = spectrum,
    wavelength = wavelength,
    constant = 1
  )

  # alpha-opic daylight (D65) efficacy ratios
  K_D65_alpha <- c(0.8731e-3, 1.4558e-3, 1.6289e-3, 1.4497e-3, 1.3262e-3)

  gamma_D65_alpha <- E_alpha / E_v / K_D65_alpha
  a_mel <- gamma_D65_alpha[5] / 1.104
  R_mel_ratio <- gamma_D65_alpha[5] / 0.9101

  E_D65_alpha <- E_v * gamma_D65_alpha
  E_D65_v_mel <- E_D65_alpha[5]
  EML <- E_D65_alpha[5] / 0.9101

  df <- data.frame(
    E_v,
    E_D65_alpha,
    gamma_D65_alpha,
    E_D65_v_mel,
    a_mel,
    EML,
    R_mel_ratio
    ) %>%
    rename_with(~ c(
      "E_v",
      "E^D65_sc,v",
      "E^D65_mc,v",
      "E^D65_lc,v",
      "E^D65_rh,v",
      "E^D65_mel,v",
      "gamma^D65_sc,v",
      "gamma^D65_mc,v",
      "gamma^D65_lc,v",
      "gamma^D65_rh,v",
      "gamma^D65_mel,v",
      "E_v,mel,D65",
      "a_mel,v",
      "EML",
      "R_mel,ratio"
      ))
  return(df)
}

