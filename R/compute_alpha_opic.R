#' Compute alpha-opic values after Lucas et al.
#'
#' @param spectrum spectral data
#' @param wavelength array in nm
#' @param N_opsin spectral sensitivity functions
#'
#' @return
#'
#' @noRd
compute_alpha_opic <- function(
  spectrum,
  wavelength,
  N_opsin
) {
  df <- N_opsin %>%
    map_df(
      integrate_spectrum,
      spectrum = spectrum,
      wavelength = wavelength,
      constant = 72983.25
    ) %>%
    rename_with(~ c(
      "E_sc",
      "E_mc",
      "E_lc",
      "E_r",
      "E_z"
    ))
  return(df)
}
