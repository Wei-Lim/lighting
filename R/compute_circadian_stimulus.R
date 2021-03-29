#' Computes the Circadian Stimulus from Rea et al.
#'
#' @param spectrum spectral power distribution
#' @param wavelength array in nm
#' @param ssf spectral sensitivity functions
#'
#' @return
#'
#' @noRd
compute_circadian_stimulus <- function (
  spectrum,
  wavelength,
  ssf
) {

  V_sco <- ssf$V_sco
  V_lbd_mp <- ssf$V_pho_mac
  S_lbd_mp <- ssf$S_sc_mac
  Mc <- ssf$Mc_lens

  rodSat <- 6.5
  k <- 0.2616
  ab_y <- 0.700
  arod <- 3.3

  M <- integrate_spectrum(spectrum, wavelength, Mc, 1)
  SV <- integrate_spectrum(spectrum, wavelength, S_lbd_mp, 1) -
    integrate_spectrum(spectrum, wavelength, V_lbd_mp, k)
  VS <- integrate_spectrum(spectrum, wavelength, V_sco, 1)

  if (SV > 0) {
    CL_A <- 1548 * (M + ab_y * SV - arod * (1 - exp(-VS / rodSat)))
  } else if (SV <= 0) {
    CL_A <- 1548 * M
  }

  CS <- 0.7 - 0.7 / (1 + (CL_A / 355.7)^1.1026)

  df <- data.frame(CS, CL_A, SV)
  return(df)
}
