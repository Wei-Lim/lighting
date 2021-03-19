#' Computes lighting values from given spectra dataframe
#'
#' @param spectra dataframe of spectra with one wavelength column in nm
#' @param str_wavelength name of wavelength column. Default is NULL: The first
#' column is the wavelength column
#'
#' @return dataframe of lighting values used in lighting
#' @export
#'
#' @examples
#' wavelength <- seq(380, 780, 1)
#' P2700 <- planck_law(2700, wavelength)
#' P6500 <- planck_law(6500, wavelength)
#'
#' spectra <- data.frame(wavelength, P2700, P6500)
#' spectra$EES <- 1
#'
#' compute_lighting(spectra)
#' compute_lighting(spectra, "wavelength")
#'
#' @importFrom dplyr all_of
compute_lighting <- function(
  spectra,
  str_wavelength = NULL
  ) {


  # defining local variables
  x_cmf_2 <- y_cmf_2 <- z_cmf_2 <- nm <- T_lens <- NULL

  # defining first column of dataframe as str_wavelength
  if (is.null(str_wavelength)) {
    str_wavelength <- colnames(spectra)[1]
  }
  wavelength <- spectra[[str_wavelength]]
  spectra <- select(spectra, -all_of(str_wavelength))
  lightsource <- colnames(spectra)

  # defining constants
  K_m <- 683.002 # in lm/W
  # loading and interpolating spectral sensitvity functions to given wavelength
  ssf <- sensitivity_functions(wavelength)
  V_pho <- ssf$cvrl.org$V_pho
  cmf2 <- select(ssf$cvrl.org, c(x_cmf_2, y_cmf_2, z_cmf_2))
  S <- ssf$cie2018_015
  TCS <- ssf$cie1995_13.3
  s_alpha <- select(ssf$cie2018_S_026, -nm)
  N_opsin <- select(ssf$lucas2014, -c(nm, V_pho, T_lens))


  # creating data frame lighting values
  lv <- data.frame()[1:length(lightsource),] %>%
    `rownames<-`(lightsource)

  # illuminance computation
  df <- spectra %>%
    map_df(
      compute_nonvisual_standards,
      wavelength = wavelength,
      V_pho = V_pho,
      K_m = K_m,
      s_alpha = s_alpha
    )

  lv <- cbind(lv, df)

  # compute colour rendering indexes including CCT, xyzXYZ, uv
  df <- spectra %>%
    map_df(
      compute_colour_rendering,
      wavelength = wavelength,
      cmf2 = cmf2,
      Km = K_m,
      R = 1,
      S = S,
      TCS
    )
  lv <- cbind(lv, df)

  # alpha-opic illuminances after Lucas et al. 2014
  df <- spectra %>%
    map_df(
      compute_alpha_opic,
      wavelength = wavelength,
      N_opsin = N_opsin
    )
  lv <- cbind(lv, df)

  # Circadian Stimulus after Rea et al. 2018
  df <- spectra %>%
    map_df(
      compute_circadian_stimulus,
      wavelength = wavelength,
      ssf = ssf$rea2018
    )
  lv <- cbind(lv, df)

  return(lv)
}
