compute_lighting <- function(
  spectra,
  str_wavelength = NULL,
  units = "photopic"
  ) {

  # defining first column of dataframe as str_wavelength
  if (is.null(str_wavelength)) {
    str_wavelength <- colnames(spectra)[1]
  }
  wl <- spectra[[str_wavelength]]
  spectra <- select(spectra, -all_of(str_wavelength))
  lightsource <- colnames(spectra)

  # loading and interpolating spectral sensitvity functions to given wavelength
  ssf <- sensitivity_functions(wl)

  # creating empty matrix
  mat <- matrix(nrow = 0, ncol = 7)

  # photopic values
  Km <- 683.002 # in lm/W
  E_v <- sapply(spectra,
                integrate_spectrum,
                wavelength = wl,
                sensitivity = ssf$cvrl.org$V_pho,
                constant = Km
                )
  mat <- rbind(mat, E_v)

  # next relative values

  df <- data.frame(t(mat))

  return(df)
}
