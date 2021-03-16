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

  # Correlated colour temperatur, xyzXYZ-CIE-1931 and u'v'-CIE-UCS-1976
  R <- 1
  cmf2 <- select(ssf$cvrl.org, c(x_cmf_2, y_cmf_2, z_cmf_2))
  df_cct <- sapply(spd,
                    compute_CCT,
                    wavelength = wl_out,
                    cmf2 = cmf2,
                    K = Km,
                    R = R
                    ) %>%
    unlist() %>%
    matrix(nrow = 9)
  rownames(df_cct) <- c("CCT",
                    "x_2", "y_2", "z_2", "X_2", "Y_2", "Z_2",
                    "u_prime", "v_prime")
  mat <- rbind(mat, df_cct)


  df <- data.frame(t(mat))

  return(df)
}
