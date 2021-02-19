test_that("interpolate_spectra() interpolates spectra data", {
  # generate spectra
  wavelength <- seq(380,780,1)
  ees <- wavelength
  ees <- 1
  y <- ees * wavelength
  df <- data.frame(wavelength, ees, y)

  # generate equivalent spectra
  wavelength <- seq(380,780,5)
  ees <- wavelength
  ees <- wavelength
  ees <- 1
  y <- ees * wavelength
  df_equal <- data.frame(wavelength, ees, y)

  # testing function
  expect_equal(df_equal, interpolate_spectra(df))
  expect_equal(df_equal, interpolate_spectra(df, wavelength))
  expect_equal(df_equal, interpolate_spectra(df, wavelength, "wavelength"))
  expect_equal(df_equal, interpolate_spectra(df, method = "fmm"))
})
