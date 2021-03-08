test_that("interpolate_spectra() interpolates spectra data", {
  # generate spectra
  wavelength <- seq(380, 780, 1)
  ees <- wavelength
  ees <- 1
  y <- ees * wavelength
  df1 <- data.frame(wavelength, ees, y)

  # generate equivalent spectra
  wavelength <- seq(380, 780, 5)
  ees <- wavelength
  ees <- wavelength
  ees <- 1
  y <- ees * wavelength
  df5 <- data.frame(wavelength, ees, y)

  # testing function
  expect_equal(df5, interpolate_spectra(df1))
  expect_equal(df5, interpolate_spectra(df1, wavelength))
  expect_equal(df5, interpolate_spectra(df1,
                                             wavelength,
                                             str_wavelength = "wavelength"
                                             ))
  expect_equal(df5, interpolate_spectra(df1, method = "fmm"))

  # testing sprague wrapper
  expect_error(
    interpolate_spectra(df5, seq(380, 781, 1), method = "sprague"),
    label = "No extrapolation with Sprague method. Check wl_out."
    )
  expect_error(
    interpolate_spectra(df5, seq(380, 780, 2), method = "sprague"),
    label = "invalid f value - premature termination."
  )
})
