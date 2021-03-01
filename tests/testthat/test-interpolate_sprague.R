test_that("Sprague interpolation works", {
  # generate y-function, which should be sprague interpolated
  x <- seq(-3, 3, 0.1)
  y <- x^3
  # sprage interpolation with quadruple sample rate f
  y_out <- interpolate_sprague(y, 4)
  # comaring single values of y_out
  expect_equal(y_out[13], -19.683)
  expect_equal(round(y_out[74], 6), -1.622234)
  expect_equal(round(y_out[211], 5), 11.39063)
})
