#' Integrates numerical a dataframe of spectra
#'
#' \loadmathjax
#' Typical numerical integration of spectrum to compute lighting values
#' \mjeqn{LV}{ascii} using the trapezoidal method. Formula:
#' \mjdeqn{LV = K \cdot \int E_e(\lambda) s(\lambda) d\lambda
#' }{ascii}
#'
#'
#' @param spectrum \mjeqn{E_e(\lambda)}{ascii} is the spectrum to be integrated.
#' @param wavelength \mjeqn{\lambda}{ascii} defines the integration wavelength
#' range.
#' @param sensitivity \mjeqn{s(\lambda)}{ascii} defines the sensitivity
#' function, which is multiplied with the spectrum \mjeqn{E_e(\lambda)}{ascii}
#' before integration. Default: 1.
#' @param constant \mjeqn{K}{ascii} is a constant. Mostly used for
#' standardisation. Default: 1.
#'
#' @return an integrated value after the formula in the description.
#' @export
#'
#' @examples
#' library(dplyr)
#' # calculating photopic quantitiy illuminance
#' K_m <- 683.002 # in lm/W constant for self-luminous object
#' ssf <- cvrl.org %>%
#'   filter(380 <= nm & nm <= 780)
#' V_pho <- ssf$V_pho
#' wl <- ees <- seq(380, 780, 1)
#' ees[] <- 1
#' integrate_spectrum(ees, wl, V_pho, K_m)
#'
#' @importFrom pracma trapz
#' @importFrom mathjaxr preview_rd
integrate_spectrum <- function(
  spectrum,
  wavelength,
  sensitivity = 1,
  constant = 1
  ) {
  x <- wavelength
  y <- spectrum * sensitivity
  n <- length(y)
  # correction term at the boundary see pracma::trapz
  h  <- x[2] - x[1]
  ca <- (y[2]-y[1]) / h
  cb <- (y[n]-y[n-1]) / h

  value <- constant * (trapz(x, y) - h^2/12 * (cb - ca))

  return(value)
}
