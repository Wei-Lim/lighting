#' Computes spectrum of a Planckian radiator
#'
#' @param temperature of black body radiator in kelvin
#' @param wl wavelength array in nm of exported spectrum
#'
#' @return spectrum of Planckian radiator
#' @export
#'
#' @examples
#' # create wavelength array
#' wl <- seq(380, 780, 1)
#'
#' planck_law(2700, wl)
#' planck_law(6500, wl)
planck_law <- function(temperature, wl) {

  #CODATA 2014 needs to be updated to CODATA 2018
  c0 <- 299792458 # speed of light in m/s
  h <- 6.626070040e-34 # Planck constant in Js
  kB <- 1.38064852e-23 # Boltzmann constant in J/K

  c1 <- 2 * pi * h * c0^2
  c2 <- h * c0 / kB

  spectrum <- c1 / ((wl * 1e-9)^5 * (exp(c2 / (wl * 1e-9 * temperature)) - 1))

  return(spectrum)
}
