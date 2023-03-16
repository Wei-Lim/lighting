# LIGHTING VALUES ----

# 1 BASE FUNCTIONS ----
# 1.1 Planck law ----

#' @title Creates spectrum of a Planckian radiator
#'
#' @description plack_law() computes the spectral intensities of
#' a Planckian radiator with a defined temperature of the black body.
#'
#' @param wl wavelength vector of Planckian radiator in nm
#' @param temperature A single temperature value of black body radiator in kelvin
#'
#' @return spectral intensities of Planckian radiator
#'
#' @examples
#' # Create wavelength vector
#' wl <- seq(380, 780, 1)
#'
#' planck_law(wl, 2700)
#' planck_law(wl, 6500)
#'
#' @export
planck_law <- function(wl, temperature) {

	# CODATA 2018 from NIST
	c0 <- 299792458      # speed of light in m/s
	h  <- 6.62607015e-34 # Planck constant in Js
	kB <- 1.380649e-23   # Boltzmann constant in J/K

	c1 <- 2 * pi * h * c0^2
	c2 <- h * c0 / kB

	intensity <- c1 / ((wl * 1e-9)^5 * (exp(c2 / (wl * 1e-9 * temperature)) - 1))

	return(intensity)
}
