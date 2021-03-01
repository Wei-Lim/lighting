#' Integrates a dataframe of spectra
#'
#' Typical integration to compute lighting values
#' \deqn{E = K \cdot \int E_\text{e}(\lambda) \cdot s(\lambda) d\lambda}.
#'
#'
#' @param sensitivity
#' @param constant
#' @param spectra
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom pracma trapz
integrate_spectrum <- function(
  spectrum,
  wavelength,
  sensitivity = 1,
  constant = 1
  ) {

  value <- constant * (trapz(wavelength, spectrum * sensitivity))

  return(value)
}
