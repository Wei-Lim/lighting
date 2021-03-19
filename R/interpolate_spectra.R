#' Interpolates a dataframe of spectra with a wavelength column.
#'
#' Description
#'
#' In Details - Not tested for wavelength interval < 1 nm.
#' Linear interpolating: extrapolation results into 0 values.
#'
#' @param spectra is a dataframe of spectra in wide table format including
#'   a wavelength column in nm.
#' @param wl_out defines the output wavelength range and interval in nm for
#'   interpolation.
#'   Default: \code{wl_out = seq(380, 780, 5)}
#' @param str_wavelength Define name of wavelength column in \code{spectra}
#'   dataframe. Default: \code{str_wavelength = NULL}. If entry is \code{NULL},
#'   then the name of the first column from dataframe \code{spectra} will
#'   chosen as \code{str_wavelength}.
#' @param method specifies the interpolation method of the functions
#'   \link[stats]{approx} and \link[stats]{spline}. Default:
#'   \code{"linear"}. Spline interpolation: \code{"fmm"}, \code{"periodic"},
#'   \code{"natural"}, \code{"monoH.FC"} and \code{"hyman"}. Sprague
#'   interpolation after \insertCite{Westland2015}{lighting}: \code{"sprague"}
#' @param tolerance used for sprague interpolation wrapper to correct numerical
#'   differentiation errors. Important to calculate the interpolation factor f.
#'   Default: 1e-14.
#'
#' @return returns a dataframe of interpolated spectra in wide table format
#'   specified by \code{wl_out}.
#' @export
#'
#' @author William Truong
#'
#' @examples
#' # Create spectrum planckian radiator using black body temperature in K
#' wavelength <- seq(380, 780, 5)
#' planck2700 <- colorscience::emittanceblackbodyPlanck(seq(380, 780, 5), 2700)
#' planck5000 <- colorscience::emittanceblackbodyPlanck(seq(380, 780, 5), 5000)
#'
#' spectra <- data.frame(wavelength, planck2700, planck5000)
#' interpolate_spectra(spectra, seq(380, 780, 1), method = "linear")
#' interpolate_spectra(spectra, seq(380, 780, 1), method = "sprague")
#' interpolate_spectra(spectra, seq(400, 700, 1), method = "sprague")
#'
#' @importFrom stats approx
#' @importFrom stats spline
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom purrr map_df
#' @importFrom rlang :=
interpolate_spectra <- function(
  spectra,
  wl_out = seq(380, 780, 5),
  method = "linear",
  str_wavelength = NULL,
  tolerance = 1e-14
  ) {


  # defining first column of dataframe as str_wavelength
  if (is.null(str_wavelength)) {
    str_wavelength <- colnames(spectra)[1]
  }
  wl <- spectra[[str_wavelength]]

  # change string to variable name
  var_wl <- as.name(str_wavelength)

  # spline interpolation using stats::spline()
  if (method == "linear") {
    spectra_interpolated <- spectra %>%
      select(-var_wl) %>%
      map_df(
        map_approx,
        x = wl,
        xout = wl_out,
        method = method,
        yleft = 0,
        yright = 0
      )
    spectra_interpolated <- cbind(wl_out, spectra_interpolated) %>%
      rename(!!str_wavelength := wl_out)
  }

  # spline interpolation using stats::spline()
  if (method == "fmm"      |
      method == "periodic" |
      method == "natural"  |
      method == "monoH.FC" |
      method == "hyman") {
    spectra_interpolated <- spectra %>%
      select(-var_wl) %>%
      map_df(
        map_spline,
        x = wl,
        xout = wl_out,
        method = method
      )
    spectra_interpolated <- cbind(wl_out, spectra_interpolated) %>%
      rename(!!str_wavelength := wl_out)
  }

  # sprague interpolation wrapper for interpolate_sprague()
  if (method == "sprague") {

    # checking wl_output range
    if (min(wl) > min(wl_out) | max(wl) < max(wl_out)) {
      stop("No extrapolation with Sprague method. Check wl_out.")
    }

    # checking if wavelength array ist equidistant
    chk_diff <- abs(min(diff(wl)) - max(diff(wl)))
    if (chk_diff > tolerance) {
      stop("Wavelength values are not equidistant.")
    } else {
      # correcting numerical difference error
      wl_diff <- unique(signif(diff(wl), 1))
    }
    # checking if wavelength array ist equidistant
    chk_diff <- abs(min(diff(wl_out)) - max(diff(wl_out)))
    if (chk_diff > tolerance) {
      stop("Wavelength_out values are not equidistant.")
    } else {
      # correcting numerical difference error
      wl_out_diff <- unique(signif(diff(wl_out), 1))
    }

    f <- wl_diff / wl_out_diff # calculation of interpolation factor

    if (f != 1) {
      spectra_interpolated <- spectra %>%
        filter(min(wl_out) <= eval(var_wl) & eval(var_wl) <= max(wl_out)) %>%
        select(-var_wl) %>%
        map_df(
          interpolate_sprague,
          f = f
        )
      spectra_interpolated <- cbind(wl_out, spectra_interpolated) %>%
        rename(!!str_wavelength := wl_out)
    } else {
      spectra_interpolated <- spectra %>%
        filter(min(wl_out) <= eval(var_wl) & eval(var_wl) <= max(wl_out))
    }
  }

  return(spectra_interpolated)
}
