#' Interpolates a dataframe of spectra with a wavelength column
#'
#' @param spectra is a dataframe of spectra in wide table format including
#'   a wavelength column in nm.
#' @param wl_out defines the output wavelength range and steps in nm for
#'   interpolation.
#'   Default: \code{wl_out = seq(380,780,5)}
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
#'
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#' @importFrom data.table melt
#' @importFrom data.table dcast
#' @importFrom data.table :=
#' @importFrom data.table setnames
#' @importFrom stats approx
#' @importFrom stats spline
#' @importFrom stats as.formula
#' @importFrom stats as.formula
#' @importFrom magrittr %>%
interpolate_spectra <- function(
  spectra,
  wl_out = seq(380, 780, 5),
  method = "linear",
  str_wavelength = NULL,
  tolerance = 1e-14
  ) {

  # defining local variable to suppress note in check()
  spectrum <- value <- wavelength <- NULL

  # defining first column of dataframe as str_wavelength
  if (is.null(str_wavelength)) {
    str_wavelength <- colnames(spectra)[1]
  }

  # convert wide table spectra to long table
  spectra_long <- melt(
    setDT(spectra),
    id.vars = str_wavelength,
    variable.name = "spectrum",
    value.name = "value"
  )

  # change string to variable name
  var_wl <- as.name(str_wavelength)

  # spline interpolation using stats::spline()
  if (method == "linear") {
    spectra_interpolated <- spectra_long[
      ,
      approx(x = eval(var_wl), y = value, xout = wl_out, method = method),
      by = spectrum
    ] %>%
      setnames(
        old = c("x", "y"),
        new = c(str_wavelength, "value")
      ) %>%
      dcast(
        as.formula(paste(str_wavelength, "~", "spectrum")),
        value.var = "value"
      ) %>%
      setDF()
  }

  # spline interpolation using stats::spline()
  if (method == "fmm"      |
      method == "periodic" |
      method == "natural"  |
      method == "monoH.FC" |
      method == "hyman") {
    spectra_interpolated <- spectra_long[
      ,
      spline(x = eval(var_wl), y = value, xout = wl_out, method = method),
      by = spectrum
    ] %>%
      setnames(
        old = c("x", "y"),
        new = c(str_wavelength, "value")
      ) %>%
      dcast(
        as.formula(paste(str_wavelength, "~", "spectrum")),
        value.var = "value"
      ) %>%
      setDF()
  }

  # sprague interpolation wrapper for interpolate_sprague()
  if (method == "sprague") {
    wl <- spectra[, eval(var_wl)]

    # checking wavelength range of wl and wl_out
    chk_wl_min <- abs(min(wl) - min(wl_out))
    chk_wl_max <- abs(max(wl) - max(wl_out))
    if (chk_wl_min > tolerance | chk_wl_max > tolerance) {
      stop("range of wavelength and wavelength_out must be equal. Please check
           your inputs!")
    }

    # checking if wavelength array ist equidistant
    chk_diff <- abs(min(diff(wl)) - max(diff(wl)))
    if (chk_diff > tolerance) {
      stop("wavelength values are not equidistant.")
    } else {
      # correcting numerical difference error
      x_diff <- unique(signif(diff(wl), 1))
    }
    # checking if wavelength array ist equidistant
    chk_diff <- abs(min(diff(wl_out)) - max(diff(wl_out)))
    if (chk_diff > tolerance) {
      stop("wavelength_out values are not equidistant.")
    } else {
      # correcting numerical difference error
      x_out_diff <- unique(signif(diff(wl_out), 1))
    }

    f <- x_diff / x_out_diff # calculation of interpolation factor

    spectra_interpolated <- spectra_long[
      ,
      interpolate_sprague(y = value, f),
      by = spectrum
    ]

    spectra_interpolated <- spectra_interpolated[
      ,
      wavelength := wl_out,
      by = spectrum
    ] %>%
      setnames(old = "wavelength", new = str_wavelength) %>%
      dcast(
        as.formula(paste(str_wavelength, "~", "spectrum")),
        value.var = "V1"
      ) %>%
      setDF()
  }

  return(spectra_interpolated)
}
