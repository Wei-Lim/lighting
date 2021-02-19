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
#'   \link[stats]{approx} and \link[stats]{spline}. Default: \code{"linear"}.
#'   Other choices for \link[stats]{spline} are: \code{"fmm"},
#'   \code{"periodic"}, \code{"natural"}, \code{"monoH.FC"} and \code{"hyman"}.
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
#' spectra <- data.frame(wavelength, planck2700, planck5000)
#' interpolate_spectra(spectra, seq(380, 780, 1), method = "linear")
#'
#' @importFrom data.table setDT
#' @importFrom data.table setDF
#' @importFrom data.table melt
#' @importFrom data.table dcast
#' @importFrom data.table setnames
#' @importFrom stats approx
#' @importFrom stats spline
#' @importFrom stats as.formula
#' @importFrom stats as.formula
#' @importFrom magrittr %>%
interpolate_spectra <- function(
  spectra,
  wl_out = seq(380,780,5),
  str_wavelength = NULL,
  method = "linear") {

  # defining local variable to suppress note in check()
  spectrum <- value <- NULL

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


  return(spectra_interpolated)
}
