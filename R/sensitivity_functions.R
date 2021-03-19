#' Returns list of interpolated spectral sensitivity functions
#'
#' In order to compute lighting values by numerical intergration the spectral
#' power distribution and spectral sensitivity function must be interpolated to
#' a common wavelength range and interval. This function provides a collection
#' sensitivity function for calcuation of different lighting values. As
#' recommended by \insertCite{CIE2018b}{lighting} the sprague method is
#' performed to interpolate smooth spectra. The exception is the
#' interpolation of daylight components and CIE test colour samples, which
#' should be explicitly interpolated linearly according to
#' \insertCite{CIE2018b,CIE1995}{lighting}.
#'
#' Firstly, the sensitivity functions are interpolated to specified wavelength
#' interval and their original wavelength range using the recommended methods by
#' \insertCite{CIE2018b,CIE1995}{lighting}. Secondly the functions are linear
#' interpolated to the specified wavelength range and interval; extrapolated
#' values are returned with 0. Wavelength interval cannot be smaller than 1.
#'
#' @param wl_out defines the wavelength interval. Wavelength range
#' (380-780)nm.
#'
#' @return list of sensitivity functions for computing lighting values
#' @export
#'
#' @examples
#' # Returning in wavelength seq(380, 780, 1)
#' sensitivity_functions()
#' # Returning original datasets as a list
#' sensitivity_functions(NULL)
#' # Returning in wavelength seq(200, 830, 1)
#' sensitivity_functions(seq(200, 830, 1))
#'
#' @references
#'     \insertAllCited{}
#'
#' @importFrom Rdpack reprompt
sensitivity_functions <- function(
  wl_out = seq(380, 780, 1)
  ) {


  df_cie1995_13.3 <- cie1995_13.3
  df_cie2018_015 <- cie2018_015
  df_cie2018_S_026 <- cie2018_S_026
  df_cvrl.org <- cvrl.org
  df_lucas2014 <- lucas2014
  df_rea2010 <- rea2010
  df_rea2018 <- rea2018

  if (!is.null(wl_out)) {
    wl_step <- unique(diff(wl_out))
    if (wl_step < 1 | wl_step != floor(wl_step)) {
      stop("Wavelenth interval not allowed. Only integer values > 0.")
    }

    # function for interpolating sensitivity functions to 1 nm steps
    interpolate_1nm <- function(df, method) {
      wl_1nm <- seq(min(df$nm), max(df$nm), 1)
      df <- interpolate_spectra(df, wl_1nm, method)
      return(df)
    }

    # CIE 015:2018 section 7.2.3 recommends sprague interpolation, if wavelength
    # intervall is konstant, except interpolation of daylight components
    df_cie1995_13.3 <- interpolate_1nm(df_cie1995_13.3, "linear")
    df_cie2018_015 <- interpolate_1nm(df_cie2018_015, "linear")
    df_cie2018_S_026 <- interpolate_1nm(df_cie2018_S_026, "sprague")
    df_cvrl.org <- interpolate_1nm(df_cvrl.org, "sprague")
    df_lucas2014 <- interpolate_1nm(df_lucas2014, "sprague")
    df_rea2010 <- interpolate_1nm(df_rea2010, "sprague")
    df_rea2018 <- interpolate_1nm(df_rea2018, "sprague")

    # Converting sensitivity function to specified wavelength step
    df_cie1995_13.3 <- interpolate_spectra(df_cie1995_13.3, wl_out, "linear")
    df_cie2018_015 <- interpolate_spectra(df_cie2018_015, wl_out, "linear")
    df_cie2018_S_026 <- interpolate_spectra(df_cie2018_S_026, wl_out, "linear")
    df_cvrl.org <- interpolate_spectra(df_cvrl.org, wl_out, "linear")
    df_lucas2014 <- interpolate_spectra(df_lucas2014, wl_out, "linear")
    df_rea2010 <- interpolate_spectra(df_rea2010, wl_out, "linear")
    df_rea2018 <- interpolate_spectra(df_rea2018, wl_out, "linear")
  }

  # Create named list with specific spectral sensitivity functions
  sensitivity <- list(
    cie1995_13.3 = df_cie1995_13.3,
    cie2018_015 = df_cie2018_015,
    cie2018_S_026 = df_cie2018_S_026,
    cvrl.org = df_cvrl.org,
    lucas2014 = df_lucas2014,
    rea2010 = df_rea2010,
    rea2018 = df_rea2018
  )

  return(sensitivity)
}
