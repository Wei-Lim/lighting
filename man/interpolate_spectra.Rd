% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lighting_values.R
\name{interpolate_spectra}
\alias{interpolate_spectra}
\title{Interpolates a dataframe of spectra with a wavelength column.}
\usage{
interpolate_spectra(
  spectra,
  wl_out = seq(380, 780, 5),
  method = "linear",
  str_wavelength = NULL,
  tolerance = 1e-14
)
}
\arguments{
\item{spectra}{is a dataframe of spectra in wide table format including
a wavelength column in nm.}

\item{wl_out}{defines the output wavelength range and interval in nm for
interpolation.
Default: \code{wl_out = seq(380, 780, 5)}}

\item{method}{specifies the interpolation method of the functions
\link[stats]{approx} and \link[stats]{spline}. Default:
\code{"linear"}. Spline interpolation: \code{"fmm"}, \code{"periodic"},
\code{"natural"}, \code{"monoH.FC"} and \code{"hyman"}. Sprague
interpolation after \insertCite{Westland2015}{lighting}: \code{"sprague"}}

\item{str_wavelength}{Define name of wavelength column in \code{spectra}
dataframe. Default: \code{str_wavelength = NULL}. If entry is \code{NULL},
then the name of the first column from dataframe \code{spectra} will
chosen as \code{str_wavelength}.}

\item{tolerance}{used for sprague interpolation wrapper to correct numerical
differentiation errors. Important to calculate the interpolation factor f.
Default: 1e-14.}
}
\value{
returns a dataframe of interpolated spectra in wide table format
specified by \code{wl_out}.
}
\description{
This function provides the all recommended interpolation method for spectra.
This includes linear, spline and sprague interpolation.
}
\details{
In Details - Not tested for wavelength interval < 1 nm.
Linear interpolating: extrapolation results into 0 values.
}
\examples{
# Create spectrum planckian radiator using black body temperature in K
wavelength <- seq(380, 780, 5)
planck2700 <- planck_law(seq(380, 780, 5), 2700)
planck5000 <- planck_law(seq(380, 780, 5), 5000)

spectra <- tibble::tibble(wavelength, planck2700, planck5000)
interpolate_spectra(spectra, seq(380, 780, 1), method = "linear")
interpolate_spectra(spectra, seq(380, 780, 1), method = "fmm")
interpolate_spectra(spectra, seq(400, 700, 1), method = "sprague")
}
\author{
Dr. William Truong
}
