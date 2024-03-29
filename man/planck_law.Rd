% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lighting_values.R
\name{planck_law}
\alias{planck_law}
\title{Creates spectrum of a Planckian radiator}
\usage{
planck_law(wl, temperature)
}
\arguments{
\item{wl}{Wavelength vector of Planckian radiator in nm}

\item{temperature}{A single temperature value of black body radiator in kelvin}
}
\value{
spectral intensities of Planckian radiator
}
\description{
\code{plack_law()} computes the spectral intensities of
a Planckian radiator with a defined temperature of the black body.
}
\examples{
# Create wavelength vector
wl <- seq(380, 780, 1)

planck_law(wl, 2700)
planck_law(wl, 6500)

}
