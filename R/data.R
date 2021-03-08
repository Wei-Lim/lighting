#' CIE-1974 test-colour samples
#'
#' \loadmathjax
#' A dataset of spectral radiance factor of the CIE-1974 test-colour samples to
#' be used in calculating the General Colour Rendering Index.
#'
#' @format A wide format data frame with 15 variables:
#' \describe{
#'   \item{nm}{wavelength, in nm. Range and intervall: \code{seq(360, 830, 5)}}
#'   \item{TCS_01}{CIE test colour sample 01, relativ spectra}
#'   ...
#'   \item{TCS_14}{CIE test colour sample 14, relativ spectra}
#' }
#' @importFrom mathjaxr preview_rd
#' @source \insertRef{CIE1995}{lighting}
"cie1995_13.3"

#' CIE daylight components
#'
#' A dataset containing the components of the relative spectral distribution of
#' daylight used in the calculation of relative spectral power distributions of
#' CIE daylight illuminants of different correlated colour temperatures.
#'
#' @format A wide format data frame with 4 variables:
#' \describe{
#'   \item{nm}{wavelength, in nm. Range and intervall: \code{seq(300, 830, 5)}}
#'   \item{S_0}{CIE daylight component 0, relativ spectra}
#'   \item{S_1}{CIE daylight component 1, relativ spectra}
#'   \item{S_2}{CIE daylight component 2, relativ spectra}
#' }
#' @source \insertRef{CIE1995}{lighting}
"cie2018_015"

#' CIE alpha-opic action spectra
#'
#' A dataset containing the action spectra representing the relative spectral
#' sensitivity of the five alpha-opic human photoreceptors to optical radiation
#' incident at the cornea, normalised to have a maximum value of 1. These action
#' spectra are based on pre-receptoral filtering for a reference observer of age
#' 32 years (CIE 1931 standard colorimetric observer) and are use to compute the
#' metrology of optical readiation for ipRGC-influenced responses to light.
#'
#' @format A wide format data frame with 4 variables:
#' \describe{
#'   \item{nm}{wavelength, in nm. Range and intervall: \code{seq(300, 830, 5)}}
#'   \item{s_sc}{S-cone-opic relating to the human S-cone response due to its
#'   photopigment}
#'   \item{s_mc}{S-cone-opic relating to the human M-cone response due to its
#'   photopigment}
#'   \item{s_lc}{S-cone-opic relating to the human L-cone response due to its
#'   photopigment}
#'   \item{s_rh}{rhodopic relating to the human rod response due to its
#'   photopigment (rhodopsin)}
#'   \item{s_mel}{melanopic relating to the human rod response due to its
#'   photopigment (melanopsin)}
#' }
#' @source \insertRef{CIE2018}{lighting}
"cie2018_S_026"

#' CIE luminosity and colour matching functions
#'
#' A dataset containing the CIE-1924 photopic luminosity function, CIE-1951
#' scotopic luminosity function, CIE-1931 2-degree XYZ colour matching functions
#' and CIE 1964 10-degree XYZ colour matching functions.
#'
#' @format A wide format data frame with 4 variables:
#' \describe{
#'   \item{nm}{wavelength, in nm. Range and intervall: \code{seq(360, 830, 1)}}
#'   \item{V_pho}{CIE-1924 photopic luminosity function, normalised to a maximum
#'   value of 1.}
#'   \item{V_sco}{CIE-1951 scotopic luminosity function, normalised to a maximum
#'   value of 1.}
#'   \item{x_cmf_2}{CIE-1931 2-degree colour matching function x}
#'   \item{y_cmf_2}{CIE-1931 2-degree colour matching function y}
#'   \item{z_cmf_2}{CIE-1931 2-degree colour matching function z}
#'   \item{x_cmf_10}{CIE-1964 10-degree colour matching function x}
#'   \item{y_cmf_10}{CIE-1964 10-degree colour matching function y}
#'   \item{z_cmf_10}{CIE-1964 10-degree colour matching function z}
#' }
#' @source \url{www.cvrl.org}
"cvrl.org"

#' Spectral sensitivity function of human photopigments
#'
#' A dataset containing the spectral sensitivity function of human
#' photopigments, photopic luminosity function and eye lens transmission
#' function provided by \insertCite{Lucas2014}{lighting} to calculate the
#' alpha-opic illuminances.
#'
#' @format A wide format data frame with 4 variables:
#' \describe{
#'   \item{nm}{wavelength, in nm. Range and intervall: \code{seq(380, 780, 5)}}
#'   \item{N_sc}{Cyanolabe response function (S-cone photopsin), normalised to
#'   give an integrated area of 1}
#'   \item{N_mc}{Chlorolabe response function (M-cone photopsin), normalised to
#'   give an integrated area of 1}
#'   \item{N_lc}{Erythrolabe response function (L-cone photopsin), normalised to
#'   give an integrated area of 1}
#'   \item{N_rh}{Rod opsin response function (Rod opsin), normalised to
#'   give an integrated area of 1}
#'   \item{N_z}{Melanopsin response function (Melanopsin), normalised to
#'   give an integrated area of 1}
#'   \item{V_pho}{CIE-1924 photopic luminosity function, normalised to a maximum
#'   value of 1.}
#'   \item{T_lens}{Eye lens transmission of a 32 year old standard observer.}
#' }
#' @source \insertRef{Lucas2014}{lighting}
"lucas2014"

#' Spectral sensitivity functions to calculate Circadian Stimulus (2010)
#'
#' A dataset containing the spectral sensitivity function provided by
#' \insertCite{Rea2010}{lighting} to calculate the Circadian Stimulus
#' (deprecated version 2010).
#'
#' @format A wide format data frame with 4 variables:
#' \describe{
#'   \item{nm}{wavelength, in nm. Range and intervall: \code{seq(380, 780, 10)}}
#'   \item{S_sc}{S-cone spectral efficiency function}
#'   \item{V_sco}{Rod spectral efficiency function}
#'   \item{M}{Melanopsin-containing retinal ganglion cell spectral
#'   efficiency function peaking at 480 nm}
#'   \item{V_pho_10}{Large-filed L+M cone spectral efficiency function}
#' }
#' @source \insertRef{Rea2010}{lighting}
"rea2010"

#' Spectral sensitivity functions to calculate Circadian Stimulus (2018)
#'
#' A dataset containing the spectral sensitivity function provided by
#' \insertCite{Rea2018}{lighting} to calculate the Circadian Stimulus
#' (2018).
#'
#' @format A wide format data frame with 4 variables:
#' \describe{
#'   \item{nm}{wavelength, in nm. Range and intervall: \code{seq(380, 780, 2)}}
#'   \item{V_pho}{photopic luminous efficiency function}
#'   \item{V_sco}{scotopic luminous efficiency function}
#'   \item{V_pho_mac}{photopic luminous efficiency function weighted with
#'   macular pigment transmittance}
#'   \item{S_sc_mac}{S-cone fundamental weighted with macular pigment
#'   transmittance}
#'   \item{Mc_lens}{Melanopsin spectral effiency function, corrected for
#'   crystalline lens transmittance}
#' }
#' @source \insertRef{Rea2018}{lighting} \cr
#' \url{http://www.lrc.rpi.edu/resources/CSCalculator_2017_10_03_Mac.xlsm}
"rea2018"
