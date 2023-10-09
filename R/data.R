# 1 LIGHT DISTRIBUTION ----
# 1.1 Lighting Distribution Data Set ----
#' @title Lighting Distribution Data Set
#'
#' @description Contains data of an example lighting distribution in list format.
#' This list format is mainly based on the description of the EULUMDAT format
#' (see definition in [EN](https://docs.agi32.com/PhotometricToolbox/Content/Open_Tool/eulumdat_file_format.htm),
#' in [DE](https://de.wikipedia.org/wiki/EULUMDAT)).
#' Some additional features have been introduced, to allow the dataset to be compatible
#' with the IES (LM63-2002) format.
#'
#' @format A wide format data frame with 15 variables:
#' \describe{
#'   \item{filepath}{Path to LDT-file}
#'   \item{file_name}{Name of LDT-file without extension}
#'   \item{company}{Company identification/databank/version/format identification}
#'   \item{Ityp}{Type indicator}
#'   \item{Isym}{Symmetry indicator}
#'   \item{Mc}{Number of C-planes between 0 and 360 degrees}
#'   \item{Dc}{Distance between C-planes}
#'   \item{Ng}{Number of luminous intensities in each C-plane}
#'   \item{Dg}{Distance between luminous intensities per C-plane}
#'   \item{report_no}{Measurement report number}
#'   \item{luminaire_name}{Luminaire name}
#'   \item{luminaire_no}{Luminaire number}
#'   \item{file_name_ldt}{File name written in LDT-file}
#'   \item{date_user}{Date/user}
#'   \item{length}{Length/diameter of luminaire (mm)}
#'   \item{width}{b - Width of luminaire (mm) (b = 0 for circular luminaire)}
#'   \item{height}{Height of luminaire (mm)}
#'   \item{length_lum}{Length/diameter of luminous area (mm)}
#'   \item{width_lum}{b1 - Width of luminous area (mm) (b1 = 0 for circular luminous area of luminaire)}
#'   \item{height_lum_C0}{Height of luminous area C0-plane (mm)}
#'   \item{height_lum_C90}{Height of luminous area C90-plane (mm)}
#'   \item{height_lum_C180}{Height of luminous area C180-plane (mm)}
#'   \item{height_lum_C270}{Height of luminous area C270-plane (mm)}
#'   \item{DFF}{Downward flux fraction (%)}
#'   \item{LORL}{Light output ratio luminaire (%)}
#'   \item{cf}{Conversion factor for luminous intensities (depending on measurement)}
#'   \item{tilt}{Tilt of luminaire during measurement (road lighting luminaires)}
#'   \item{lamp_standard_sets_no}{n - Number of standard sets of lamps
#'    (optional, also extendable on company-specific basis).
#'    For absolute photometry, this value is 1.}
#'   \item{lamp_no}{Number of lamps. For absolute photometry, number is
#'   negative.}
#'   \item{lamp_type}{Type of lamps}
#'   \item{lum_flux}{Total luminous flux of lamps (lm). For absolute
#'   photometry, this field is Total Luminous Flux of Luminaire.}
#'   \item{cct}{Color appearance / color temperature of lamps}
#'   \item{cri}{Color rendering group / color rendering index}
#'   \item{power}{Wattage including ballast (W)}
#'   \item{DR}{Direct ratios for room indices k = 0.6 ... 5 (for determination
#'   of luminaire numbers according to utilization factor method)}
#'   \item{angle_C}{Angles of C-planes (beginning with 0 degrees, horizontal)}
#'   \item{angle_G}{Gamma angles (beginning with 0 degrees, vertical)}
#'   \item{lum_int_tbl}{Luminous intensity tibble as a function of C-angle and
#'   G-angle. Dependent on light distribution symmetry (Isym)}
#'   \item{lum_int_extended_tbl}{Luminous intensity tibble extended by given C-
#'   and G-angles. For Plots, Calculation and Conversion.}
#'   \item{test_lab}{Photometric testing laboratory}
#'   \item{photometry_type}{1 = C, 2 = B, 3 = A}
#'   \item{ballast_factor}{Ballast factor (ballast efficiency)}
#'   \item{plot}{Light distribution polar chart in ggplot2 format. After
#'   read_ldt() process, initally empty using [read_ldt()]. Added by using [ld_add_light_distribution_plot()]}
#' }
#'
#'
#' @examples
#' ld_data
#'
"ld_data"

# 2 LIGHTING VALUES AND SPECTRA ----
# 2.1 Test-Colour Samples CIE-1974 13.3 ----
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
#' @source \insertRef{CIE1995}{lighting}
"cie1995_13.3"

# 2.2 Daylight components CIE-2018 015 ----
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

# 2.3 Alpha-opic action spectra CIE-2018 S 026----
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

# 2.4 Luminosity and colour matching functions ----
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

# 2.5 Spectral sensitivity function alpha-opic illuminance (Lucas 2014) ----
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

# 2.6 Spectral sensitivity functions Circadian Stimulus (2010)----
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


