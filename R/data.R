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
