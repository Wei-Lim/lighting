# LIGHTING DISTRIBUTION ----

# 1.0 EXTRACTING LDT ----

#' Extracts Luminous Intensity LDT Data
#'
#' @description
#'
#' Extracts luminous intensity from LDT-file. It depends on the number of
#' luminous intensities per C-plane and per gamma angle. Internal private
#' function for iteration purposes.
#'
#' @param C A single C-plane value.
#' @param i Counter for the unique C-plane values. Used for extracting the right
#' lines.
#' @param gamma A vector of gamma angles. Length corresponds to the number of
#' luminous intensity per gamma angle.
#' @param Ng Number of luminous intensities in each C-plane.
#' @param lines_data Lines with luminous intensity data from the LDT file.
#'
#'
#' @export
extract_lum_intensity_ldt <-
	function(C, i, gamma, Ng, lines_data) {

		I   <- lines_data[((i - 1) * Ng + 1):(i * Ng)] %>% as.numeric()

		# Creates tibble dataframe
		tbl <- tibble::tibble(C, gamma, I)

		return(tbl)
	}

# 1.1 READ LDT DATA ----

#' Reads LDT Data
#'
#' @description
#'
#' Reads light distribution data from LDT file. The data is stored in a specific
#' list (ld_list) with light distribution data.
#'
#' @param file Path to LDT-file.
#'
#' @returns \code{ld_list}: Specific list for light distributions that can be
#' used with all functions that have a prefix "ld_".
#'
#' @details Returns a list (\code{ld_list}) with following items:
#'
#' \strong{Filepath}
#' - \code{filepath}: Path to LDT-file
#' - \code{file_name}: Name of LDT-file without extension
#'
#' \strong{
#'   \href{https://docs.agi32.com/PhotometricToolbox/Content/Open_Tool/eulumdat_file_format.htm}{EULUMDAT}
#'   (\href{https://de.wikipedia.org/wiki/EULUMDAT}{see German definitions})
#' }
#'
#' - \code{company}: Company identification/databank/version/format identification
#' - \code{Ityp}: Type indicator
#' - \code{Isym}: Symmetry indicator
#' - \code{Mc}: Number of C-planes between 0 and 360 degrees
#' - \code{Dc}: Distance between C-planes
#' - \code{Ng}: Number of luminous intensities in each C-plane
#' - \code{Dg}: Distance between luminous intensities per C-plane
#' - \code{report_no}: Measurement report number
#' - \code{luminaire_name}: Luminaire name
#' - \code{luminaire_no}: Luminaire number
#' - \code{file_name_ldt}: File name written in LDT-file
#' - \code{date_user}: Date/user
#' - \code{length}: Length/diameter of luminaire (mm)
#' - \code{width}: b - Width of luminaire (mm) (b = 0 for circular luminaire)
#' - \code{height}: Height of luminaire (mm)
#' - \code{length_lum}: Length/diameter of luminous area (mm)
#' - \code{width_lum}: b1 - Width of luminous area (mm) (b1 = 0 for circular
#' luminous area of luminaire)
#' - \code{height_lum_C0}: Height of luminous area C0-plane (mm)
#' - \code{height_lum_C90}: Height of luminous area C90-plane (mm)
#' - \code{height_lum_C180}: Height of luminous area C180-plane (mm)
#' - \code{height_lum_C270}: Height of luminous area C270-plane (mm)
#' - \code{DFF}: Downward flux fraction (%)
#' - \code{LORL}: Light output ratio luminaire (%)
#' - \code{cf}: Conversion factor for luminous intensities (depending on measurement)
#' - \code{tilt}: Tilt of luminaire during measurement (road lighting luminaires)
#' - \code{lamp_standard_sets_no}: n - Number of standard sets of lamps
#' (optional, #' also extendable on company-specific basis).#' For absolute
#' photometry, this #' value is 1.
#' - \code{lamp_no}: Number of lamps. For absolute photometry, number is
#' negative.
#' - \code{lamp_type}: Type of lamps
#' - \code{lum_flux}: Total luminous flux of lamps (lm). For absolute
#' photometry, this field is Total Luminous Flux of Luminaire.
#' - \code{cct}: Color appearance / color temperature of lamps
#' - \code{cri}: Color rendering group / color rendering index
#' - \code{power}: Wattage including ballast (W)
#' - \code{DR}: Direct ratios for room indices k = 0.6 ... 5 (for determination
#' of luminaire numbers according to utilization factor method)
#' - \code{angle_C}: Angles of C-planes (beginning with 0 degrees, horizontal)
#' - \code{angle_G}: Gamma angles (beginning with 0 degrees, vertical)
#' - \code{lum_int_tbl}: Luminous intensity tibble as a function of C-angle and
#' G-angle. Dependent on light distribution symmetry (Isym)
#' - \code{lum_int_extended_tbl}: Luminous intensity tibble extended by given C-
#' and G-angles. For Plots, Calculation and Conversion.
#'
#' \strong{IES LM-63-02 additional definitons}
#' - \code{test_lab}: Photometric testing laboratory
#' - \code{photometry_type}: 1 = C, 2 = B, 3 = A
#' - \code{ballast_factor}: Ballast factor (ballast efficiency)
#'
#' @examples
#' library(tidyverse)
#' library(furrr)
#'
#' file <- system.file('extdata', "ldt_min_example.ldt", package = "lighting")
#'
#' read_ldt(file)
#'
#' @export
read_ldt <-
	function(file) {

		# Read txt-lines
		lines    <- readLines(file)
		header   <- lines[1:42]
		data     <- lines[-(1:42)]

		ld_list = list(

			## Filepath definitions
			filepath  = file,
			file_name = basename(file) %>% tools::file_path_sans_ext(),

			## * Header ----

			# EULUMDAT description
			# see https://de.wikipedia.org/wiki/EULUMDAT
			# see https://docs.agi32.com/PhotometricToolbox/Content/Open_Tool/eulumdat_file_format.htm

			# Company identification/databank/version/format identification
			company = header[1],

			# Ityp - Type indicator
			Ityp = header[2],

			# Isym - Symmetry indicator
			Isym = header[3],

			# Mc - Number of C-planes between 0 and 360 degrees
			Mc   = header[4] %>% as.numeric(),

			# Dc - Distance between C-planes
			Dc   = header[5] %>% as.numeric(),

			# Ng - Number of luminous intensities in each C-plane
			Ng   = header[6] %>% as.numeric(),

			# Dg - Distance between luminous intensities per C-plane
			Dg   = header[7] %>% as.numeric(),

			# Measurement report number
			report_no      = header[8],

			# Luminaire name
			luminaire_name = header[9],

			# Luminaire number
			luminaire_no   = header[10],

			# File name
			file_name_ldt  = header[11],

			# Date/user
			date_user      = header[12],

			# Length/diameter of luminaire (mm)
			length = header[13] %>% as.numeric(),

			# b - Width of luminaire (mm) (b = 0 for circular luminaire)
			width  = header[14] %>% as.numeric(),

			# Height of luminaire (mm)
			height = header[15] %>% as.numeric(),

			# Length/diameter of luminous area (mm)
			length_lum      = header[16] %>% as.numeric(),

			# b1 - Width of luminous area (mm) (b1 = 0 for circular luminous area of luminaire)
			width_lum       = header[17] %>% as.numeric(),

			# Height of luminous area C0-plane (mm)
			height_lum_C0   = header[18] %>% as.numeric(),

			# Height of luminous area C90-plane (mm)
			height_lum_C90  = header[19] %>% as.numeric(),

			# Height of luminous area C180-plane (mm)
			height_lum_C180 = header[20] %>% as.numeric(),

			# Height of luminous area C270-plane (mm)
			height_lum_C270 = header[21] %>% as.numeric(),

			# DFF - Downward flux fraction (%)
			DFF  = header[22] %>% as.numeric(),

			# LORL - Light output ratio luminaire (%)
			LORL = header[23] %>% as.numeric(),

			# Conversion factor for luminous intensities (depending on measurement)
			cf   = header[24] %>% as.numeric(),

			# Tilt of luminaire during measurement (road lighting luminaires)
			tilt = header[25] %>% as.numeric(),

			# n - Number of standard sets of lamps (optional, also extendable on company-specific basis)
			# For absolute photometry, this value is 1
			lamp_standard_sets_no = header[26] %>% as.numeric(),

			# Number of lamps
			# For absolute photometry, number is negative
			lamp_no               = header[27] %>% as.numeric(),

			# Type of lamps
			lamp_type             = header[28],

			# Total luminous flux of lamps (lm)
			# For absolute photometry, this field is Total Luminous Flux of Luminaire
			lum_flux = header[29] %>% as.numeric(),

			# Color appearance / color temperature of lamps
			cct      = header[30],

			# Color rendering group / color rendering index
			cri      = header[31],

			# Wattage including ballast (W)
			power    = header[32] %>% as.numeric(),

			# DR - Direct ratios for room indices k = 0.6 ... 5 (for determination of luminaire numbers according to utilization factor method)
			DR       = header[33:42] %>% as.numeric()
		)


		## * Angles definitions ----
		Mc   <- ld_list$Mc
		Ng   <- ld_list$Ng
		Dc   <- ld_list$Dc
		Isym <- ld_list$Isym

		# Angles C (beginning with 0 degrees)
		angle_C <- data[1:Mc] %>% as.numeric()
		data    <- data[-(1:Mc)] # removing angles from data

		# Angles G (beginning with 0 degrees)
		angle_G <- data[1:Ng] %>% as.numeric()
		data    <- data[-(1:Ng)] # removing angles from data

		## * Iteration to extracting light intensity data according to C and G angles ----
		# luminous intensity dependent on LDT symmetry
		switch(Isym,

			# 0 - no symmetry
			`0` = C <- seq(0, 360 - Dc, Dc),

			# 1 - symmetry about the vertical axis
			`1` = C <- 0,

			# 2 - symmetry to plane C0-C180
			`2` = C <- seq(0, 180, Dc),

			# 3 - symmetry to plane C90-C270
			`3` = C <- seq(90, 270, Dc),

			# 4 - symmetry to plane C0-C180 and to plane C90-C270
			`4` = C <- seq( 0,  90, Dc)
		)
		i <- 1:length(C)

		# Luminous intensity distribution (cd/1000 lumens)
		lum_int_tbl <- tibble::tibble(C, i) %>%
			dplyr::mutate(tbl = furrr::future_map2(C, i, extract_lum_intensity_ldt, gamma = angle_G, Ng = Ng, lines_data = data)) %>%
			dplyr::select(-C, -i) %>%
			tidyr::unnest(tbl) %>%
			tidyr::pivot_wider(names_from = C, names_prefix = "C", values_from = I)

		## * Extend luminous intensity data for plotting and calculating ----
		tbl <- lum_int_tbl %>%
			tidyr::pivot_longer(-gamma, "C", names_prefix = "C", names_transform = as.numeric, values_to = "I")

		switch(
			Isym,

			# 0 - no symmetry
			`0` = lum_int_extended_tbl <- tbl,

			# 1 - symmetry about the vertical axis
			`1` = lum_int_extended_tbl <- tbl %>%
				dplyr::bind_rows(
					tbl %>% dplyr::mutate(C = 180)
				),

			# 2 - symmetry to plane C0-C180
			`2` = lum_int_extended_tbl <- tbl %>%
				dplyr::bind_rows(
					tbl %>%
						dplyr::mutate(C = rev(C) + 180) %>%
						dplyr::filter(C != 180, C != 360)
				) %>%
				dplyr::arrange(C),

			# 3 - symmetry to plane C90-C270
			`3` = lum_int_extended_tbl <- tbl %>%
				dplyr::bind_rows(
					tbl %>%
						dplyr::mutate(C = rev(C + 180) ) %>%
						dplyr::mutate(C = dplyr::if_else(C >= 360, C - 360, C)) %>%
						dplyr::filter(C != 90, C != 270)
				) %>%
				dplyr::arrange(C),

			# 4 - symmetry to plane C0-C180 and to plane C90-C270
			`4` = {
				tbl2 <- tbl %>%
					dplyr::bind_rows(
						tbl %>%
							dplyr::mutate(C = rev(C + 90)) %>%
							dplyr::filter(C != 90)
					) %>%
					dplyr::arrange(C)

				lum_int_extended_tbl <- tbl2 %>%
					dplyr::bind_rows(
						tbl2 %>%
							dplyr::mutate(C = rev(C) + 180) %>%
							dplyr::filter(C != 180, C != 360)
					) %>%
					dplyr::arrange(C)
			}
		)

		lum_int_extended_tbl <- lum_int_extended_tbl %>%
			tidyr::pivot_wider(names_from = C, names_prefix = "C", values_from = I)


		# * Appending angles and luminous intensity tables to LDT-list ----
		ld_list <- ld_list %>%
			append(list(

				angleC               = angle_C,
				angleG               = angle_G,
				lum_int_tbl          = lum_int_tbl,
				lum_int_extended_tbl = lum_int_extended_tbl,

				## IES additional definitions
				# Photometric testing laboratory
				test_lab = "-",

				# Photometry type: 1 = C, 2 = B, 3 = C
				photometry_type = "1",

				# ballast factor
				ballast_factor = 1
			))

		return(ld_list)
	}
