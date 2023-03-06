# LIGHTING DISTRIBUTION ----

# 1.0 EXTRACTING LDT ----

#' @title Extract luminous intensity data from a LDT-file into a tibble
#'
#' @description `extract_lum_intensity_ldt()` returns a tibble of luminous
#' intensity data extracted from a LDT-file. The luminous intensities depends on
#' C-plane and gamma angles.
#'
#' @details This function is used internally in the packages for iteration purposes.
#'
#' @param C A single C-plane value.
#' @param i Counter for the unique C-plane values. Used for extracting the right
#' lines.
#' @param gamma A vector of gamma angles. Length corresponds to the number of
#' luminous intensity per gamma angle.
#' @param Ng Number of luminous intensities in each C-plane.
#' @param lines_data Lines with luminous intensity data from the LDT file.
#'
#' @returns A [tibble::tibble()] with luminous intensities, C-planes and gamma angles.
#'
#' @export
extract_lum_intensity_ldt <- function(C, i, gamma, Ng, lines_data) {

		I   <- lines_data[((i - 1) * Ng + 1):(i * Ng)] %>% as.numeric()

		# Creates tibble dataframe
		tbl <- tibble::tibble(C, gamma, I)

		return(tbl)
	}

# 1.1 READ LDT DATA ----

#' @title Read a LDT-file into a specific light distribution list format
#'
#' @description `read_ldt()` is used to extract light distribution data of a
#' LDT-file into a specific list format `ld_list` (for item description see [ld_data]).
#'
#'
#' @param file Path to LDT-file.
#'
#' @returns A specific light distribution list (`ld_list`), that can be
#' used with all functions that have a prefix `ld_` (for list items description
#' see [ld_data]).
#'
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
read_ldt <- function(file) {

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
				ballast_factor = 1,

				# placeholder for light distribution ggplot
				plot = NA
			))

		return(ld_list)
	}

# 1.2 PLOT LIGHT DISTRIBUTION ----

#' @title Plots a polar chart of a light distribution
#'
#' @description `plot_light_distribution()` returns a ggplot polar chart of a
#' light distribution using the extended luminous intensity data, which are
#' stored in a `ld_list`, like [ld_data].
#'
#' @param lum_int_extended_tbl A tibble of extended luminous intensity data
#' @param line_color A text in hex colour format
#' @param line_size A numeric, which controls the line size of [ggplot2::geom_line()]
#' @param title The text for the plot title
#' @param x_lab The text for the x-axis label
#' @param y_lab The text for the y-axis label
#'
#' @returns A ggplot2 object with polar chart visualisation
#'
#' @examples
#' plot_light_distribution(ld_data$lum_int_extended_tbl)
#' plot_light_distribution(
#'   ld_data$lum_int_extended_tbl,
#'   title = "Test title",
#'   x_lab = "x-axis",
#'   y_lab = "y-axis"
#' )
#'
#' @export
plot_light_distribution <- function(
		lum_int_extended_tbl,
		line_color = "#BCCF03", # PRACHT green
		line_size  = 1.5,
		title      = "",
		x_lab      = expression(gamma ~ "[\u00b0]"),
		y_lab      = expression("I [cd/1000 lm]")
) {

	C <- NULL

	lum_int_extended_tbl %>%

		# Data wrangling
		tidyr::pivot_longer(-gamma, "C", names_prefix = "C", names_transform = as.numeric, values_to = "I") %>%
		dplyr::filter(C == 0 | C == 90 | C == 180 | C == 270) %>%
		dplyr::mutate(
			gamma = dplyr::case_when(
				# transform gamma for plotting
				C == 0   | C == 90  ~ 360 - gamma,
				C == 180 | C == 270 ~ gamma,
			),
			C = dplyr::case_when(
				C == 0  | C == 180 ~ "C0/180",
				C == 90 | C == 270 ~ "C90/270"
			),
			C = factor(C, levels = c("C0/180", "C90/270"))
		) %>%

		# Plotting
		ggplot2::ggplot(ggplot2::aes(x = gamma, y = I, linetype = C)) +

		# Create polar graph
		ggplot2::geom_line(color = line_color, size = line_size) +
		ggplot2::coord_polar(start = pi) +

		# Define special x-axis for light distribution (see gamma transformation)
		ggplot2::scale_x_continuous(
			limits = c(0, 360),
			breaks = seq(0, 330, 30),
			labels = c("0", "30", "60", "90", "120", "150", "180", "150", "120", "90",
					   "60", "30")
		) +

		# Themes and Labels
		ggplot2::theme_light() +
		ggplot2::theme(
			plot.title         = ggplot2::element_text(size = 18),
			panel.grid.minor   = ggplot2::element_line(size = 0.5),
			panel.grid.major   = ggplot2::element_line(size = 0.5),
			axis.text          = ggplot2::element_text(size = 12),
			axis.title.y       = ggplot2::element_text(vjust = 4),
			axis.title         = ggplot2::element_text(size = 16),
			legend.text        = ggplot2::element_text(size = 12),
			legend.title       = ggplot2::element_blank(),
			legend.position    = "bottom",
			legend.box.spacing = grid::unit(0, "mm")
		) +
		ggplot2::labs(
			title = title,
			x     = x_lab,
			y     = y_lab
		)
}


# 1.3 LD: ADD LIGHT DISTRIBUTION PLOT ----

#' @title Adds plot polar chart into `ld_list`
#'
#' @description `ld_add_light_distribution_plot()` adds the ggplot2 object from
#' [plot_light_distribution()] into the light distribution list `ld_list`
#' (for list items description see [ld_data]).
#'
#' @param ld_list A specific light distribution list
#' @param line_color A text in hex colour format
#' @param line_size A numeric, which controls the line size of [ggplot2::geom_line()]
#' @param title The text for the plot title
#' @param x_lab The text for the x-axis label
#' @param y_lab The text for the y-axis label
#'
#' @returns A `ld_list`, like [ld_data]
#'
#' @examples
#' ld_add_light_distribution_plot(ld_data)
#'
#' @export
ld_add_light_distribution_plot <- function(
		ld_list,
		line_color = "#BCCF03",
		line_size  = 1.5,
		title      = "",
		x_lab      = expression(gamma ~ "[\u00b0]"),
		y_lab      = expression("I [cd/1000 lm]")
) {

	lum_int_extended_tbl <- ld_list$lum_int_extended_tbl

	ld_list$plot <- plot_light_distribution(
		lum_int_extended_tbl,
		line_color = line_color,
		line_size  = line_size,
		title      = ld_list$file_name,
		x_lab      = x_lab,
		y_lab      = y_lab
	)

	return(ld_list)
}


# 1.4 LD: WRITE TO SVG ----

#' @title Write a `ld_list` polar chart to a SVG file
#'
#' @description `ld_write_svg()` exports the ggplot2 object of the specific
#' light distribution list `ld_list` as a SVG graphic file (*.svg).
#'
#' @param ld_list A specific light distribution list
#' @param file The path to file without extension.
#'
#' @returns `ld_write_svg()` returns the ggplot2 object invisibly.
#'
#' @examples
#' ld_data$plot <- ld_data %>% ld_add_light_distribution_plot()
#'
#' # ld_write_svg() will write the file to the current working directory, if
#' # it is unspecified without directory path.
#' \dontrun{
#' ld_write_svg(ld_data, file = "test")
#' }
#' @export
ld_write_svg <- function(ld_list, file) {

	svglite::svglite(stringr::str_c(file, ".svg"))
	print(ld_list$plot)
	grDevices::dev.off()

	return(invisible(NULL))
}


# 1.5 LD: WRITE TO LDT ----

#' @title Write light distribution data (`ld_list`) to a LDT file
#'
#' @description `ld_write_ldt()` exports light distribution data (`ld_list`) as
#' an EULUMDAT file (*.ldt).
#'
#' @param ld_list A specific light distribution list
#' @param file The path to file without extension.
#' @param user The text naming the user, who manipulated the LDT-file
#'
#' @returns `ld_write_ldt()` returns a NULL invisibly.
#'
#' @examples
#' # ld_write_ldt() will write the file to the current working directory, if
#' # it is unspecified without directory path.
#' \dontrun{
#' ld_write_ldt(ld_data, file = "test")
#' }
#' @export
ld_write_ldt <- function(ld_list, file, user = "") {

	C <- NULL

	ld_list$file_name_ldt <- stringr::str_c(ld_list$file_name, ".ldt")
	ld_list$date_user <- stringr::str_c( Sys.time() %>% format("%Y-%m-%d"), user, sep = ", " )

	# Select ldt header features
	ldt_header <- c(
		"company", "Ityp", "Isym", "Mc", "Dc", "Ng", "Dg",
		"report_no", "luminaire_name", "luminaire_no", "file_name_ldt", "date_user",
		"length", "width", "height",
		"length_lum", "width_lum", "height_lum_C0", "height_lum_C90", "height_lum_C180", "height_lum_C270",
		"DFF", "LORL", "cf", "tilt",
		"lamp_standard_sets_no", "lamp_no", "lamp_type",
		"lum_flux", "cct", "cri", "power"
	)

	# Convert to character array for writing lines
	ldt_export_chr <- c(
		ld_list[ldt_header] %>% as.character(),
		ld_list$DR,
		ld_list$angleC,
		ld_list$angleG,
		ld_list$lum_int_tbl %>%
			tidyr::pivot_longer(
				# data            = -gamma,
				cols            = dplyr::starts_with("C"),
				names_prefix    = "C",
				names_to        = "C",
				names_transform = as.numeric,
				values_to       = "I"
			) %>%
			dplyr::arrange(C) %>%
			dplyr::pull(I)
	)

	writeLines(ldt_export_chr, stringr::str_c(file, ".ldt"))

	return(invisible(NULL))

}

# 1.6 LD: WRITE TO IES ----

#' @title Write light distribution data (`ld_list`) to an IES file
#'
#' @description `ld_write_ies_lm63_2002()` exports light distribution data (`ld_list`)
#' after ANSI/IESNA LM-63-2002 standard in IES file format (*.ies).
#'
#' @param ld_list A specific light distribution list
#' @param file The path to file without extension.
#'
#' @returns `ld_write_ies_lm63_2002()` returns a NULL invisibly.
#'
#' @examples
#' # ld_write_ies_lm63_2002() will write the file to the current working
#' # directory, if it is unspecified without directory path.
#' \dontrun{
#' ld_write_ies_lm63_2002(ld_data, file = "test")
#' }
#' @export
ld_write_ies_lm63_2002 <- function(ld_list, file = "test") {

	C <- . <-  NULL

	# Set up data table
	lum_int_extended_dt <- data.table::setDT(ld_list$lum_int_extended_tbl)

	# Luminous intensity in long table format
	lum_int_extended_long_dt <- data.table::melt(
		data          = lum_int_extended_dt,
		measure.vars  = data.table:::patterns("^C", cols = names(lum_int_extended_dt)),
		variable.name = "C",
		value.name    = "I"
	)

	# Removing prefix from variable "C" names
	lum_int_extended_long_dt[, C := stringr::str_remove(C, "C")]

	# TILT: Marker for end of keywords
	tilt <- "TILT=NONE"

	# * Data descriptions ----
	lamp_no             <- ld_list$lamp_no
	lumens_per_lamp     <- ld_list$lum_flux / lamp_no
	candela_multiplier  <- ld_list$LORL / 100
	angle_vertical_no   <- ld_list$lum_int_extended_tbl %>% nrow()
	angle_horizontal_no <- ld_list$lum_int_extended_tbl %>% dplyr::select(-gamma) %>% ncol()
	photometric_type    <- ld_list$photometry_type

	# units_type: luminous dimensions in feet (1) or in meters (2)
	units_type <- 2
	width_lum  <- ld_list$width_lum
	length_lum <- ld_list$length_lum
	height_lum <- ld_list$height_lum_C0

	ballast_factor   <- ld_list$ballast_factor
	future_use       <- "1"
	input_watts      <- ld_list$power
	angle_vertical   <- ld_list$lum_int_extended_tbl$gamma
	angle_horizontal <- lum_int_extended_long_dt %>%
		dplyr::distinct(C) %>%
		dplyr::pull(C)

	candela_values <- angle_horizontal %>%
		purrr::map_chr( function(x) {
			lum_int_extended_long_dt[C == x, .(I)][[1]] %>%
				stringr::str_c(collapse = " ")
		})

	# * Checks ----

	# Tilted luminaire definition
	# Note: conversion to TILT= INCLUDE is not implemented
	if (ld_list$tilt != 0) tilt <- "TILT=INCLUDE"

	# Absolute photometry
	if (ld_list$lamp_no == "-1") lumens_per_lamp <- lamp_no

	# Dimensions luminous shape

	# Circular or vertical cylindrical
	if (ld_list$width_lum == 0) {
		width_lum  <- -ld_list$length_lum
		length_lum <- -ld_list$length_lum
	}

	# Luminous height definition
	# Not entirely correct solution
	if ( ld_list$height_lum_C0   != 0 | ld_list$height_lum_C90  != 0 |
		 ld_list$height_lum_C180 != 0 | ld_list$height_lum_C270 != 0 ) {
		height_lum <- max(ld_list$height_lum_C0, ld_list$height_lum_C90, ld_list$height_lum_C180, ld_list$height_lum_C270)
	}

	ies_export_chr <- c(

		# First line distinguishes from other ies-formats
		"IESNA:LM-63-2002",

		# * Keywords ----
		stringr::str_c("[TEST]",      ld_list$report_no, sep = " "),
		stringr::str_c("[TESTLAB]",   ld_list$test_lab, sep = " "),
		stringr::str_c("[ISSUEDATE]", Sys.time() %>% format("%Y-%m-%d"), sep = " "),
		stringr::str_c("[MANUFAC]",   ld_list$company, sep = " "),
		stringr::str_c("[LUMCAT]",    ld_list$luminaire_no, sep = " "),
		stringr::str_c("[LUMINAIRE]", ld_list$luminaire_name, sep = " "),
		"[FILEGENINFO] created by R package lighting",

		tilt,
		# End of keywords

		stringr::str_c(lamp_no, lumens_per_lamp, candela_multiplier,
			  angle_vertical_no, angle_horizontal_no, photometric_type,
			  units_type, width_lum, length_lum, height_lum, sep = " "),
		stringr::str_c(ballast_factor, future_use, input_watts, sep = " "),
		stringr::str_c(angle_vertical, collapse = " "),
		stringr::str_c(angle_horizontal, collapse = " "),
		candela_values
	)

	writeLines(ies_export_chr, stringr::str_c(file, ".ies"))

	return(invisible(NULL))
}


# 1.6 LD: UPDATE LD_LIST ----

#' @title Update light distribution list
#'
#' @description `ld_update()` changes configurable parameters in the light
#' distribution data. The principal light distribution is not altered in the process.
#'
#' @param ld_list A specific light distribution list
#' @param file_name Name of LDT-file without extension
#' @param company Company identification/databank/version/format identification
#' @param report_no Measurement report number
#' @param luminaire_name Luminaire name
#' @param luminaire_no Luminaire number
#' @param date_user Date/user
#' @param length Length/diameter of luminaire (mm)
#' @param width b - Width of luminaire (mm) (b = 0 for circular luminaire)
#' @param height Height of luminaire (mm)
#' @param length_lum Length/diameter of luminous area (mm)
#' @param width_lum b1 - Width of luminous area (mm) (b1 = 0 for circular luminous area of luminaire)
#' @param height_lum_C0 Height of luminous area C0-plane (mm)
#' @param height_lum_C90 Height of luminous area C90-plane (mm)
#' @param height_lum_C180 Height of luminous area C180-plane (mm)
#' @param height_lum_C270 Height of luminous area C270-plane (mm)
#' @param lamp_no Number of lamps. For absolute photometry, number is
#'   negative.
#' @param lamp_type Type of lamps
#' @param lum_flux Total luminous flux of lamps (lm). For absolute
#'   photometry, this field is Total Luminous Flux of Luminaire.
#' @param cct Color appearance / color temperature of lamps
#' @param cri Color rendering group / color rendering index
#' @param power Wattage including ballast (W)
#'
#' @returns `ld_update()` returns a light distribution list (ld_list)
#'
#' @examples
#' ld_update(ld_data, file = "test")
#' @export
ld_update <- function(
		ld_list,
		file_name       = NA,
		company         = NA,
		report_no       = NA,
		luminaire_name  = NA,
		luminaire_no    = NA,
		date_user       = NA,
		length          = NA,
		width           = NA,
		height          = NA,
		length_lum      = NA,
		width_lum       = NA,
		height_lum_C0   = NA,
		height_lum_C90  = NA,
		height_lum_C180 = NA,
		height_lum_C270 = NA,
		lamp_no         = NA,
		lamp_type       = NA,
		lum_flux        = NA,
		cct             = NA,
		cri             = NA,
		power           = NA
) {

	if (!is.na(file_name))       ld_list$file_name       <- file_name
	if (!is.na(company))         ld_list$company         <- company
	if (!is.na(report_no))       ld_list$report_no       <- report_no
	if (!is.na(luminaire_name))  ld_list$luminaire_name  <- luminaire_name
	if (!is.na(luminaire_no))    ld_list$luminaire_no    <- luminaire_no
	if (!is.na(date_user))       ld_list$date_user       <- date_user
	if (!is.na(length))          ld_list$length          <- length
	if (!is.na(width))           ld_list$width           <- width
	if (!is.na(height))          ld_list$height          <- height
	if (!is.na(length_lum))      ld_list$length_lum      <- length_lum
	if (!is.na(width_lum))       ld_list$width_lum       <- width_lum
	if (!is.na(height_lum_C0))   ld_list$height_lum_C0   <- height_lum_C0
	if (!is.na(height_lum_C90))  ld_list$height_lum_C90  <- height_lum_C90
	if (!is.na(height_lum_C180)) ld_list$height_lum_C180 <- height_lum_C180
	if (!is.na(height_lum_C270)) ld_list$height_lum_C270 <- height_lum_C270
	if (!is.na(lamp_no))         ld_list$lamp_no         <- lamp_no
	if (!is.na(lamp_type))       ld_list$lamp_type       <- lamp_type
	if (!is.na(lum_flux))        ld_list$lum_flux        <- lum_flux
	if (!is.na(cct))             ld_list$cct             <- cct
	if (!is.na(cri))             ld_list$cri             <- cri
	if (!is.na(power))           ld_list$power           <- power

	return(ld_list)
}
