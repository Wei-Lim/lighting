# LIGHTING DISTRIBUTION ----

# 1.0 EXTRACTING LDT ----

#' Extracting Luminous Intensity LDT Data
#'
#' @description
#'
#' Extracts luminous intensity from LDT-file. It depends on the number of
#' luminous intensities per C-plane and per gamma angle. Internal private
#' function for iteration purposes.
#'
#' @param C A single C-plane value.
#' @param i Counter for the unique C-plane values. Used for extracting the right lines.
#' @param gamma A vector of gamma angles. Length corresponds to the number of luminous intensity per gamma angle.
#' @param Ng Number of luminous intensities in each C-plane.
#' @param lines_data Lines with luminous intensity data from the LDT file.
#'
extract_lum_intensity_ldt <-
	function(C, i, gamma, Ng, lines_data) {

		I   <- lines_data[((i - 1) * Ng + 1):(i * Ng)] %>% as.numeric()

		# Creates tibble dataframe
		tbl <- tibble::tibble(C, gamma, I)

		return(tbl)
	}
