# LIGHTING VALUES ----

# 1 BASIC ----
# 1.1 Planck law ----

#' @title Creates spectrum of a Planckian radiator
#'
#' @description `plack_law()` computes the spectral intensities of
#' a Planckian radiator with a defined temperature of the black body.
#'
#' @param wl Wavelength vector of Planckian radiator in nm
#' @param temperature A single temperature value of black body radiator in kelvin
#'
#' @return spectral intensities of Planckian radiator
#'
#' @examples
#' # Create wavelength vector
#' wl <- seq(380, 780, 1)
#'
#' planck_law(wl, 2700)
#' planck_law(wl, 6500)
#'
#' @export
planck_law <- function(wl, temperature) {

	# CODATA 2018 from NIST
	c0 <- 299792458      # speed of light in m/s
	h  <- 6.62607015e-34 # Planck constant in Js
	kB <- 1.380649e-23   # Boltzmann constant in J/K

	c1 <- 2 * pi * h * c0^2
	c2 <- h * c0 / kB

	intensity <- c1 / ((wl * 1e-9)^5 * (exp(c2 / (wl * 1e-9 * temperature)) - 1))

	return(intensity)
}

# 1.2 Interpolation ----
#' @title Interpolating spectrum using the Sprague method
#'
#' @description `interpolate_sprague()` performs a special interpolation for a
#' spectrum with equidistant data points. interpolation of defined
#' spectrum data points.
#'
#' @details This function is originally based on a Matlab code \insertCite{Westland2012}{lighting}
#' and was adapted to R. Sampling rate must be at least doubled, i. e. f \eqn{\ge}2.
#' And f must be nominal (2, 3, ...). #' For more insight of the sprague
#' interpolation method see \insertCite{Westland2015}{lighting}.
#'
#' @param y Numerical spectral data vector to be interpolated
#' @param f Interpolation factor. Doubling the sampling rate with f = 2.
#'
#' @return A vector of interpolated data points using the Sprague method.
#'
#' @references
#'     \insertAllCited{}
#'
#' @examples
#' spectra_tbl <- tibble::tibble(wavelength = seq(380, 780, 5)) %>%
#' 	dplyr::mutate(p2700K = planck_law(wavelength, 2700)) %>%
#' 	dplyr::mutate(p5000K = planck_law(wavelength, 5000))
#'
#' # interpolation to wavelength range seq(380, 780, 5)
#' interpolate_sprague(spectra_tbl$p2700K, 5)
#'
#' @export
interpolate_sprague <- function(y, f) {
	# adapted sprague interpolation from Westland 2012 Matlab Code
	# f is an interpolation factor
	# e.g. if f = 2 the sampling rate is doubled
	if (f < 2 | ((f  - floor(f)) > 0)) {
		stop("invalid f value - premature termination.")
	}
	# set the parameters
	c1 <- matrix(
		c(
			 884, -1960, 3033, -2648,  1080, -180,   508, -540,
			 488,  -367,  144,   -24,   -24,  144,  -367,  488,
			-540,   508, -180,  1080, -2648, 3033, -1960,  884
		),
		nrow  = 4,
		byrow = TRUE
	)

	y_length <- length(y)

	# select a spectrum
	r    <- y
	# add the extra start and end points
	k    <- c1[1, ]
	p1   <- (k %*% r[1:6]) / 209
	k    <- c1[2, ]
	p2   <- (k %*% r[1:6]) / 209
	k    <- c1[3, ]
	endV <- length(r)
	p3   <- (k %*% r[(endV-5):endV]) / 209
	k    <- c1[4, ]
	p4   <- (k %*% r[(endV-5):endV]) / 209
	r    <- c(p1, p2, r, p3, p4)
	N    <- y_length + 4

	p    <- matrix(0,  f * (N - 5) + 1, 1)
	xx   <- seq(1 / f, 1 - 1 / f, len = f - 1)

	for (j in 3:(N-3)) {
		a0 <- r[j]
		a1 <- ( 2*r[j-2]-16*r[j-1]+ 16*r[j+1]-  2*r[j+2])/24
		a2 <- (  -r[j-2]+16*r[j-1]- 30*r[j]  + 16*r[j+1]-   r[j+2])/24
		a3 <- (-9*r[j-2]+39*r[j-1]- 70*r[j]  + 66*r[j+1]-33*r[j+2]+ 7*r[j+3])/24
		a4 <- (13*r[j-2]-64*r[j-1]+126*r[j]  -124*r[j+1]+61*r[j+2]-12*r[j+3])/24
		a5 <- (-5*r[j-2]+25*r[j-1]- 50*r[j]  + 50*r[j+1]-25*r[j+2]+ 5*r[j+3])/24
		yy <- a0 + a1 * xx + a2 * xx^2 + a3 * xx^3 + a4 * xx^4 + a5 * xx^5
		index <- j - 2
		p[ (index - 1) * f + 1                                   , 1] <- r[j]
		p[((index - 1) * f + 1 + 1):((index - 1) * f + 1 + f - 1), 1] <- yy
	}
	p[f*(N-5) + 1, 1] <- r[N-2]

	p <- as.vector(p)

	return(p)
}

#' Wrapper function to get only y-value of approx function
#' @noRd
approx_wrapper <- function(...) {
	l <- stats::approx(...)
	res <- l$y
	return(res)
}

#' Wrapper function to get only y-value of spline function
#' @noRd
spline_wrapper <- function(...) {
	l <- stats::spline(...)
	res <- l$y
	return(res)
}

#' Interpolates a dataframe of spectra with a wavelength column.
#'
#' This function provides the all recommended interpolation method for spectra.
#' This includes linear, spline and sprague interpolation.
#'
#' In Details - Not tested for wavelength interval < 1 nm.
#' Linear interpolating: extrapolation results into 0 values.
#'
#' @param spectra is a dataframe of spectra in wide table format including
#'   a wavelength column in nm.
#' @param wl_out defines the output wavelength range and interval in nm for
#'   interpolation.
#'   Default: \code{wl_out = seq(380, 780, 5)}
#' @param str_wavelength Define name of wavelength column in \code{spectra}
#'   dataframe. Default: \code{str_wavelength = NULL}. If entry is \code{NULL},
#'   then the name of the first column from dataframe \code{spectra} will
#'   chosen as \code{str_wavelength}.
#' @param method specifies the interpolation method of the functions
#'   \link[stats]{approx} and \link[stats]{spline}. Default:
#'   \code{"linear"}. Spline interpolation: \code{"fmm"}, \code{"periodic"},
#'   \code{"natural"}, \code{"monoH.FC"} and \code{"hyman"}. Sprague
#'   interpolation after \insertCite{Westland2015}{lighting}: \code{"sprague"}
#' @param tolerance used for sprague interpolation wrapper to correct numerical
#'   differentiation errors. Important to calculate the interpolation factor f.
#'   Default: 1e-14.
#'
#' @return returns a dataframe of interpolated spectra in wide table format
#'   specified by \code{wl_out}.
#'
#' @author Dr. William Truong
#'
#' @examples
#' # Create spectrum planckian radiator using black body temperature in K
#' wavelength <- seq(380, 780, 5)
#' planck2700 <- planck_law(seq(380, 780, 5), 2700)
#' planck5000 <- planck_law(seq(380, 780, 5), 5000)
#'
#' spectra <- tibble::tibble(wavelength, planck2700, planck5000)
#' interpolate_spectra(spectra, seq(380, 780, 1), method = "linear")
#' interpolate_spectra(spectra, seq(380, 780, 1), method = "fmm")
#' interpolate_spectra(spectra, seq(400, 700, 1), method = "sprague")
#' @export
interpolate_spectra <- function(
		spectra,
		wl_out         = seq(380, 780, 5),
		method         = "linear",
		str_wavelength = NULL,
		tolerance      = 1e-14
) {

	# defining first column of dataframe as str_wavelength
	if (is.null(str_wavelength)) {
		str_wavelength <- colnames(spectra)[1]
	}
	wl <- spectra[[str_wavelength]]

	# change string to variable name
	var_wl <- as.name(str_wavelength)

	# linear interpolation using stats::approx()
	if (method == "linear") {
		spectra_interpolated <- spectra %>%
			dplyr::select(-var_wl) %>%
			purrr::map(
				approx_wrapper,
				x      = wl,
				xout   = wl_out,
				method = method,
				yleft  = 0,
				yright = 0
			) %>%
			tibble::as_tibble()
		spectra_interpolated <- cbind(wl_out, spectra_interpolated) %>%
			dplyr::rename(!!str_wavelength := wl_out)
	}

	# spline interpolation using stats::spline()
	if (method == "fmm"      |
		method == "periodic" |
		method == "natural"  |
		method == "monoH.FC" |
		method == "hyman") {
		spectra_interpolated <- spectra %>%
			dplyr::select(-var_wl) %>%
			purrr::map(
				spline_wrapper,
				x      = wl,
				xout   = wl_out,
				method = method
			) %>%
			tibble::as_tibble()
		spectra_interpolated <- cbind(wl_out, spectra_interpolated) %>%
			dplyr::rename(!!str_wavelength := wl_out)
	}

	# sprague interpolation wrapper for interpolate_sprague()
	if (method == "sprague") {

		# checking wl_output range
		if (min(wl) > min(wl_out) | max(wl) < max(wl_out)) {
			stop("No extrapolation with Sprague method. Check wl_out.")
		}

		# checking if wavelength array ist equidistant
		chk_diff <- abs(min(diff(wl)) - max(diff(wl)))
		if (chk_diff > tolerance) {
			stop("Wavelength values are not equidistant.")
		} else {
			# correcting numerical difference error
			wl_diff <- unique(signif(diff(wl), 1))
		}
		# checking if wavelength array ist equidistant
		chk_diff <- abs(min(diff(wl_out)) - max(diff(wl_out)))
		if (chk_diff > tolerance) {
			stop("Wavelength_out values are not equidistant.")
		} else {
			# correcting numerical difference error
			wl_out_diff <- unique(signif(diff(wl_out), 1))
		}

		f <- wl_diff / wl_out_diff # calculation of interpolation factor

		if (f != 1) {
			spectra_interpolated <- spectra %>%
				dplyr::filter(min(wl_out) <= eval(var_wl) & eval(var_wl) <= max(wl_out)) %>%
				dplyr::select(-var_wl) %>%
				purrr::map(
					interpolate_sprague,
					f = f
				) %>%
				tibble::as_tibble()
			spectra_interpolated <- cbind(wl_out, spectra_interpolated) %>%
				dplyr::rename(!!str_wavelength := wl_out)
		} else {
			spectra_interpolated <- spectra %>%
				dplyr::filter(min(wl_out) <= eval(var_wl) & eval(var_wl) <= max(wl_out))
		}
	}

	return(spectra_interpolated)
}

# 1.3 Integration ----
#' Integrates numerical a dataframe of spectra
#'
#' \loadmathjax
#' Typical numerical integration of spectrum to compute lighting values
#' \mjeqn{LV}{ascii} using the trapezoidal method. Formula:
#' \mjdeqn{LV = K \cdot \int E_e(\lambda) s(\lambda) d\lambda
#' }{ascii}
#'
#'
#' @param spectrum \mjeqn{E_e(\lambda)}{ascii} is the spectrum to be integrated.
#' @param wavelength \mjeqn{\lambda}{ascii} defines the integration wavelength
#' range.
#' @param sensitivity \mjeqn{s(\lambda)}{ascii} defines the sensitivity
#' function, which is multiplied with the spectrum \mjeqn{E_e(\lambda)}{ascii}
#' before integration. Default: 1.
#' @param constant \mjeqn{K}{ascii} is a constant. Mostly used for
#' standardisation. Default: 1.
#'
#' @return an integrated value after the formula in the description.
#'
#'
#' @examples
#' # calculating photopic quantitiy illuminance
#' K_m <- 683.002 # in lm/W constant for self-luminous object
#' ssf <- cvrl.org %>%
#'   dplyr::filter(380 <= nm & nm <= 780)
#' V_pho <- ssf$V_pho
#' wl <- ees <- seq(380, 780, 1)
#' ees[] <- 1
#' integrate_spectrum(ees, wl, V_pho, K_m)
#' @export
integrate_spectrum <- function(
		spectrum,
		wavelength,
		sensitivity = 1,
		constant    = 1
) {
	x <- wavelength
	y <- spectrum * sensitivity
	n <- length(y)
	# correction term at the boundary see pracma::trapz
	h  <-  x[2] - x[1]
	ca <- (y[2] - y[1])   / h
	cb <- (y[n] - y[n-1]) / h

	value <- constant * (pracma::trapz(x, y) - h^2/12 * (cb - ca))
	return(value)
}

# 2 COLORIMETRY ----
# 2.0 Sensitivity functions ----
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

	df_cie1995_13.3  <- cie1995_13.3
	df_cie2018_015   <- cie2018_015
	df_cie2018_S_026 <- cie2018_S_026
	df_cvrl.org      <- cvrl.org
	df_lucas2014     <- lucas2014
	df_rea2010       <- rea2010
	df_rea2018       <- rea2018

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
		df_cie1995_13.3  <- interpolate_1nm(df_cie1995_13.3, "linear")
		df_cie2018_015   <- interpolate_1nm(df_cie2018_015, "linear")
		df_cie2018_S_026 <- interpolate_1nm(df_cie2018_S_026, "sprague")
		df_cvrl.org      <- interpolate_1nm(df_cvrl.org, "sprague")
		df_lucas2014     <- interpolate_1nm(df_lucas2014, "sprague")
		df_rea2010       <- interpolate_1nm(df_rea2010, "sprague")
		df_rea2018       <- interpolate_1nm(df_rea2018, "sprague")

		# Converting sensitivity function to specified wavelength step
		df_cie1995_13.3  <- interpolate_spectra(df_cie1995_13.3, wl_out, "linear")
		df_cie2018_015   <- interpolate_spectra(df_cie2018_015, wl_out, "linear")
		df_cie2018_S_026 <- interpolate_spectra(df_cie2018_S_026, wl_out, "linear")
		df_cvrl.org      <- interpolate_spectra(df_cvrl.org, wl_out, "linear")
		df_lucas2014     <- interpolate_spectra(df_lucas2014, wl_out, "linear")
		df_rea2010       <- interpolate_spectra(df_rea2010, wl_out, "linear")
		df_rea2018       <- interpolate_spectra(df_rea2018, wl_out, "linear")
	}

	# Create named list with specific spectral sensitivity functions
	sensitivity <- list(
		cie1995_13.3  = df_cie1995_13.3,
		cie2018_015   = df_cie2018_015,
		cie2018_S_026 = df_cie2018_S_026,
		cvrl.org      = df_cvrl.org,
		lucas2014     = df_lucas2014,
		rea2010       = df_rea2010,
		rea2018       = df_rea2018
	)

	return(sensitivity)
}



# 2.1 CRI and CCT ----

#' Compute CIE-XYZ-1931 color space values
#'
#' @param spectrum spectral power distribution of light source per wavelength
#' @param wavelength array in nm corresponding to spectrum
#' @param cmf2 spectral sensitivity of CIE 2° colour matching functions
#' @param K constant for standardization
#' @param R spectral reflection factor
#'
#' @noRd
compute_XYZ_CIE1931 <- function(spectrum, wavelength, cmf2, K, R) {
	wl_diff <- mean(diff(wavelength))
	X_2 <- K * sum(spectrum * R * cmf2$x_cmf_2) * wl_diff
	Y_2 <- K * sum(spectrum * R * cmf2$y_cmf_2) * wl_diff
	Z_2 <- K * sum(spectrum * R * cmf2$z_cmf_2) * wl_diff

	x_2 <- X_2 / (X_2 + Y_2 + Z_2)
	y_2 <- Y_2 / (X_2 + Y_2 + Z_2)
	z_2 <- Z_2 / (X_2 + Y_2 + Z_2)

	df <- data.frame(x_2, y_2, z_2, X_2, Y_2, Z_2)
	return(df)
}

#' Compute CIE-UCS-1976 color space values
#'
#' @param X_2 2°-tristimulus value
#' @param Y_2 2°-tristimulus value
#' @param Z_2 2°-tristimulus value
#'
#' @noRd
compute_UCS_CIE1976 <- function(X_2, Y_2, Z_2) {
	u_prime = 4 * X_2 / (X_2 + 15 * Y_2 + 3 * Z_2)
	v_prime = 9 * Y_2 / (X_2 + 15 * Y_2 + 3 * Z_2)
	#w_prime = 1 - u_prime - v_prime
	df <- data.frame(u_prime, v_prime)
	return(df)
}

#' Defines current colour distance of light source to planckian radiator
#'
#' @param current_CCT current correlated colour temperature
#' @param wavelength array in nm
#' @param u_prime light source colour chromaticity CIE-UCS-1976
#' @param v_prime light source colour chromaticity CIE-UCS-1976
#' @param cmf2 spectral sensitivity of colour matching functions
#' @param Km luminous efficacy in lm/W
#'
#' @noRd
find_CCT <- function(current_CCT, wavelength, u_prime, v_prime, cmf2, Km) {
	spectrum_P <- planck_law(current_CCT, wavelength)

	XYZ_P <- compute_XYZ_CIE1931(spectrum_P, wavelength, cmf2, Km, 1)
	uv_P  <- compute_UCS_CIE1976(XYZ_P$X_2, XYZ_P$Y_2, XYZ_P$Z_2)

	delta_uv <- sqrt((u_prime - uv_P$u_prime)^2 + 4 / 9
					 * (v_prime - uv_P$v_prime)^2)
	return(delta_uv)
}

#' Applying von Kries colour shift transformation
#'
#' @param u chromaticty coordinate CIE-UCS-1960
#' @param v chromaticty coordinate CIE-UCS-1960
#'
#' @noRd
apply_von_Kries_color_shift <- function(u, v) {
	c = (4 - u - 10 * v) / v
	d = (1.708 * v + 0.404 - 1.481 * u) / v
	df <- data.frame(c, d)
	return(df)
}

#' Determine CIE 1964 WUV values
#'
#' @param Y tristimulus value CIE-XYZ-1931
#' @param u test light source chromaticty coordinate CIE-UCS-1960
#' @param v test light source chromaticty coordinate CIE-UCS-1960
#' @param u_r reference light source chromaticty coordinate CIE-UCS-1960
#' @param v_r reference light source chromaticty coordinate CIE-UCS-1960
#'
#' @noRd
compute_WUV_CIE1964 <- function(Y, u, v, u_r, v_r) {
	W <- 25 * Y^(1 / 3) - 17
	U <- 13 * W * (u - u_r)
	V <- 13 * W * (v - v_r)
	df <- data.frame(W, U, V)
	return(df)
}

#' Computes CCT, xyzXYZ, uv and colour rendering indexes
#'
#' @param spectrum spectral power distribution of light source per wavelength
#' @param wavelength array in nm corresponding to spectrum
#' @param cmf2 spectral sensitivity of CIE 2° colour matching functions
#' @param Km luminous efficacy in lm/W
#' @param R spectral reflection factor
#' @param S daylight components S_0, S_1 and S_2
#' @param TCS CIE test color samples 1 - 14 CIE 1995
#'
#' @noRd
compute_colour_rendering <- function(
		spectrum,
		wavelength,
		cmf2,
		Km = 683.002,
		R  = 1,
		S,
		TCS
) {
	# define some local variables
	nm <- u_prime <- v_prime <- NULL

	# step 1 defining CCT ----
	xyzXYZ <- compute_XYZ_CIE1931(spectrum, wavelength, cmf2, Km, R)
	uv     <- compute_UCS_CIE1976(xyzXYZ$X_2, xyzXYZ$Y_2, xyzXYZ$Z_2)

	# Defining optimisation interval using McCamy Method
	n <- (xyzXYZ$x_2 - 0.3320) / (xyzXYZ$y_2 - 0.1858)
	current_CCT <- -449 * n^3 + 3525 * n^2 - 6823.3 * n + 5520.33
	CCTmin <- current_CCT - 500
	CCTmax <- current_CCT + 500

	result <- stats::optimize(
		find_CCT,
		wavelength = wavelength,
		u_prime    = uv$u_prime,
		v_prime    = uv$v_prime,
		cmf2       = cmf2,
		Km         = Km,
		interval   = c(CCTmin, CCTmax)
	)
	CCT <- result$minimum

	# compute color rendering index after ASSIST recommends 2010
	# step 2 ----
	if (CCT < 5000) {
		spectrum_ref <- planck_law(CCT, wavelength)
	} else if (CCT >= 5000 & CCT <= 7000) {
		xD <- -4.6070e9 / CCT^3 + 2.9678e6 / CCT^2 + 0.09911e3 / CCT + 0.244063
	} else if (CCT > 7000 & CCT <= 25000) {
		xD <- -2.0064e9 / CCT^3 + 1.9018e6 / CCT^2 + 0.24748e3 / CCT + 0.237040
	}
	if (CCT >= 5000 & CCT <= 25000) {
		yD <- -3.0000 * xD^2 + 2.870 * xD - 0.275
		M1 <- (-1.3515 -  1.7703 * xD +  5.9114 * yD) /
			( 0.0241 +  0.2562 * xD -  0.7341 * yD)
		M2 <- ( 0.0300 - 31.4424 * xD + 30.0717 * yD) /
			( 0.0241 +  0.2562 * xD -  0.7341 * yD)
		spectrum_ref <- S$S_0 + M1 * S$S_1 + M2 * S$S_2
	}
	spectrum_ref <- (spectrum_ref ) /  max(spectrum_ref)
	wl_diff <- mean(diff(wavelength))

	K_k <- 100 / sum(spectrum     * R * cmf2$y_cmf_2) / wl_diff
	K_r <- 100 / sum(spectrum_ref * R * cmf2$y_cmf_2) / wl_diff

	# Remark: Using K = 1 leads to results in ASSIST recommend (ees p.13) (p.14)
	xyzXYZ_k <- compute_XYZ_CIE1931(spectrum    , wavelength, cmf2, K_k, R)
	xyzXYZ_r <- compute_XYZ_CIE1931(spectrum_ref, wavelength, cmf2, K_r, R)

	# step 3 ----
	## step 3a
	xyzXYZ_ki <- TCS %>%
		dplyr::select(-nm) %>%
		purrr::map_df(
			compute_XYZ_CIE1931,
			spectrum   = spectrum,
			wavelength = wavelength,
			cmf2       = cmf2,
			K          = K_k # K = 1 for values as in Assist recommends document
		)
	xyzXYZ_ri <- TCS %>%
		dplyr::select(-nm) %>%
		purrr::map_df(
			compute_XYZ_CIE1931,
			spectrum   = spectrum_ref,
			wavelength = wavelength,
			cmf2       = cmf2,
			K          = K_r # K = 1 for values as in Assist recommends document
		)
	## step 3b
	uv_k <- compute_UCS_CIE1976(xyzXYZ_k$X_2, xyzXYZ_k$Y_2, xyzXYZ_k$Z_2) %>%
		dplyr::rename(u = u_prime, v = v_prime)
	uv_r <- compute_UCS_CIE1976(xyzXYZ_r$X_2, xyzXYZ_r$Y_2, xyzXYZ_r$Z_2) %>%
		dplyr::rename(u = u_prime, v = v_prime)
	uv_ki <- compute_UCS_CIE1976(xyzXYZ_ki$X_2, xyzXYZ_ki$Y_2, xyzXYZ_ki$Z_2) %>%
		dplyr::rename(u = u_prime, v = v_prime)
	uv_ri <- compute_UCS_CIE1976(xyzXYZ_ri$X_2, xyzXYZ_ri$Y_2, xyzXYZ_ri$Z_2) %>%
		dplyr::rename(u = u_prime, v = v_prime)

	# transform to CIE-UCS-1960
	uv_k$v <- 2 / 3 * uv_k$v
	uv_r$v <- 2 / 3 * uv_r$v
	uv_ki$v <- 2 / 3 * uv_ki$v
	uv_ri$v <- 2 / 3 * uv_ri$v

	# step 4 von Kries color shift ----
	cd_k <- apply_von_Kries_color_shift(uv_k$u, uv_k$v)
	cd_r <- apply_von_Kries_color_shift(uv_r$u, uv_r$v)
	cd_ki <- apply_von_Kries_color_shift(uv_ki$u, uv_ki$v)

	uv_ki$u <- (10.872 + 0.404 * cd_r$c / cd_k$c * cd_ki$c - 4 * cd_r$d /
					cd_k$d * cd_ki$d) /
		(16.518 + 1.481 * cd_r$c / cd_k$c * cd_ki$c -     cd_r$d /
		 	cd_k$d * cd_ki$d)
	uv_ki$v <-   5.520 /
		(16.518 + 1.481 * cd_r$c / cd_k$c * cd_ki$c -     cd_r$d /
		 	cd_k$d * cd_ki$d)

	# step 5 Determine CIE 1964 W*U*V* values ----
	WUV_ki <- compute_WUV_CIE1964(xyzXYZ_ki$Y_2, uv_ki$u, uv_ki$v, uv_r$u, uv_r$v)
	WUV_ri <- compute_WUV_CIE1964(xyzXYZ_ri$Y_2, uv_ri$u, uv_ri$v, uv_r$u, uv_r$v)

	dE <- sqrt((WUV_ri$U - WUV_ki$U)^2 + (WUV_ri$V - WUV_ki$V)^2 +
			   	(WUV_ri$W - WUV_ki$W)^2)

	R_i <- 100 - 4.6 * dE

	R_a <- sum(R_i[1:8]) / 8

	# Creating dataframes
	df_Ra <- data.frame(R_a, t(R_i)) %>%
		dplyr::rename_with(~ c("R_a", "R_1", "R_2", "R_3", "R_4", "R_5", "R_6", "R_7",
						"R_8", "R_9", "R_10", "R_11", "R_12", "R_13", "R_14"))

	df <- data.frame(CCT) %>%
		cbind(df_Ra) %>%
		cbind(xyzXYZ) %>%
		cbind(uv)

	return(df)
}

# 2.2 Non-visual quantities ----
#' Computes nonvisual quantities defined by CIE, DIN and IWBI
#'
#' @param spectrum spectral power distribution
#' @param wavelength array in nm
#' @param V_pho photopic luminosity function
#' @param K_m luminous efficacy
#' @param s_alpha spectral sensitivty functions for alpha-values
#'
#' @return nonvisual quantities
#'
#' @noRd
compute_nonvisual_standards <- function(
		spectrum,
		wavelength,
		V_pho,
		K_m,
		s_alpha
) {

	E_v <- integrate_spectrum(spectrum, wavelength, V_pho, K_m)

	E_alpha <- purrr::map(
		s_alpha,
		integrate_spectrum,
		spectrum   = spectrum,
		wavelength = wavelength,
		constant   = 1
	) %>%
		as.data.frame()

	# alpha-opic daylight (D65) efficacy ratios
	K_D65_alpha <- c(0.8731e-3, 1.4558e-3, 1.6289e-3, 1.4497e-3, 1.3262e-3)

	gamma_D65_alpha <- E_alpha / E_v / K_D65_alpha
	a_mel           <- gamma_D65_alpha[5] / 1.104
	R_mel_ratio     <- gamma_D65_alpha[5] / 0.9101

	E_D65_alpha <- E_v * gamma_D65_alpha
	E_D65_v_mel <- E_D65_alpha[5]
	EML         <- E_D65_alpha[5] / 0.9101

	df <- data.frame(
		E_v,
		E_D65_alpha,
		gamma_D65_alpha,
		E_D65_v_mel,
		a_mel,
		EML,
		R_mel_ratio
	) %>%
		dplyr::rename_with(~ c(
			"E_v",
			"E^D65_sc,v",
			"E^D65_mc,v",
			"E^D65_lc,v",
			"E^D65_rh,v",
			"E^D65_mel,v",
			"gamma^D65_sc,v",
			"gamma^D65_mc,v",
			"gamma^D65_lc,v",
			"gamma^D65_rh,v",
			"gamma^D65_mel,v",
			"E_v,mel,D65",
			"a_mel,v",
			"EML",
			"R_mel,ratio"
		))
	return(df)
}

#' Compute alpha-opic values after Lucas et al.
#'
#' @param spectrum spectral data
#' @param wavelength array in nm
#' @param N_opsin spectral sensitivity functions
#'
#' @return alpha-opic values
#'
#' @noRd
compute_alpha_opic <- function(
		spectrum,
		wavelength,
		N_opsin
) {
	df <- N_opsin %>%
		purrr::map(
			integrate_spectrum,
			spectrum   = spectrum,
			wavelength = wavelength,
			constant   = 72983.25
		) %>%
		as.data.frame() %>%
		dplyr::rename_with(~ c(
			"E_sc",
			"E_mc",
			"E_lc",
			"E_r",
			"E_z"
		))
	return(df)
}


#' Computes the Circadian Stimulus from Rea et al.
#'
#' @param spectrum spectral power distribution
#' @param wavelength array in nm
#' @param ssf spectral sensitivity functions
#'
#' @return Circadian Stimulus
#'
#' @noRd
compute_circadian_stimulus <- function (
		spectrum,
		wavelength,
		ssf
) {

	V_sco    <- ssf$V_sco
	V_lbd_mp <- ssf$V_pho_mac
	S_lbd_mp <- ssf$S_sc_mac
	Mc       <- ssf$Mc_lens

	rodSat <- 6.5
	k      <- 0.2616
	ab_y   <- 0.700
	arod   <- 3.3

	M <- integrate_spectrum(spectrum, wavelength, Mc, 1)
	SV <- integrate_spectrum(spectrum, wavelength, S_lbd_mp, 1) -
		integrate_spectrum(spectrum, wavelength, V_lbd_mp, k)
	VS <- integrate_spectrum(spectrum, wavelength, V_sco, 1)

	if (SV > 0) {
		CL_A <- 1548 * (M + ab_y * SV - arod * (1 - exp(-VS / rodSat)))
	} else if (SV <= 0) {
		CL_A <- 1548 * M
	}

	CS <- 0.7 - 0.7 / (1 + (CL_A / 355.7)^1.1026)

	df <- data.frame(CS, CL_A, SV)
	return(df)
}

# 2.3 Wrapper for computing lighting quantities ----
#' Computes lighting values from given spectra dataframe
#'
#' The following lighting values are implemented:
#' \itemize{
#'   \item{E_v: }{illuminance in lx}
#'   \item{E^D65_sc,v: }{S-cone-opic equivalent daylight (D65) illuminance in lx
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{E^D65_mc,v: }{M-cone-opic equivalent daylight (D65) illuminance in lx
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{E^D65_lc,v: }{L-cone-opic equivalent daylight (D65) illuminance in lx
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{E^D65_rh,v: }{rhodopic equivalent daylight (D65) illuminance in lx
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{E^D65_mel,v: }{melanopic equivalent daylight (D65) illuminance in lx
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{gamma^D65_sc,v: }{S-cone-opic daylight (D65) efficacy ratio
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{gamma^D65_mc,v: }{M-cone-opic daylight (D65) efficacy ratio
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{gamma^D65_lc,v: }{L-cone-opic daylight (D65) efficacy ratio
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{gamma^D65_rh,v: }{rhodopic daylight (D65) efficacy ratio
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{gamma^D65_mel,v: }{melanopic daylight (D65) efficacy ratio
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{E_v,mel,D65: }{melanopic daylight equivalent illuminance  in lx
#'   \insertCite{DIN2015}{lighting}}
#'   \item{a_mel,v: }{melanopic factor of luminous radiation
#'   \insertCite{DIN2015}{lighting}}
#'   \item{EML: }{equivalent melanopic lux \insertCite{IWBI2019}{lighting}}
#'   \item{R_mel,ratio: }{melanopic ratio \insertCite{IWBI2019}{lighting}}
#'   \item{CCT: }{correlated colour temperature \insertCite{CIE2018b}{lighting}}
#'   \item{R_a: }{color rendering index \insertCite{CIE1995}{lighting}}
#'   \item{R_i: }{1-14 specific color rendering index
#'   \insertCite{CIE1995}{lighting}}
#'   \item{x_2: }{chromaticity coordinate CIE-XYZ-1931, 2° standard observer
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{y_2: }{chromaticity coordinate CIE-XYZ-1931, 2° standard observer
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{z_2: }{chromaticity coordinate CIE-XYZ-1931, 2° standard observer
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{X_2: }{tristimulus value CIE-XYZ-1931, 2° standard observer
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{Y_2: }{tristimulus value CIE-XYZ-1931, 2° standard observer
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{Z_2: }{tristimulus value CIE-XYZ-1931, 2° standard observer
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{u_prime: }{chromaticity coordinate CIE-UCS-1976
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{v_prime: }{chromaticity coordinate CIE-UCS-1976
#'   \insertCite{CIE2018b}{lighting}}
#'   \item{E_sc: }{cyanopic illuminance \insertCite{Lucas2014}{lighting}}
#'   \item{E_mc: }{chloropic illuminance \insertCite{Lucas2014}{lighting}}
#'   \item{E_lc: }{erythropic illuminance \insertCite{Lucas2014}{lighting}}
#'   \item{E_r: }{rhodopic illuminance \insertCite{Lucas2014}{lighting}}
#'   \item{E_z: }{melanopic illuminance \insertCite{Lucas2014}{lighting}}
#'   \item{CS: }{circadian stimulus \insertCite{Rea2018}{lighting}}
#'   \item{CL_A: }{circadian light \insertCite{Rea2018}{lighting}}
#'   \item{SV: }{blue-yellow spectral opponency \insertCite{Rea2018}{lighting}}
#' }
#'
#'
#' @param spectra dataframe of spectra with one wavelength column in nm
#' @param str_wavelength name of wavelength column. Default is NULL: The first
#' column is the wavelength column
#'
#' @return dataframe of lighting values used in lighting.
#' @export
#'
#' @examples
#' wavelength <- seq(380, 780, 1)
#' P2700 <- planck_law(2700, wavelength)
#' P6500 <- planck_law(6500, wavelength)
#'
#' spectra <- data.frame(wavelength, P2700, P6500)
#' spectra$EES <- 1
#'
#' compute_lighting(spectra)
#' compute_lighting(spectra, "wavelength")
#'
#' @references
#'     \insertAllCited{}
#'
#' @importFrom dplyr all_of
compute_lighting <- function(
		spectra,
		str_wavelength = NULL
) {

	# defining local variables
	x_cmf_2 <- y_cmf_2 <- z_cmf_2 <- nm <- T_lens <- NULL

	# defining first column of dataframe as str_wavelength
	if (is.null(str_wavelength)) {
		str_wavelength <- colnames(spectra)[1]
	}
	wavelength     <- spectra[[str_wavelength]]
	spectra        <- dplyr::select(spectra, -all_of(str_wavelength))
	lightsource    <- colnames(spectra)

	# defining constants
	K_m     <- 683.002 # in lm/W
	# loading and interpolating spectral sensitvity functions to given wavelength
	ssf     <- sensitivity_functions(wavelength)
	V_pho   <- ssf$cvrl.org$V_pho
	cmf2    <- dplyr::select(ssf$cvrl.org, c(x_cmf_2, y_cmf_2, z_cmf_2))
	S       <- ssf$cie2018_015
	TCS     <- ssf$cie1995_13.3
	s_alpha <- dplyr::select(ssf$cie2018_S_026, -nm)
	N_opsin <- dplyr::select(ssf$lucas2014, -c(nm, V_pho, T_lens))


	# creating data frame lighting values
	lv <- data.frame()[1:length(lightsource),] %>%
		`rownames<-`(lightsource)


	# illuminance computation
	df <- spectra %>%
		purrr::map_df(
			compute_nonvisual_standards,
			wavelength = wavelength,
			V_pho      = V_pho,
			K_m        = K_m,
			s_alpha    = s_alpha
		)

	lv <- cbind(lv, df)

	# compute colour rendering indexes including CCT, xyzXYZ, uv
	df <- spectra %>%
		purrr::map_df(
			compute_colour_rendering,
			wavelength = wavelength,
			cmf2       = cmf2,
			Km         = K_m,
			R          = 1,
			S          = S,
			TCS
		)

	lv <- cbind(lv, df)

	# alpha-opic illuminances after Lucas et al. 2014
	df <- spectra %>%
		purrr::map_df(
			compute_alpha_opic,
			wavelength = wavelength,
			N_opsin    = N_opsin
		)
	lv <- cbind(lv, df)

	# Circadian Stimulus after Rea et al. 2018
	df <- spectra %>%
		purrr::map_df(
			compute_circadian_stimulus,
			wavelength = wavelength,
			ssf = ssf$rea2018
		)
	lv <- cbind(lv, df)

	return(lv)
}
