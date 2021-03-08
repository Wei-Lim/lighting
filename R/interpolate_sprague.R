#' Interpolating using the Sprague method
#'
#' Perform sprague interpolation of given spectrum data points and returning a
#' list of points obtained by interpolation. The original Matlab code by
#' \insertCite{Westland2012}{lighting} was adapted to R. Sampling rate must be
#' at least doubled, i. e. f \eqn{\ge}2. And f must be nominal (2, 3, ...).
#' For more insight of the sprague interpolation method see
#' \insertCite{Westland2015}{lighting}.
#'
#' @param y numeric spectral data y input to be interpolated
#' @param f interpolation factor. Doubled sampling rate (f = 2).
#'
#' @return returns interpolated data points using the sprague method.
#' @export
#'
#' @examples
#' # Creating y-function as an alternative vor spectral data
#' x <- seq(-100, 100, 2)
#' y <- x^3
#' x_out <- seq(-100, 100, 0.5)
#' f <- 4 # -> steps: 2 / 0.5
#'
#' interpolate_sprague(y, f)
#'
#' @references
#'     \insertAllCited{}
#'
#' @importFrom Rdpack reprompt
interpolate_sprague <- function(y, f) {
  # adapted sprague interpolation from Westland 2012 Matlab Code
  # f is an interpolation factor
  # e.g. if f = 2 the sampling rate is doubled
  if (f < 2 | ((f  - floor(f)) > 0)) {
    stop("invalid f value - premature termination.")
  }
  # set the parameters
  c1 <- matrix(c(  884, -1960, 3033, -2648,  1080, -180,   508, -540,
                   488,  -367,  144,   -24,   -24,  144,  -367,  488,
                  -540,   508, -180,  1080, -2648, 3033, -1960,  884
                ),
               nrow = 4,
               byrow = TRUE
               )

  y_length <- length(y)

  # select a spectrum
  r <- y
  # add the extra start and end points
  k <- c1[1, ]
  p1 <- (k %*% r[1:6]) / 209
  k <- c1[2, ]
  p2 <- (k %*% r[1:6]) / 209
  k <- c1[3, ]
  endV <- length(r)
  p3 <- (k %*% r[(endV-5):endV]) / 209
  k <- c1[4, ]
  p4 <- (k %*% r[(endV-5):endV]) / 209
  r <- c(p1, p2, r, p3, p4)
  N <- y_length + 4

  p <- matrix(0,  f * (N - 5) + 1, 1)
  xx <- seq(1 / f, 1 - 1 / f, len = f - 1)

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
  return(p)
}
