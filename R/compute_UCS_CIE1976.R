#' Title
#'
#' @param X_2
#' @param Y_2
#' @param Z_2
#'
#' @NoRd
compute_UCS_CIE1976 <- function(X_2, Y_2, Z_2) {
  u_prime = 4 * X_2 / (X_2 + 15 * Y_2 + 3 * Z_2)
  v_prime = 9 * Y_2 / (X_2 + 15 * Y_2 + 3 * Z_2)
  #w_prime = 1 - u_prime - v_prime
  df <- data.frame(u_prime, v_prime)
  return(df)
}
