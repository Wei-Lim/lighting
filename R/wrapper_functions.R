

map_approx <- function(x, y, xout, method, yleft, yright) {
  l <- approx(x, y, xout, method, yleft, yright)
  res <- l$y
  return(res)
}

map_spline <- function(x, y, xout, method) {
  l <- spline(x, y, xout = xout, method = method)
  res <- l$y
  return(res)
}
