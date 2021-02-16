lightingTrapz <- function(spectrum,wavelength,sensitivity,constant){
  value <- constant*(pracma::trapz(wavelength,spectrum*sensitivity))
  return(value)
}
