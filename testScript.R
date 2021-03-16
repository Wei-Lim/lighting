#library(tidyr)
library(dplyr)
library(data.table)
library(tidyverse)
#library(tictoc)

###
spectra <- fread("./inst/extdata/light-sources/CIE_015_2018_illuminants_300-780_5nm.csv")
spectra$EES <- as.numeric(spectra$EES)
setDF(spectra)

# wavelength output for interpolation
wl_out <- seq(380, 780, 5)

spectrum <- spectra$EES
ssf <- sensitivity_functions(wl_out)
cmf2 <- select(ssf$cvrl.org, c(x_cmf_2, y_cmf_2, z_cmf_2))
TCS <- ssf$cie1995_13.3



#spectra <- interpolate_spectra(spectra, wl_out)
method <- "linear"
str_wavelength <- "wavelength.nm"
wl <- spectra[[str_wavelength]]

# wrapper function
myfun <- function(x, y, xout, method) {
  l <- approx(x, y, xout, method, yleft = 0, yright = 0)
  res <- l$y
  return(res)
}


test <- spectra %>%
  select(-as.name(str_wavelength)) %>%
  map_dfc(
    myfun,
    x = wl,
    xout = wl_out,
    method = method
    )

approx(x = wl, y = spectra$EES, xout= wl_out)

