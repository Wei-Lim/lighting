#library(tidyr)
library(dplyr)
library(data.table)
library(tidyverse)
library(microbenchmark)
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

long_fun <- function(spectra) {

  method <- "linear"
  str_wavelength <- colnames(spectra)[1]
  wl <- spectra[[str_wavelength]]

  spectra_long <- melt(
    setDT(spectra),
    id.vars = str_wavelength,
    variable.name = "spectrum",
    value.name = "value"
  )

  var_wl <- as.name(str_wavelength)

  spectra_interpolated <- spectra_long[
    ,
    approx(
      x = eval(var_wl),
      y = value,
      xout = wl_out,
      method = method,
      yleft = 0,
      yright = 0
    ),
    by = spectrum
  ] %>%
    setnames(
      old = c("x", "y"),
      new = c(str_wavelength, "value")
    ) %>%
    dcast(
      as.formula(paste(str_wavelength, "~", "spectrum")),
      value.var = "value"
    ) %>%
    setDF()
  return(spectra_interpolated)
}



method <- "linear"
str_wavelength <- "wavelength.nm"
wl <- spectra[[str_wavelength]]

map_approx <- function(x, y, xout, method, yleft, yright) {
  l <- approx(x, y, xout, method, yleft, yright)
  res <- l$y
  return(res)
}

test <- spectra %>%
  select(-as.name(str_wavelength)) %>%
  map_df(
    map_spline,
    x = wl,
    xout = wl_out,
    method = "fmm"
  )
test <- cbind(wl_out, test)

# wrapper function

# microbenchmark
mbm <- microbenchmark(
  map_fun(spectra),
  long_fun(spectra),
  times = 500
)
ggplot2::autoplot(mbm)


