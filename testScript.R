library(dplyr)
library(data.table)

###
spd <- fread("./inst/extdata/light-sources/CIE_015_2018_illuminants_300-780_5nm.csv")
spd$EES <- as.numeric(spd$EES)
as <- fread("./inst/extdata/spectral-sensitivity/cvrl.org_CIE_function_360nm-830nm_1nm.csv")

# wavelength output for interpolation
wl_out <- seq(380,780,5)

# linear interpolation of spd to 380-780_5nm
spd <- interpolate_spectra(spd, method = "fmm")
plot(spd$wavelength.nm,spd$`Standard illuminant D65`)

# linear interpolate as to 5 nm steps
as <- interpolate_spectra(as)



Km <- 683.002 # lm/W

spd <- spd %>% select(-wavelength.nm)

spectrum <- spd$EES
V <- as$`CIE 1924 - V(lbd)`

# photopic integration
lightingTrapz <- function(spectrum,wavelength,sensitivity,constant){
  value <- constant*(pracma::trapz(wavelength,spectrum*sensitivity))
  return(value)
}

sapply(spd, lightingTrapz, sensitivity = V, wavelength = wl_out, constant = Km)




