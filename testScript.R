library(dplyr)
library(data.table)

###
spectra <- fread("./inst/extdata/light-sources/CIE_015_2018_illuminants_300-780_5nm.csv")
spectra$EES <- as.numeric(spectra$EES)
as <- fread("./inst/extdata/spectral-sensitivity/cvrl.org_CIE_function_360nm-830nm_1nm.csv")

# wavelength output for interpolation
x_out <- seq(380, 780, 5)

# linear interpolation of spd to 380-780_5nm
spectra <- interpolate_spectra(spectra, method = "fmm")
plot(spectra$wavelength.nm, spectra$`Standard illuminant D65`)

# linear interpolate as to 5 nm steps
as <- interpolate_spectra(as)




Km <- 683.002 # lm/W




V <- as$`CIE 1924 - V(lbd)`

integrate_spectrum(spectra[,1], x_out, V, Km)

# photopic values
sapply(spectra,
       integrate_spectrum,
       wavelength = x_out,
       sensitivity = V,
       constant = Km
       )



