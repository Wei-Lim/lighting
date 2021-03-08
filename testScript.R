library(dplyr)
library(data.table)

###
spectra <- fread("./inst/extdata/light-sources/CIE_015_2018_illuminants_300-780_5nm.csv")
spectra$EES <- as.numeric(spectra$EES)

# wavelength output for interpolation
x_out <- seq(380, 780, 5)

# linear interpolation of spd to 380-780_5nm
spectra <- interpolate_spectra(spectra, method = "fmm")
plot(spectra$wavelength.nm, spectra$`Standard illuminant D65`)

compute_lighting(spectra)

# linear interpolate as to 5 nm steps
as <- interpolate_spectra(as)

interpolate_spectra(spectra, seq(400, 700, 1), method = "sprague")
