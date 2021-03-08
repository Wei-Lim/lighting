library(dplyr)
library(data.table)

###
spectra <- fread("./inst/extdata/light-sources/CIE_015_2018_illuminants_300-780_5nm.csv")
spectra$EES <- as.numeric(spectra$EES)

# wavelength output for interpolation
wl_out <- seq(380, 780, 1)

spectra <- interpolate_spectra(spectra, wl_out)

K <- 683.002
R <- 1
ssf <- sensitivity_functions(wl_out)
cmf2 <- select(ssf$cvrl.org, c(x_cmf_2, y_cmf_2, z_cmf_2))

spd <- select(spectra, -all_of("wavelength.nm"))

df <- sapply(spectra,
             compute_XYZ_CIE1931,
             wavelength = wl_out,
             cmf2 = cmf2,
             K = K,
             R = R
             ) %>%
  unlist() %>%
  matrix(nrow = 6, byrow = TRUE)

compute_XYZ_CIE1931(spd, wl_out, cmf2, K, R)


# Code
wl_diff <- mean(diff(wl_out))
X <- K * sum(spectra * R * cmf2$x_cmf_2) * wl_diff
Y <- K * sum(spectra * R * cmf2$y_cmf_2) * wl_diff
Z <- K * sum(spectra * R * cmf2$z_cmf_2) * wl_diff

x <- X / (X + Y + Z)
y <- Y / (X + Y + Z)
z <- Z / (X + Y + Z)
matrix(x,y,z,X,Y,Z)
