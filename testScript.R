library(dplyr)
library(data.table)
library(tictoc)

###
spectra <- fread("./inst/extdata/light-sources/CIE_015_2018_illuminants_300-780_5nm.csv")
spectra$EES <- as.numeric(spectra$EES)

# wavelength output for interpolation
wl_out <- seq(380, 780, 1)

spectra <- interpolate_spectra(spectra, wl_out)

Km <- 683.002
R <- 1
ssf <- sensitivity_functions(wl_out)
cmf2 <- select(ssf$cvrl.org, c(x_cmf_2, y_cmf_2, z_cmf_2))

spd <- select(spectra, -all_of("wavelength.nm"))

df <- sapply(spd,
             compute_CCT,
             wavelength = wl_out,
             cmf2 = cmf2,
             K = Km,
             R = R
             ) %>%
  unlist(use.names = TRUE) %>%
  matrix(nrow = 9)
rownames(df) <- c("CCT",
                  "x_2", "y_2", "z_2", "X_2", "Y_2", "Z_2",
                  "u_prime", "v_prime")

X <- df[4,]
Y <- df[5,]
Z <- df[6,]

compute_UCS_CIE1976(X, Y, Z)




# Code CCT

ees <- wl_out
ees[] <- 1 # ca. 5400 K

compute_CCT(ees, wl_out, cmf2)

current_CCT <- 5400

# define starting interval using McCamy method
xyzXYZ <- compute_XYZ_CIE1931(ees, wl_out, cmf2, Km, R)
uv <- compute_UCS_CIE1976(xyzXYZ$X_2, xyzXYZ$Y_2, xyzXYZ$Z_2)

n <- (xyzXYZ$x_2 - 0.3320) / (xyzXYZ$y_2 - 0.1858)
current_CCT <- -449 * n^3 + 3525 * n^2 - 6823.3 * n + 5520.33
CCTmin <- current_CCT - 500
CCTmax <- current_CCT + 500


test <- optimize(
  find_CCT,
  wavelength = wl_out,
  u_prime = uv$u_prime,
  v_prime = uv$v_prime,
  cmf2 = cmf2,
  Km = Km,
  interval = c(CCTmin, CCTmax)
  )

rosena <- function(x, a) 100*(x[2]-x[1]^2)^2 + (a-x[1])^2  # min: (a, a^2)
pracma::fminsearch(rosena, c(-1.2, 1), a = sqrt(2))
# x = (1.414214 2.000010) , fval = 1.239435e-11

fminsearch(rosena, c(-1.2, 1), dfree=FALSE, a = sqrt(2))
# x = (1.414214 2.000000) , fval = 3.844519e-26
