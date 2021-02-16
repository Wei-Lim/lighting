library(readr)
library(dplyr)
ees <- readr::read_csv("./data/light-sources/EES_380nm-780nm_1nm.csv")
as <- readr::read_csv("./data/spectral-sensitivity/cvrl.org_CIE_function_360nm-830nm_1nm.csv")
as <- as  %>%
  filter(380 <= wavelength & wavelength <= 780)

Km <- 683.002 # lm/W

wl <- seq(380,780,1)

spectrum <- ees$EES
V <- as$`CIE 1924 - V(lbd)`
test <- spectrum * V




Km*(pracma::trapz(wl,spectrum*V))
