## code to prepare `cie2018_S_026` dataset goes here
library(data.table)

cie2018_S_026 <- fread("./data-raw/spectral-sensitivity/CIE_2018_S_026_380-780_1nm.csv")

setnames(
  cie2018_S_026,
  old = c("S-cone-opic", "M-cone-opic", "L-cone-opic", "rhodopic", "melanopic"),
  new = c("s_sc"       , "s_mc"       , "s_lc"       , "s_rh"    , "s_mel")
)

setDF(cie2018_S_026)

usethis::use_data(cie2018_S_026, overwrite = TRUE)
