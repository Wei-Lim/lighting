## code to prepare `rea2010` dataset goes here
library(data.table)

rea2010 <- fread("./data-raw/spectral-sensitivity/Rea_2010_380-730_10nm.csv")

setnames(
  rea2010,
  old = c("S cone Slbd" , "scotopic V'lbd", "ipRGC"        , "photopic V10lbd"),
  new = c("S_sc"        , "V_sco"         , "M"        , "V_pho_10")
)

setDF(rea2010)

usethis::use_data(rea2010, overwrite = TRUE)
