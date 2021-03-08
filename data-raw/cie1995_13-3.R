## code to prepare `cie1995_13.3` dataset goes here
library(data.table)

cie1995_13.3 <- fread("./data-raw/spectral-sensitivity/CIE_1995_TCS_360-830_5nm.csv")

setDF(cie1995_13.3)

usethis::use_data(cie1995_13.3, overwrite = TRUE)

