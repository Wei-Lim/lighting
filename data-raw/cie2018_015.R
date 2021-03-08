## code to prepare `cie2018_015` dataset goes here
library(data.table)

cie2018_015 <- fread("./data-raw/spectral-sensitivity/CIE_2018_015_daylight_components_300-830_5nm.csv")

setDF(cie2018_015)

usethis::use_data(cie2018_015, overwrite = TRUE)
