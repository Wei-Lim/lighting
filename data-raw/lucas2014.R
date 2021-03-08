## code to prepare `lucas2014` dataset goes here
library(data.table)

lucas2014 <- fread("./data-raw/spectral-sensitivity/Lucas_2014_380-780_5nm.csv")

setnames(
  lucas2014,
  old = c("cyanopic", "chloropic", "erythropic", "rhodopic", "melanopic",
          "photopic", "lens transm."
  ),
  new = c("N_sc"    , "N_mc"     , "N_lc"      , "N_r"     , "N_z",
          "V_pho"   , "T_lens"
  )
)

setDF(lucas2014)

usethis::use_data(lucas2014, overwrite = TRUE)
