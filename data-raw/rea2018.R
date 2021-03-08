## code to prepare `rea2018` dataset goes here
library(data.table)

rea2018 <- fread("./data-raw/spectral-sensitivity/Rea_2018_380-780_2nm.csv")

setnames(
  rea2018,
  old = c("Photopic (2 deg. Observer)", "Scotopic", "Vlambda / macula",
          "Scone / macula", "Melanopsin corr. * lens"
  ),
  new = c("V_pho"                     , "V_sco"   , "V_pho_mac",
          "S_sc_mac"      , "Mc_lens"
  )
)

setDF(rea2018)

usethis::use_data(rea2018, overwrite = TRUE)
