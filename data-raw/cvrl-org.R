## code to prepare `cvrl.org` dataset goes here
library(data.table)

cvrl.org <- fread("./data-raw/spectral-sensitivity/cvrl.org_360-830_1nm.csv")

setnames(
  cvrl.org,
  old = c("CIE 1924 - V(lbd)", "CIE 1951 - V'(lbd)",
          "CIE 1931 - x2"    , "CIE 1931 - y2"     , "CIE 1931 - z2",
          "CIE 1964 - x10"   , "CIE 1964 - y10"    , "CIE 1964 - z10"
  ),
  new = c("V_pho"            , "V_sco",
          "x_cmf_2"           , "y_cmf_2"            , "z_cmf_2",
          "x_cmf_10"          , "y_cmf_10"           , "z_cmf_10"
  )
)

setDF(cvrl.org)

usethis::use_data(cvrl.org, overwrite = TRUE)
