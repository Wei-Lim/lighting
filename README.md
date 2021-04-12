
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lighting

<!-- badges: start -->
<!-- badges: end -->

The goal of lighting is to provide a function to calculate lighting
values of spectra.

## Installation

<!-- You can install the released version of lighting from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("lighting") -->
<!-- ``` -->

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Wei-Lim/lighting")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(lighting)
## basic example code
# load or generate a absolute spectrum and creating a dataframe
wavelength <- seq(380, 780, 1)
df_spectra <- data.frame(wavelength)
# generate equivalent energy spectrum
df_spectra$EES <- 1
# generate spectra of planckian radiator using body temperature in K
df_spectra$P_5000 <- planck_law(5000, wavelength)


# calculating lighting values
compute_lighting(df_spectra)
#>                 E_v   E^D65_sc,v   E^D65_mc,v   E^D65_lc,v   E^D65_rh,v
#> EES    7.298313e+04 6.322860e+04 7.002033e+04 7.343292e+04 6.696076e+04
#> P_5000 2.884317e+18 2.130843e+18 2.728751e+18 2.892210e+18 2.514670e+18
#>         E^D65_mel,v gamma^D65_sc,v gamma^D65_mc,v gamma^D65_lc,v gamma^D65_rh,v
#> EES    6.611318e+04      0.8663454      0.9594042       1.006163      0.9174827
#> P_5000 2.437686e+18      0.7387686      0.9460649       1.002737      0.8718424
#>        gamma^D65_mel,v  E_v,mel,D65   a_mel,v          EML R_mel,ratio      CCT
#> EES          0.9058693 6.611318e+04 0.8205338 7.264386e+04   0.9953514 5455.761
#> P_5000       0.8451520 2.437686e+18 0.7655362 2.678481e+18   0.9286364 5000.000
#>             R_a      R_1      R_2      R_3      R_4      R_5      R_6      R_7
#> EES    95.28884 94.58566 96.79495 98.20200 92.61221 94.17999 96.43327 96.49333
#> P_5000 98.62715 99.36049 98.97014 99.32823 97.33024 98.58838 99.12349 97.89873
#>             R_8      R_9     R_10     R_11     R_12     R_13     R_14       x_2
#> EES    93.00933 82.22809 94.22567 92.25888 88.76124 94.71195 98.69880 0.3333413
#> P_5000 98.41749 98.83776 98.41111 97.70811 93.75563 98.62692 99.57781 0.3451189
#>              y_2       z_2          X_2          Y_2          Z_2   u_prime
#> EES    0.3333455 0.3333132 7.298224e+04 7.298315e+04 7.297609e+04 0.2105270
#> P_5000 0.3516459 0.3032352 2.830781e+18 2.884317e+18 2.487237e+18 0.2114209
#>          v_prime         E_sc         E_mc         E_lc          E_r
#> EES    0.4736918 7.296780e+04 7.298196e+04 7.298207e+04 7.298153e+04
#> P_5000 0.4846936 2.425286e+18 2.831866e+18 2.864253e+18 2.750246e+18
#>                 E_z        CS         CL_A           SV
#> EES    7.298152e+04 0.6992131 1.680363e+05 3.026874e+01
#> P_5000 2.690920e+18 0.7000000 6.034985e+18 8.456950e+14
```
