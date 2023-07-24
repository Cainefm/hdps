
<!-- README.md is generated from README.Rmd. Please edit that file -->

# High-dimensional propensity score (HDPS)

<!-- badges: start -->
<!-- badges: end -->

The primary objective of HDPS is to develop an R-package tailored for
conducting High-Dimensional Propensity Score (HDPS) analyses in
epidemiological studies. The initial focus of the project is on the
CDARS database, a clinical electronic database in Hong Kong.
Nevertheless, the package is designed to be versatile and can be adapted
for use with other databases too.

## Installation

You can install the development version of hdps from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Cainefm/hdps")
```

## Example

Here we using diagnosis database as an example to illustrate: Diagnosis
records
<p align="center">
<img width="400" src="https://user-images.githubusercontent.com/20833144/147062935-b6d7ab55-aee7-455e-bdf0-b9da26444542.png">
</p>

This is a basic example which shows you how to solve a common problem:

``` r
library(hdps)
#> Loading required package: data.table
#> Loading required package: pbapply
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
