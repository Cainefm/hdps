
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

Here, we are using the diagnosis database as an illustrative example.

<p align="center">
<img width="131" alt="CleanShot 2023-07-24 at 17 43 16@2x" src="https://github.com/Cainefm/hdps/assets/20833144/f7bb062f-d347-4615-8a71-61d874557ca1">
</p>

In addition to the diagnosis dataset, we have a master sheet that includes exposure and outcome indicators.
<p align="center">
<img width="178" alt="CleanShot 2023-07-24 at 17 42 54@2x" src="https://github.com/Cainefm/hdps/assets/20833144/4b6cc860-3b26-4dbd-afb2-068f300a5d23">

</p>


### Loading the package...  
This package harnesses the power of *data.table* for efficient handling of large datasets. But there is no need to be a data.table before using it.
``` r
library(hdps)
#> Loading required package: data.table
#> Loading required package: pbapply
```

### Generating the prevelence for covariates
``` r
hdpsCohort <- rec_assess(dx,"pid",code = "icd9code",type = "dx")
hdpsCohort[1:5,1:5]
#   id dx_000_freq dx_000_once dx_000_spor dx_002.3_freq
#1:  1           0           0           0             0
#2:  2           0           0           0             0
#3:  3           0           0           0             0
#4:  4           0           0           0             0
#5:  5           0           0           0             0
```
<img width="471" alt="CleanShot 2023-07-24 at 17 48 45@2x" src="https://github.com/Cainefm/hdps/assets/20833144/7083ce58-f16b-48a3-8df8-a45a842872e9">


### Merge the master sheet with hdps information
``` r
hdpsCohort<- merge(master,hdpsCohort,by="pid")
hdpsCohort[1:5,1:5]
#   pid outcome exposure dx_000_freq dx_000_once
#1:   1       0        0           0           0
#2:   2       0        1           0           0
#3:   3       1        1           0           0
#4:   4       0        0           0           0
#5:   5       1        0           0           0
```
<img width="327" alt="CleanShot 2023-07-24 at 17 56 39@2x" src="https://github.com/Cainefm/hdps/assets/20833144/95be10ff-da06-4aa4-823b-46293de36fbb">

### Recurrence Assessment and Prioritization
``` r
prioritize(hdpsCohort,type = "dx",expo = "exposure",outc = "outcome")
head(hdpsResult)
#            code  e1  e0  d1  d0  c1  c0 e1c1 e0c1 e1c0 e0c0 d1c1 d0c1 d1c0 d0c0         pc1         pc0       rrCE     rrCD      #bias
#1:   dx_000_freq 251 240 242 249 1.0 490    1    0  250  240    1    0  241  249 0.003984064 0.100000000 0.03984064 2.033195 #0.9100866
#2:   dx_000_once 251 240 242 249 2.0 489    1    1  250  239    1    1  241  248 0.003984064 0.004166667 0.95617530 1.014523 #0.9999973
#3:   dx_000_spor 251 240 242 249 1.0 490    1    0  250  240    1    0  241  249 0.003984064 0.100000000 0.03984064 2.033195 #0.9100866
#4: dx_002.3_freq 251 240 242 249 0.1 491    0    0  251  240    0    0  242  249 0.000000000 0.100000000         NA       NA       # NA
#5: dx_002.3_once 251 240 242 249 1.0 490    1    0  250  240    1    0  241  249 0.003984064 0.100000000 0.03984064 2.033195 #0.9100866
#6: dx_002.3_spor 251 240 242 249 1.0 490    1    0  250  240    1    0  241  249 0.003984064 0.100000000 0.03984064 2.033195 #0.9100866
#     absLogBias ce_strength cd_strength
#1: 9.421550e-02   0.9601594  1.03319502
#2: 2.651753e-06   0.0438247  0.01452282
#3: 9.421550e-02   0.9601594  1.03319502
#4:           NA          NA          NA
#5: 9.421550e-02   0.9601594  1.03319502
#6: 9.421550e-02   0.9601594  1.03319502
```
<img width="885" alt="CleanShot 2023-07-24 at 18 15 37@2x" src="https://github.com/Cainefm/hdps/assets/20833144/55a104ba-b1f4-4bbe-bc00-aac567bcfe93">


### Plot for bias

``` r
library(plotly)
p <- ggplot(data=hdpsResult, 
            aes(text=code, x = rrCE, y = rrCD)) +
    geom_point() +
    theme_bw()

ggplotly(p)
```
![image](https://github.com/Cainefm/hdps/assets/20833144/e3fa7e0b-af0c-4abc-a112-c180e06a38e6)
{% include a.html %}
