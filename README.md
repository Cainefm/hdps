
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

This example utilizes a simulated diagnosis database from the Clinical Data Analysis and Reporting System (CDARS) in the Hong Kong, which is an electronic health records system for public hospitals. By employing this database, we aim to demonstrate the *HDPS* package, and potentially contribute to broader efforts in enhancing other real-world clinical data analysis.

## Example dataset 
### Master sheet 
Here is the master sheet for a random sample of 491 pseudo-patients  
- pid: patient ID  
- outcome: 0-no outcome and 1-with outcome  
- exposure: 0-no exposure and 1-with exposure  

<p align="center">
<img width="178" alt="CleanShot 2023-07-24 at 17 42 54@2x" src="https://github.com/Cainefm/hdps/assets/20833144/4b6cc860-3b26-4dbd-afb2-068f300a5d23">
</p>

### Diagnosis dataset 
Here are the random simulated diagnosis records (Dx) from the CDARS database.  
- pid: patient ID  
- icd9code: diagnosis code of ICD-9-cm before index date for each patient  

<p align="center">
<img width="131" alt="CleanShot 2023-07-24 at 17 43 16@2x" src="https://github.com/Cainefm/hdps/assets/20833144/f7bb062f-d347-4615-8a71-61d874557ca1">
</p>


## Package installation  

You can install the development version of hdps from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Cainefm/hdps")
```

## Loading the package   
Ps. This package harnesses the power of *data.table* for efficient handling of large datasets. 
``` r
library(hdps)
#> Loading required package: data.table
#> Loading required package: pbapply
```
## Generating the prevalence for covariates  
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
<p align="center"><img width="471" alt="CleanShot 2023-07-24 at 17 48 45@2x" src="https://github.com/Cainefm/hdps/assets/20833144/7083ce58-f16b-48a3-8df8-a45a842872e9"></p>

## Merge the master sheet with covariates' prevalence  
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
<p align="center"><img width="327" alt="CleanShot 2023-07-24 at 17 56 39@2x" src="https://github.com/Cainefm/hdps/assets/20833144/95be10ff-da06-4aa4-823b-46293de36fbb"></p>

## Recurrence Assessment and Prioritization  
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
<p align="center"><img src="https://github.com/Cainefm/hdps/assets/20833144/55a104ba-b1f4-4bbe-bc00-aac567bcfe93"></p>

## Covariates selection
After obtaining the bias score, we can select the number of covariates to be included in the model. The selection can be done using the bias score, the prevalence of the covariates, or the strength of the covariates. 

``` r
hdpsResult[order(absLogBias,decreasing = T),.SD[1:250]][,code]
# [1] "dx_518.0_once"  "dx_294.8_freq"  "dx_E980.5_once"
# [4] "dx_428.0_freq"  "dx_883.2_once"  "dx_E928.9_once"
# [7] "dx_242.90_once" "dx_682.8_once"  "dx_781.9_once" 
# [10] "dx_943.00_once" "dx_453.8_once"  "dx_536.8_once"
```
<p align="center"><img src="https://github.com/Cainefm/hdps/assets/20833144/78d2699d-6604-4129-b154-41477209b0b8"></p>

## Bias diagnostic plot
``` r
library(plotly)
p <- ggplot(data=hdpsResult, 
            aes(text=code, x = rrCE, y = rrCD)) +
    geom_point() +
    theme_bw()+
    labs(x = "Strength of covariate-outcome association",
         y = "Strength of covariate-exposure association")

ggplotly(p)
```
<p align="center"><img src="https://github.com/Cainefm/hdps/assets/20833144/1167cdbe-e7fd-4aad-b14f-24dc3f3df910" width="600" height="600"></p>
