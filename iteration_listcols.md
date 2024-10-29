Simple document
================
Linshan Xie
2024-10-29

``` r
library(ggplot2)
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal()+theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis")

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Here is some lists

``` r
l = list(
  vec_numeric = 1:4,
  unif_sample = runif(100),
  mat         = matrix(1:8, 2, 4),
  summary     = summary(rnorm(1000)))

l
```

    ## $vec_numeric
    ## [1] 1 2 3 4
    ## 
    ## $unif_sample
    ##   [1] 0.044708464 0.536820162 0.548076554 0.034092851 0.720480222 0.526111766
    ##   [7] 0.439135832 0.441268398 0.101891284 0.191659557 0.022523141 0.556214345
    ##  [13] 0.909474777 0.623839113 0.726159577 0.365049953 0.393479040 0.918354940
    ##  [19] 0.911481072 0.577806021 0.629675054 0.085373301 0.392633704 0.800533041
    ##  [25] 0.196854511 0.242017825 0.757903588 0.787253734 0.644359191 0.285393362
    ##  [31] 0.500313285 0.765383376 0.990015047 0.605824046 0.648964877 0.001707196
    ##  [37] 0.358286910 0.366762647 0.446350680 0.698567974 0.452852783 0.398782032
    ##  [43] 0.852461431 0.171725053 0.641359387 0.326353910 0.905251764 0.550943193
    ##  [49] 0.788704840 0.613113949 0.651910058 0.203349323 0.831147798 0.799914102
    ##  [55] 0.706854362 0.822834024 0.803936613 0.138640839 0.696612193 0.780514416
    ##  [61] 0.444917820 0.425570238 0.509507563 0.220065808 0.935580004 0.050814440
    ##  [67] 0.110569624 0.121267753 0.563570581 0.848667582 0.210821979 0.230899644
    ##  [73] 0.305327538 0.139591723 0.164775954 0.711803024 0.932663110 0.227448293
    ##  [79] 0.300836428 0.517589759 0.276232268 0.869844422 0.574403227 0.481212816
    ##  [85] 0.724424712 0.915875721 0.544761887 0.219754490 0.213685395 0.619747151
    ##  [91] 0.354357383 0.967051640 0.536627861 0.180924098 0.632726311 0.391447427
    ##  [97] 0.715019620 0.295398836 0.441742586 0.848710571
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -2.809358 -0.689444  0.040762 -0.006973  0.667122  3.198847

``` r
l$mat
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

``` r
l[["mat"]]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

``` r
l[["mat"]][1,3]
```

    ## [1] 5

``` r
l[[1]]
```

    ## [1] 1 2 3 4

``` r
l[[4]]
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -2.809358 -0.689444  0.040762 -0.006973  0.667122  3.198847

Make a list that is hopefully a bit more useful

``` r
list_norms = 
  list(
    a = rnorm(20, 0, 5),
    b = rnorm(20, 4, 5),
    c = rnorm(20, 0, 10),
    d = rnorm(20, 4, 10)
  )

list_norms[["b"]]
```

    ##  [1]  7.6189412  2.9921595  4.3170054 14.1035067  5.8539751 -4.1976957
    ##  [7]  8.6322484  2.1585478 -2.2049122 13.5194423 -4.0801944  0.4153971
    ## [13]  8.0497822  4.9405223  9.0607403  3.3519748  6.3982053 14.4990392
    ## [19]  3.6457181  5.7506852

Let’s reuse the function that we wrote last time.

``` r
mean_and_sd = function(x){
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  out_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(out_df)
}
```

use the function

``` r
mean_and_sd(list_norms[["a"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.13  4.32
