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
    ##   [1] 0.82244833 0.03205944 0.33936528 0.09358754 0.31112718 0.06514236
    ##   [7] 0.32247777 0.94954076 0.45014347 0.38195412 0.38554274 0.06290206
    ##  [13] 0.14729845 0.57724251 0.69988225 0.20022393 0.73867953 0.92222239
    ##  [19] 0.86387292 0.30416229 0.43219006 0.55404712 0.70973460 0.63643008
    ##  [25] 0.10848105 0.09232365 0.80422400 0.15946137 0.32057820 0.44460728
    ##  [31] 0.45243718 0.90840451 0.23043167 0.12793575 0.44046638 0.81418098
    ##  [37] 0.71888658 0.37013585 0.93890901 0.09432753 0.23300090 0.34070704
    ##  [43] 0.90574827 0.33555667 0.32761298 0.56276474 0.11478832 0.35335022
    ##  [49] 0.14973134 0.85836084 0.56201298 0.53321888 0.81628558 0.08674029
    ##  [55] 0.12815925 0.51295990 0.31178748 0.45735262 0.38965824 0.11538113
    ##  [61] 0.93410139 0.60417504 0.21358834 0.91503104 0.60995418 0.15583524
    ##  [67] 0.45789012 0.66593360 0.57046269 0.60436085 0.63867519 0.21835519
    ##  [73] 0.88618376 0.41002066 0.14179054 0.56111299 0.40174397 0.09431155
    ##  [79] 0.97848868 0.07330583 0.08916777 0.51258587 0.62063855 0.18885698
    ##  [85] 0.16506483 0.53859625 0.95438818 0.89802691 0.59983837 0.48661700
    ##  [91] 0.28368371 0.70407807 0.04608895 0.40385728 0.99115265 0.40390070
    ##  [97] 0.14114709 0.40977876 0.05823662 0.96000271
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.50828 -0.66183 -0.03406 -0.02967  0.62221  3.57849

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

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.50828 -0.66183 -0.03406 -0.02967  0.62221  3.57849

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

    ##  [1]  4.86853865  5.55627064 -0.60616902 -1.30934063  6.60536604  3.45858272
    ##  [7]  7.62097197  8.82353004 13.52055032  6.35257169 11.23068601  9.41374647
    ## [13]  0.06992341  2.80799288 -3.14417766  1.69198597  7.00495563  6.90328234
    ## [19] -4.88167410 -2.94210713

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
    ## 1 0.436  6.47

## Use a for loop

create output list, and run for loop

``` r
output = vector("list",length = 4)
for (i in 1:4) {
  output[[i]] = mean_and_sd(list_norms[[i]])
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.436  6.47
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.15  5.08
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.74  7.51
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.14  9.67
