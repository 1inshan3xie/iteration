Simple document
================
Linshan Xie
2024-10-24

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

Load the key packages.

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

## writing my first function

as an example, here’s a z-score computation

``` r
x_vec = rnorm(25, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.558096555 -1.156145168  0.805766393 -0.004049964  0.042332198
    ##  [6]  0.430543670  0.433803738 -0.343151345  0.582117843 -0.334013413
    ## [11] -0.515282965  2.519296823  1.041206697  1.850255261  0.476720747
    ## [16]  0.222115993 -0.210060067 -0.593105891 -0.828920062  0.283796393
    ## [21] -0.957687653 -0.520173132 -2.646871834 -0.060677009  0.040279303

Now i’ll write a function to do this

``` r
z_score = function(x){
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_score(x = x_vec)
```

    ##  [1] -0.558096555 -1.156145168  0.805766393 -0.004049964  0.042332198
    ##  [6]  0.430543670  0.433803738 -0.343151345  0.582117843 -0.334013413
    ## [11] -0.515282965  2.519296823  1.041206697  1.850255261  0.476720747
    ## [16]  0.222115993 -0.210060067 -0.593105891 -0.828920062  0.283796393
    ## [21] -0.957687653 -0.520173132 -2.646871834 -0.060677009  0.040279303

does this always work?

``` r
z_score(x = 3)
```

    ## [1] NA

``` r
z_score(x=c("my","name","is","jeff"))
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in x - mean(x): non-numeric argument to binary operator

需要加上条件

``` r
z_score = function(x){
  if(!is.numeric(x)){
    stop("x needs to be numeric")
  }
  
  if(length(x) < 5){
    stop("you need at least 5 numbers to compute z score")
  }
  
 z = (x - mean(x)) / sd(x)
 
 return(z)
}
```

``` r
z_score(x = 3)
```

    ## Error in z_score(x = 3): you need at least 5 numbers to compute z score

``` r
z_score(x=c("my","name","is","jeff"))
```

    ## Error in z_score(x = c("my", "name", "is", "jeff")): x needs to be numeric

## A new function

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

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.7  3.62

## Check stuff using a simulation

``` r
sim_df = 
  tibble(
    x = rnorm(30, 10, 5)
  )

sim_df |>
  summarize(
    mean = mean(x), 
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.9  4.56

### create a simulation fuction to check mean and sd

``` r
sim_mean_sd = function(sample_size, true_mean = 10, true_sd = 5){
  sim_df = 
    tibble(
      x = rnorm(sample_size, true_mean, true_sd))
  
  out_df = sim_df |>
    summarize(
      mean = mean(x), 
      sd = sd(x))
  
  return(out_df)
}
```

``` r
sim_mean_sd(sample_size = 30, true_mean = 4, true_sd = 12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.39  12.6

``` r
sim_mean_sd(sample_size = 30, true_mean = 4, true_sd = 12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.22  12.8

``` r
sim_mean_sd(30, 16, 2)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  15.8  1.91

``` r
sim_mean_sd(330)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.6  5.10
