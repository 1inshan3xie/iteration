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

    ##  [1] -1.31625212 -1.03711420  0.29941069 -0.77347947  1.63000339 -1.14621123
    ##  [7] -0.29292117  0.91309028  0.76107027  0.95181443 -1.03996584  0.91504215
    ## [13]  1.83417552 -1.46416390 -0.70496792 -0.46043172  0.67238389 -0.51695801
    ## [19]  0.75540874  0.06636638 -1.72136577 -0.38809291  0.90934978  0.79147788
    ## [25]  0.36233086

Now i’ll write a function to do this

``` r
z_score = function(x){
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_score(x = x_vec)
```

    ##  [1] -1.31625212 -1.03711420  0.29941069 -0.77347947  1.63000339 -1.14621123
    ##  [7] -0.29292117  0.91309028  0.76107027  0.95181443 -1.03996584  0.91504215
    ## [13]  1.83417552 -1.46416390 -0.70496792 -0.46043172  0.67238389 -0.51695801
    ## [19]  0.75540874  0.06636638 -1.72136577 -0.38809291  0.90934978  0.79147788
    ## [25]  0.36233086

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
