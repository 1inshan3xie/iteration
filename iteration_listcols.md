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
    ##   [1] 0.999236594 0.690109011 0.622207868 0.223092291 0.316769388 0.563352187
    ##   [7] 0.950580032 0.693880449 0.490143164 0.383278471 0.358533279 0.630749232
    ##  [13] 0.085175690 0.740221297 0.309665196 0.732456172 0.260399670 0.580916292
    ##  [19] 0.552366399 0.134600180 0.851291646 0.853710276 0.200811899 0.384227595
    ##  [25] 0.364121739 0.523028012 0.327496426 0.740157737 0.315071785 0.940674605
    ##  [31] 0.732994822 0.735769837 0.535978061 0.526185420 0.117957811 0.758962376
    ##  [37] 0.567747646 0.854114154 0.212233267 0.993913696 0.041391754 0.997595163
    ##  [43] 0.726367716 0.951533112 0.297661190 0.168299787 0.177577283 0.801776711
    ##  [49] 0.806721714 0.753854671 0.599564969 0.762999507 0.392647565 0.182244391
    ##  [55] 0.104859335 0.860853716 0.753838219 0.395659602 0.009287617 0.228756447
    ##  [61] 0.236058217 0.108888598 0.798670799 0.927867034 0.278511719 0.096342155
    ##  [67] 0.484717977 0.872358395 0.971037125 0.713796853 0.577342435 0.969669524
    ##  [73] 0.870460850 0.007039391 0.274554334 0.162943930 0.398043702 0.592278527
    ##  [79] 0.569966783 0.206611522 0.025417886 0.689268483 0.418148232 0.912665733
    ##  [85] 0.788749960 0.765944541 0.303565074 0.443715053 0.141913987 0.715966603
    ##  [91] 0.656603096 0.397994672 0.768675836 0.041381444 0.713430052 0.297448810
    ##  [97] 0.092432342 0.876794323 0.835921793 0.309453242
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.30410 -0.58269  0.02918  0.02680  0.68280  3.24321

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
    ## -3.30410 -0.58269  0.02918  0.02680  0.68280  3.24321

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

    ##  [1]  6.4547586 -2.6240905  0.7356150 -2.1518541  7.4375542 -2.0491518
    ##  [7]  1.1632274 -3.3947218 -2.2430502 -3.4818809  5.5097373  3.7004710
    ## [13]  0.7606382 -0.7912903  1.4952542  5.3702240  7.6910102 -4.1461363
    ## [19]  7.3190655  5.6081555

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
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.217  5.55

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
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.217  5.55
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.62  4.16
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.52  9.33
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.80  9.12

## Do the same thing

but with `map` instead

``` r
#这个的结果会输出四个表
output = map(list_norms, mean_and_sd)
output
```

    ## $a
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.217  5.55
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.62  4.16
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.52  9.33
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.80  9.12

``` r
#也素把结果写在一个表里
output = map_dbl(list_norms, IQR)
output
```

    ##         a         b         c         d 
    ##  6.144412  7.708995 12.794413 10.104911

``` r
##把结果写在一个表里
output = map_dfr(list_norms, mean_and_sd)
output
```

    ## # A tibble: 4 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.217  5.55
    ## 2  1.62   4.16
    ## 3  1.52   9.33
    ## 4  4.80   9.12

## List columns

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norms
  )

##可以对listcol_df进行操作，just like一个df
listcol_df |>
  filter(name %in% c("a", "b"))
```

    ## # A tibble: 2 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>

``` r
listcol_df |>
  select(-samp)
```

    ## # A tibble: 4 × 1
    ##   name 
    ##   <chr>
    ## 1 a    
    ## 2 b    
    ## 3 c    
    ## 4 d

``` r
#调取samp里的a
listcol_df[["samp"]][["a"]]
```

    ##  [1]   0.49327758   0.88806308   1.93673429 -14.31001618   9.16883082
    ##  [6]   2.25536846  -3.93732299  -0.63415874  -3.30220797  -5.32628225
    ## [11]   6.14773461   6.40195657  -4.23723932  -0.12739357   0.53971397
    ## [16]   3.96759723   6.57948650  -2.44327320  -0.03582932  -8.37227503

``` r
mean_and_sd(listcol_df[["samp"]][["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.62  4.16

``` r
map(listcol_df[["samp"]], mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.217  5.55
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.62  4.16
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.52  9.33
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.80  9.12

ADD A LIST COLUMN

``` r
listcol_df |>
  mutate(output = map(samp, mean_and_sd),
         iqr = map_dbl(samp, IQR)) |>
  select(-samp) |>
  unnest(output)
```

    ## # A tibble: 4 × 4
    ##   name    mean    sd   iqr
    ##   <chr>  <dbl> <dbl> <dbl>
    ## 1 a     -0.217  5.55  6.14
    ## 2 b      1.62   4.16  7.71
    ## 3 c      1.52   9.33 12.8 
    ## 4 d      4.80   9.12 10.1

``` r
## unnest:将保存在tibble中的子数据框合并为一个大数据框
```

## NSDUH

use the function we worte last time

``` r
nsduh_table_format = function(html, table_number) {
  out_table = 
    html |> 
    html_table() |> 
    nth(table_number) |>
    slice(-1) |> 
    select(-contains("P value"))
  
  return(out_table)
}
```

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)
```

``` r
nsduh_table_format(html = nsduh_html, table_number = 1)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 12.90a           13.36            13.28b             12.86             
    ##  2 Nort… 13.88a           14.66            13.98              13.51             
    ##  3 Midw… 12.40b           12.76            12.45              12.33             
    ##  4 South 11.24a           11.64            12.02              11.88             
    ##  5 West  15.27            15.62            15.53a             14.43             
    ##  6 Alab… 9.98             9.60             9.90               9.71              
    ##  7 Alas… 19.60a           21.92            17.30              18.44             
    ##  8 Ariz… 13.69            13.12            15.12              13.45             
    ##  9 Arka… 11.37            11.59            12.79              12.14             
    ## 10 Cali… 14.49            15.25            15.03              14.11             
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_table_format(html = nsduh_html, table_number = 4)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 1.66a            1.76             0.60               0.64              
    ##  2 Nort… 1.94a            2.18             0.60               0.66              
    ##  3 Midw… 1.37             1.43             0.48               0.54              
    ##  4 South 1.45b            1.56             0.53               0.57              
    ##  5 West  2.03             2.05             0.82               0.85              
    ##  6 Alab… 1.23             1.22             0.42               0.41              
    ##  7 Alas… 1.54a            2.00             0.51               0.65              
    ##  8 Ariz… 2.25             2.29             1.01               0.85              
    ##  9 Arka… 0.93             1.07             0.41               0.48              
    ## 10 Cali… 2.14             2.16             0.89               0.94              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_table_format(html = nsduh_html, table_number = 5)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 0.30             0.33             0.12               0.10              
    ##  2 Nort… 0.43a            0.54             0.13               0.13              
    ##  3 Midw… 0.30             0.31             0.11               0.10              
    ##  4 South 0.27             0.26             0.12               0.08              
    ##  5 West  0.25             0.29             0.13               0.11              
    ##  6 Alab… 0.22             0.27             0.10               0.08              
    ##  7 Alas… 0.70a            1.23             0.11               0.08              
    ##  8 Ariz… 0.32a            0.55             0.17               0.20              
    ##  9 Arka… 0.19             0.17             0.10               0.07              
    ## 10 Cali… 0.20             0.20             0.13               0.09              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

if use for loop:

``` r
nsduh_df = 
  tibble(
    drug = c("marj", "cocaine", "heroin"),
    table_n = c(1, 4, 5)
  ) |>
  mutate(table = map(table_n, nsduh_table_format, html = nsduh_html)) |>
#或者写成：map(table_n, \(x) nsduh_table_format(html = nsduh_html, table_number = x))
  unnest(cols = "table")

nsduh_df |>
  filter(State == "New York")
```

    ## # A tibble: 3 × 13
    ##   drug    table_n State    `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)`
    ##   <chr>     <dbl> <chr>    <chr>            <chr>            <chr>             
    ## 1 marj          1 New York 14.24b           15.04            13.94             
    ## 2 cocaine       4 New York 2.28             2.54             0.71              
    ## 3 heroin        5 New York 0.38a            0.52             0.13              
    ## # ℹ 7 more variables: `12-17(2014-2015)` <chr>, `18-25(2013-2014)` <chr>,
    ## #   `18-25(2014-2015)` <chr>, `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>,
    ## #   `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/linshanxie/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-09-26 11:21:54.721264 (8.651)

    ## file min/max dates: 1869-01-01 / 2024-09-30

    ## using cached file: /Users/linshanxie/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2024-09-26 11:22:00.710929 (3.932)

    ## file min/max dates: 1949-10-01 / 2024-09-30

    ## using cached file: /Users/linshanxie/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2024-09-26 11:22:02.515276 (1.036)

    ## file min/max dates: 1999-09-01 / 2024-09-30

Create a list column

``` r
weather_nest = 
  nest(weather_df, data = date:tmin)
weather_nest
```

    ## # A tibble: 3 × 3
    ##   name           id          data              
    ##   <chr>          <chr>       <list>            
    ## 1 CentralPark_NY USW00094728 <tibble [730 × 4]>
    ## 2 Molokai_HI     USW00022534 <tibble [730 × 4]>
    ## 3 Waterhole_WA   USS0023B17S <tibble [730 × 4]>

``` r
weather_nest[["data"]][[1]]
```

    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01   157   4.4   0.6
    ##  2 2021-01-02    13  10.6   2.2
    ##  3 2021-01-03    56   3.3   1.1
    ##  4 2021-01-04     5   6.1   1.7
    ##  5 2021-01-05     0   5.6   2.2
    ##  6 2021-01-06     0   5     1.1
    ##  7 2021-01-07     0   5    -1  
    ##  8 2021-01-08     0   2.8  -2.7
    ##  9 2021-01-09     0   2.8  -4.3
    ## 10 2021-01-10     0   5    -1.6
    ## # ℹ 720 more rows

use function

``` r
weather_lm = function(df) {
  lm(tmax ~ tmin, data = df)
}
```

``` r
weather_lm(weather_nest[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

use map

``` r
weather_nest |>
  mutate(model_fit = map(data, \(x) lm(tmax~tmin, data = x))) |>
  pull(model_fit)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = x)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = x)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = x)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137
