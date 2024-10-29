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
    ##   [1] 0.55724445 0.25060450 0.27153485 0.32668898 0.32816949 0.86015617
    ##   [7] 0.79290795 0.29106946 0.41380667 0.77893726 0.80341829 0.53710386
    ##  [13] 0.11369871 0.12966279 0.73338750 0.51277775 0.90465455 0.72920566
    ##  [19] 0.96762067 0.65246499 0.43794500 0.29019223 0.38931391 0.87200379
    ##  [25] 0.16253565 0.74508519 0.62412205 0.27233680 0.11276529 0.20741391
    ##  [31] 0.81547092 0.83620206 0.50800340 0.97890662 0.59893286 0.20219106
    ##  [37] 0.72347634 0.67517189 0.06548516 0.16798669 0.95378224 0.21587578
    ##  [43] 0.30985480 0.20784674 0.29323188 0.62797227 0.98498518 0.23826042
    ##  [49] 0.23418362 0.02048814 0.21480756 0.78232018 0.83859905 0.86138832
    ##  [55] 0.99948920 0.77763619 0.84785488 0.95815246 0.88190633 0.57182899
    ##  [61] 0.84209332 0.34571126 0.54987168 0.64492238 0.10995694 0.83934746
    ##  [67] 0.75045817 0.53035979 0.80215305 0.50194701 0.86851297 0.70115536
    ##  [73] 0.49439859 0.96553390 0.39557408 0.30105238 0.22966418 0.42387348
    ##  [79] 0.20944822 0.65866625 0.62142930 0.39441870 0.28827827 0.94994527
    ##  [85] 0.92226214 0.84916383 0.83177813 0.21908913 0.03181017 0.24634853
    ##  [91] 0.24568851 0.80173011 0.17432823 0.52958706 0.26560086 0.55317037
    ##  [97] 0.92053284 0.95102563 0.35075553 0.39100246
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.45628 -0.59958  0.04316  0.02806  0.63092  3.11456

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
    ## -3.45628 -0.59958  0.04316  0.02806  0.63092  3.11456

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

    ##  [1] 13.3089578  3.9545120 12.1972325  8.8372113 -7.2035383  7.3156973
    ##  [7] 11.4675135  3.3585303 -3.7878344  2.6246714  4.2638001  8.1016176
    ## [13]  5.8387567  1.1955888  0.4533213  5.7686605  4.3528190 -0.1072933
    ## [19]  3.8236887  7.8270610

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
    ## 1 -2.33  5.15

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
    ## 1 -2.33  5.15
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.68  5.12
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.44  8.64
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.92  8.82

## Do the same thing

but with `map` instead

``` r
#这个的结果会输出四个表
output = map(list_norms, mean_and_sd)
output
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.33  5.15
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.68  5.12
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.44  8.64
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.92  8.82

``` r
#也素把结果写在一个表里
output = map_dbl(list_norms, IQR)
output
```

    ##         a         b         c         d 
    ##  6.419347  5.628299 12.074782 11.798636

``` r
##把结果写在一个表里
output = map_dfr(list_norms, mean_and_sd)
output
```

    ## # A tibble: 4 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.33  5.15
    ## 2  4.68  5.12
    ## 3  1.44  8.64
    ## 4  5.92  8.82

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

    ##  [1] -3.9736982 -9.8066361 -5.5924459 -4.7487113  8.9503095 -7.9872468
    ##  [7] -8.3763533  4.2695233 -5.4266557 -0.4001963  1.0812269  0.7750976
    ## [13]  1.6637979 -2.4802822 -5.5594737  6.4793061 -2.6442659 -4.7403908
    ## [19]  0.3927250 -8.5176538

``` r
mean_and_sd(listcol_df[["samp"]][["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.68  5.12

``` r
map(listcol_df[["samp"]], mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.33  5.15
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.68  5.12
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.44  8.64
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.92  8.82

ADD A LIST COLUMN

``` r
listcol_df |>
  mutate(output = map(samp, mean_and_sd),
         iqr = map_dbl(samp, IQR)) |>
  select(-samp) |>
  unnest(output)
```

    ## # A tibble: 4 × 4
    ##   name   mean    sd   iqr
    ##   <chr> <dbl> <dbl> <dbl>
    ## 1 a     -2.33  5.15  6.42
    ## 2 b      4.68  5.12  5.63
    ## 3 c      1.44  8.64 12.1 
    ## 4 d      5.92  8.82 11.8

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
