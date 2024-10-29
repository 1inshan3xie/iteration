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

``` r
library(readxl)
```

## writing my first function

as an example, here’s a z-score computation

``` r
x_vec = rnorm(25, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.04542057 -0.48487900 -0.03837685  2.14820838 -1.31407828 -0.23432516
    ##  [7]  1.16459437 -1.66064833  0.82943464  0.37587835 -1.24121460  0.61816121
    ## [13] -0.98940536 -0.52856537 -1.78613754  0.49296229 -0.77590860 -0.01118984
    ## [19]  1.26648295 -0.02103901 -0.26174557  1.15097415  0.17565601  1.43251227
    ## [25] -0.35277167

Now i’ll write a function to do this

``` r
z_score = function(x){
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_score(x = x_vec)
```

    ##  [1]  0.04542057 -0.48487900 -0.03837685  2.14820838 -1.31407828 -0.23432516
    ##  [7]  1.16459437 -1.66064833  0.82943464  0.37587835 -1.24121460  0.61816121
    ## [13] -0.98940536 -0.52856537 -1.78613754  0.49296229 -0.77590860 -0.01118984
    ## [19]  1.26648295 -0.02103901 -0.26174557  1.15097415  0.17565601  1.43251227
    ## [25] -0.35277167

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
    ## 1  10.8  4.23

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
    ## 1  8.46  5.30

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
    ## 1  3.63  9.79

``` r
sim_mean_sd(sample_size = 30, true_mean = 4, true_sd = 12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.67  13.3

``` r
sim_mean_sd(30, 16, 2)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  16.0  2.43

``` r
sim_mean_sd(330)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.2  5.00

## revisit LoTR dataset

``` r
fellowship_df = 
  read_excel("./data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship_ring")

two_towers = readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers")

return_king = readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king")
```

Use a function to solve the problem

``` r
lotr_import = function(cell_range, movie_title){
  movie_df = 
    read_excel("./data/LotR_Words.xlsx", range = cell_range) |>
    mutate(movie = movie_title) |>
    janitor::clean_names() |>
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words"
    ) |>
    select(movie, everything())
  
  return(movie_df)

}
```

``` r
lotr_import(cell_range = "B3:D6", movie_title = "fellowship_ring")
```

    ## # A tibble: 6 × 4
    ##   movie           race   sex    words
    ##   <chr>           <chr>  <chr>  <dbl>
    ## 1 fellowship_ring Elf    female  1229
    ## 2 fellowship_ring Elf    male     971
    ## 3 fellowship_ring Hobbit female    14
    ## 4 fellowship_ring Hobbit male    3644
    ## 5 fellowship_ring Man    female     0
    ## 6 fellowship_ring Man    male    1995

``` r
lotr_df = 
  bind_rows(
    lotr_import(cell_range = "B3:D6", movie_title = "fellowship_ring"),
    lotr_import(cell_range = "F3:H6", movie_title = "two_towers"),
    lotr_import(cell_range = "J3:L6", movie_title = "return_king")
  )
```

## NUSDH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

marj_table = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  mutate(drug = "marj")

cocaine_table = 
  nsduh_html |> 
  html_table() |> 
  nth(4) |>
  slice(-1) |> 
  mutate(drug = "cocaine")

heroin_table = 
  nsduh_html |> 
  html_table() |> 
  nth(5) |>
  slice(-1) |> 
  mutate(drug = "heroin")
```

``` r
nsduh_table_format = function(html, table_number, table_name) {
  out_table = 
    html |> 
    html_table() |> 
    nth(table_number) |>
    slice(-1) |> 
    mutate(drug = table_name)
  
  return(out_table)
}
```

``` r
bind_rows(
nsduh_table_format(nsduh_html, 1, "marj"),
nsduh_table_format(nsduh_html, 4, "cocaine"),
nsduh_table_format(nsduh_html, 5, "heroin"))
```

    ## # A tibble: 168 × 17
    ##    State     `12+(2013-2014)` `12+(2014-2015)` `12+(P Value)` `12-17(2013-2014)`
    ##    <chr>     <chr>            <chr>            <chr>          <chr>             
    ##  1 Total U.… 12.90a           13.36            0.002          13.28b            
    ##  2 Northeast 13.88a           14.66            0.005          13.98             
    ##  3 Midwest   12.40b           12.76            0.082          12.45             
    ##  4 South     11.24a           11.64            0.029          12.02             
    ##  5 West      15.27            15.62            0.262          15.53a            
    ##  6 Alabama   9.98             9.60             0.426          9.90              
    ##  7 Alaska    19.60a           21.92            0.010          17.30             
    ##  8 Arizona   13.69            13.12            0.364          15.12             
    ##  9 Arkansas  11.37            11.59            0.678          12.79             
    ## 10 Californ… 14.49            15.25            0.103          15.03             
    ## # ℹ 158 more rows
    ## # ℹ 12 more variables: `12-17(2014-2015)` <chr>, `12-17(P Value)` <chr>,
    ## #   `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>, `18-25(P Value)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `26+(P Value)` <chr>,
    ## #   `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>, `18+(P Value)` <chr>,
    ## #   drug <chr>
