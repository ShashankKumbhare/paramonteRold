
<!-- README.md is generated from README.Rmd. Please edit that file -->

# paramonte

<!-- badges: start -->

<!-- badges: end -->

A serial/parallel library of Monte Carlo routines for sampling
mathematical objective functions of arbitrary-dimensions, in particular,
the posterior distributions of Bayesian models in data science, Machine
Learning, and scientific inference, with the design goal of unifying the
automation (of Monte Carlo simulations), user-friendliness (of the
library), accessibility (from multiple programming environments),
high-performance (at runtime), and scalability (across many parallel
processors).

## Installation

You can install the released version of paramonte from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("paramonte")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(paramonte)
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
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
