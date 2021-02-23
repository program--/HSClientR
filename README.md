
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HSClientR <a href='https://github.com/program--/HSClientR'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build status](https://travis-ci.com/program--/HSClientR.svg?branch=master)](https://travis-ci.com/program--/HSClientR)
[![Codecov test coverage](https://codecov.io/gh/program--/HSClientR/branch/master/graph/badge.svg)](https://codecov.io/gh/program--/HSClientR)
[![R-CMD-check](https://github.com/program--/HSClientR/workflows/R-CMD-check/badge.svg)](https://github.com/program--/HSClientR/actions)
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/HSClientR)](https://CRAN.R-project.org/package=HSClientR) -->
<!-- badges: end -->

HSClientR is an API wrapper for the
[HydroShare](https://www.hydroshare.org) API. Using HSClientR, you can
access resources from HydroShare directly in your R environment.

## Installation

You can install the developmental version of HSClientR with:

``` r
# install.packages("remotes")
remotes::install_github("program--/HSClientR")
```

## Example

This is a basic example which shows you how to access a resource. We’ll
access the resource [Hydrologic Terrain Analysis Using Web Based
Tools](https://www.hydroshare.org/resource/e1d4f2aff7d84f79b901595f6ea48368/):

``` r
library(HSClientR)

# Set your HydroShare username and password for API auth
Sys.setenv(HSCLIENT_USER = "hydroshare-user"
           HSCLIENT_PASS = "hydroshare-pass")

hs_resource(
    full_text_search = "Hydrologic Terrain Analysis Using Web Based Tools",
    author = "David Tarboton"
)

#> # A tibble: 1 x 19
#>   resource_title resource_type resource_id abstract authors creator doi   public
#>   <chr>          <chr>         <chr>       <chr>    <chr>   <chr>   <lgl> <lgl> 
#> 1 Hydrologic Te… CompositeRes… e1d4f2aff7… "Digita… David … David … NA    TRUE  
#> # … with 11 more variables: discoverable <lgl>, shareable <lgl>, immutable <lgl>,
#> #   published <lgl>, date_created <chr>, date_last_updated <chr>, bag_url <chr>,
#> #   science_metadata_url <chr>, resource_map_url <chr>, resource_url <chr>,
#> #   content_types <chr>
```
