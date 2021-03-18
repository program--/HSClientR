
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HSClientR - A [HydroShare](https://www.hydroshare.org) API Client for R <a href='https://github.com/program--/HSClientR'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.com/program--/HSClientR.svg?branch=master)](https://travis-ci.com/program--/HSClientR)
[![Codecov test
coverage](https://codecov.io/gh/program--/HSClientR/branch/master/graph/badge.svg)](https://codecov.io/gh/program--/HSClientR)
[![R-CMD-check](https://github.com/program--/HSClientR/workflows/R-CMD-check/badge.svg)](https://github.com/program--/HSClientR/actions)
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/HSClientR)](https://CRAN.R-project.org/package=HSClientR) -->
<!-- badges: end -->

HSClientR is an API wrapper for
[HydroShare](https://www.hydroshare.org). Using HSClientR, you can
access resources from HydroShare directly in your R environment, either
by the package functions or the developmental
[`R6 Class`](https://r6.r-lib.org/reference/R6Class.html).

## Installation

You can install the developmental version of HSClientR with:

``` r
# install.packages("remotes")
remotes::install_github("program--/HSClientR")
```

## Example Using Package Functions

This is a basic example which shows you how to access a resource. We’ll
access the resource [Hydrologic Terrain Analysis Using Web Based
Tools](https://www.hydroshare.org/resource/e1d4f2aff7d84f79b901595f6ea48368/),
as well as some additional functionality:

``` r
library(HSClientR)

# To set auth headers
hs_auth(set_headers = TRUE)

# Search for resources
hs_resource(
    text = "Hydrologic Terrain Analysis Using Web Based Tools",
    author = "David Tarboton"
)

# Alternatively, if you want to use specific functions,
# you can call hs_search() with the same parameters

#> $.next
#> [1] NA
#> 
#> $.prev
#> [1] NA
#> 
#> $results
#> # A tibble: 5 x 10
#>   id     title    author abstract   contributor subject  availability content_type
#>   <chr>  <chr>    <chr>  <chr>      <chr>       <chr>    <chr>        <chr>       
#> 1 c1be8… Clearin… Tarbo… "Can your… ""          #gisDay… public       Composite, …
#> 2 1302d… Clearin… Tarbo… "Can your… ""          AGU2015  public       Composite, …
#> 3 e1d4f… Hydrolo… Tarbo… "Digital … ""          TauDEM,… public       Presentatio…
#> 4 66fd9… Materia… Tarbo… "Material… ""          Worksho… public       Presentatio…
#> 5 d752e… The Mod… Tarbo… "Model My… ""          present… public       Presentatio…
#> # … with 2 more variables: metadata <list>, geodata <list>

# Another way of finding resources is via the Discover API.
# This is equivalent to accessing the "Discover" page directly
# on the HydroShare website. It returns the first 40 results.

hs_discover()

#> # A tibble: 40 x 6
#>    title          link          author       abstract          short_id   metadata
#>    <chr>          <chr>         <chr>        <chr>             <chr>      <list>  
#>  1 SUMMA Simulat… https://hydr… Choi, Young… "These are examp… 03dc01d36… <tibble…
#>  2 SUMMA Simulat… https://hydr… Choi, Young… "These are examp… ac54c8046… <tibble…
#>  3 NLDAS Forcing… https://hydr… Choi, Young… "This resource w… a28685d2d… <tibble…
#>  4 SHAW model in… https://hydr… Marshall, A… "This dataset su… 5a355d673… <tibble…
#>  5 Application o… https://hydr… Pedrazas, M… "Fluvio-deltaic … cf3c26339… <tibble…
#>  6 ModelMyWaters… https://hydr… Ensign, Sco… "A watershed mul… a10bfc16d… <tibble…
#>  7 Wasatch Envir… https://hydr… University … "This dataset co… 5057577e8… <tibble…
#>  8 Wasatch Envir… https://hydr… University … "This dataset co… 252980b3b… <tibble…
#>  9 Wasatch Envir… https://hydr… University … "This dataset co… 6445418c7… <tibble…
#> 10 Wasatch Envir… https://hydr… University … "This dataset co… 74dc57ed7… <tibble…
#> # … with 30 more rows

# You can also call hs_resource() with no parameters to do basic pagination searches.
# This is usually preferred over hs_discover().
hs_resource()

#> $.next
#> [1] "https://www.hydroshare.org/hsapi/resource/search?page=2"
#> 
#> $.prev
#> [1] NA
#> 
#> $results
#> # A tibble: 100 x 10
#>    id     title  author  abstract  contributor subject  availability content_type 
#>    <chr>  <chr>  <chr>   <chr>     <chr>       <chr>    <chr>        <chr>        
#>  1 73aae… 00_Ze… Arscot… "Stroud … ""          mmw, mo… public       Geographic F…
#>  2 73ad4… 00_Ze… Gisond… "Second … ""          osi, mo… public       Geographic F…
#>  3 ea93a… 00_Ze… Gisond… "Part of… ""          osi, mo… public       Geographic F…
#>  4 a2a5c… 01_Pe… Gish, … "Aggrega… ""          cbf, mm… public       Geographic F…
#>  5 5002f… 01_Ze… Gisond… "Part of… ""          osi, mo… public       Geographic F…
#>  6 1048d… 02_Ma… Gish, … "Aggrega… ""          cbf, mm… public       Geographic F…
#>  7 b4b3c… 02_Ze… Gisond… "Second … ""          osi, mo… public       Geographic F…
#>  8 fb70e… 02_Ze… Gisond… "Part of… ""          osi, mo… public       Geographic F…
#>  9 74704… 03_Cl… Gish, … "Aggrega… ""          cbf, mm… public       Geographic F…
#> 10 737cf… 03_Mo… Gisond… "Second … ""          osi, mo… public       Geographic F…
#> # … with 90 more rows, and 2 more variables: metadata <list>, geodata <list>

# You can also find resources by HS alphanumeric IDs:
hs_resource(id = "e1d4f2aff7d84f79b901595f6ea48368")

#> # A tibble: 1 x 19
#>   resource_title  resource_type  resource_id abstract authors creator doi   public
#>   <chr>           <chr>          <chr>       <chr>    <chr>   <chr>   <lgl> <lgl> 
#> 1 Hydrologic Ter… CompositeReso… e1d4f2aff7… "Digita… David … David … NA    TRUE  
#> # … with 11 more variables: discoverable <lgl>, shareable <lgl>, immutable <lgl>,
#> #   published <lgl>, date_created <chr>, date_last_updated <chr>, bag_url <chr>,
#> #   science_metadata_url <chr>, resource_map_url <chr>, resource_url <chr>,
#> #   content_types <list>
```

## Example Using R6 HydroShare Client Class

> Note: functionality for queries and discover traversal still need to
> be implemented. The example below may change as the object matures.

``` r
# Create new HSClient object
hs_client <- HSClient$new()

#> R6-based client still in-development.
#> Full functionality may not exist.
#> 
#> HSClientR - R6 Client
#> =======================================
#> Thanks for using HSClientR! ♥
#> You can start a query with $query()
#> Make sure to authenticate with $authenticate()
#> =======================================

hs_client$print()

#> It looks like you haven't authenticated yet...
#> Please call $authenticate() on this object to get
#> OAuth2 authentication set up with HydroShare! :)

# Begin authentication
hs_client$authenticate()

# OAuth2 Dance will begin and a web browser
# will open to HydroShare's login/authorization
# page. Once you login, an access token will be
# sent back to a local httpuv web server for
# your R session.

# Search for a resource
hs_client$query(text = "NHDPlus VAA")

#> # A tibble: 2 x 18
#>   text     author  abstract    contributor subject   availability created modified
#>   <chr>    <chr>   <chr>       <chr>       <chr>     <chr>        <chr>   <chr>   
#> 1 " \n ba… Rea, A… "The NHDPl… ""          NHDPlus,… public       2019-0… 2019-08…
#> 2 " \n 60… Johnso… "These fil… ""          roughnes… public       2020-1… 2021-02…
#> # … with 10 more variables: coverage_type <chr>, east <dbl>, northlimit <dbl>,
#> #   eastlimit <dbl>, southlimit <dbl>, westlimit <dbl>, start_date <lgl>,
#> #   end_date <lgl>, resource_type <chr>, content_type <chr>

# $query uses hs_search() to perform the query,
# and while a tibble is outputted, the object returns
# itself invisibly. Once the query is performed, we can
# print our object to see our last query and its results:

hs_client$print()

#> Authenticated as:
#> Justin Singh-Mohudpur
#> User: jsinghm
#> ID:   8409
#> Org:  University of California, Santa Barbara
#> =======================================
#> Last Query ($query): text = "NHDPlus VAA"
#> Query Results:
#>  
#>  6092c8a62fac45be97a09bfd0b0bf726  
#>  
#>  NHDPlus Value Added Attributes - no geometries  
#>  These files contain a curated set ...
```
