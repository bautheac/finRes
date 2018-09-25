[![Travis-CI Build Status](https://travis-ci.org/bautheac/finRes.svg?branch=master)](https://travis-ci.org/bautheac/finRes)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/bautheac/finRes?branch=master&svg=true)](https://ci.appveyor.com/project/bautheac/finRes)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

# finRes

finRes is a set of packages that work in harmony because they share common data representations and 'API' design. It is meant to facilitate data-science and research in finance and financial economics in R. This package is designed to make it easy to install and load multiple 'finRes' packages in a single step.

## Installation

Install the development version from [github](https://github.com/bautheac/finRes/) with:

``` r
devtools::install_github(repo = "finRes", username = "bautheac")
```

## Example

Classic Fama & French asset pricing factors

``` r
library(finRes)
data(list = c("factors"), package = "FFresearch", envir = environment())
```
