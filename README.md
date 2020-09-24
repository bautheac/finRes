finRes
================

<style> body {text-align: justify} </style>

<!-- [![Travis-CI Build Status](https://travis-ci.org/bautheac/finRes.svg?branch=master)](https://travis-ci.org/bautheac/finRes) -->

<!-- [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/bautheac/finRes?branch=master&svg=true)](https://ci.appveyor.com/project/bautheac/finRes) -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## finRes

There are only so many things that can be done in Excel; serious data
munging and further statistical processing not only require flexibility
but also computational power. Programming languages make these
accessible and within that realm, the R programming language stands out
of the crowd for statistical analysis, the workhorse of research in
finance and financial economics. Programming in R can be rather daunting
though. Fetching data requires the user to make API calls; once
retrieved the data have to be stored somewhere which requires the user
not only to communicate with databases but also to set them up entirely
to meet there needs. With the data available comes the time of financial
modelling, the dreadful part for most.  
The [finRes](https://bautheac.github.io/finRes/) suite strives to
abstract complexity away in all these areas. Through a series of
packages it facilitates financial data acquisition from Bloomberg, data
storage and put forwards financial modelling solutions albeit limited to
asset pricing at the time of writing.
[finRes](https://bautheac.github.io/finRes/) leverages the power of the
[tidyverse](www.tidyverse.org) of [Hadley Wickham](http://hadley.nz/)
and the [RStudio](https://www.rstudio.com/) team (Wickham 2017) with the
suite organised as a set of packages that work in harmony because they
share common data representations and ‘API’ design. This package is
designed to make it easy to install and load multiple ‘finRes’ packages
in a single step.  
Install the development version from github with
`devtools::install_github("bautheac/finRes")`.

### Datasets

finRes is home to a number of packages that, although self-contained
with consumption value on their own, host datasets that play important
roles in the finRes suite, mostly in relation to data collection,
storage and wrangling but also to analytics and asset pricing in
particular. At the time of writing, the set of dataset packages in
finRes includes: [BBGsymbols](https://bautheac.github.io/BBGsymbols/),
[fewISOs](https://bautheac.github.io/fewISOs/),
[GICS](https://bautheac.github.io/GICS/),
[factors](https://bautheac.github.io/factors/), and
[FFresearch](https://bautheac.github.io/FFresearch/).

### Bloomberg

The finRes suite is organised along the data-science pipeline where
preprocessing, including data collection and wrangling, plays a major
role and is often reported by data-scientists to amount up to 80% of
work-time. finRes addresses the issue in two complementary packages that
work in conjunction with most of the dataset packages above.  
On the one hand the [pullit](https://bautheac.github.io/pullit/) package
provides tools for data collection from Bloomberg. It returns clean and
tidy, ready-to-use, data objects for other packages further down the
pipeline to work with. On the other hand the
[storethat](https://bautheac.github.io/storethat/) package helps storing
the data retrieved with pullit for off-Bloomberg consumption in R.  
Both pullit and storethat work in tandem with the BBGsymbols package.
The latter plays a central role in finRes where it provides the formers
the wording required to interact with Bloomberg through the interface
provided by the [Whit Armstrong](https://github.com/armstrtw), [Dirk
Eddelbuettel](https://github.com/eddelbuettel) & [John
Laing](https://github.com/johnlaing)’s
[Rblpapi](https://github.com/Rblp/Rblpapi) package (Armstrong,
Eddelbuettel, and Laing 2018).

### Asset pricing

At the time of writing, the analytics part of the pipeline in finRes
focusses on asset pricing.  
On the one hand the FFresearch package abovementioned provides data on
classic asset pricing factors and a number of sort portfolios. The data
is pulled directly from Kenneth French’s [data
library](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html)
and tidied up for seamless consumption in R.  
On the other hand the [factorem](https://bautheac.github.io/factorem/)
package provides tools for outright factor construction from data
retrieve with pullit. The returned objects carry corresponding return &
positions time series and a number of methods help with performance
analysis.

### Visualization

The bottom-end of the pipeline (communication) is addressed in the
[plotit](https://bautheac.github.io/plotit/) package that provides a
number of plot methods for finRes objects.

### Packages

finRes packages at the time of writing:  
\- [BBGsymbols](https://bautheac.github.io/BBGsymbols/): popular
Bloomberg tickers and field symbols conveniently packaged for R users.  
\- [fewISOs](https://bautheac.github.io/fewISOs/): a collection of
financial economics related ISO code datasets conveniently packaged for
consumption in R.  
\- [GICS](https://bautheac.github.io/GICS/): Global Industry
Classification Standard dataset conveniently packaged for consumption in
R.  
\- [FFresearch](https://bautheac.github.io/FFresearch/): Fama/French
asset pricing research data conveniently packaged for consumption by R
users.  
\- [pullit](https://bautheac.github.io/pullit/): Bloomberg financial
data collection in R made easy.  
\- [storethat](https://bautheac.github.io/storethat/): store Bloomberg
financial data for off-Bloomberg consumption in R.  
\- [factorem](https://bautheac.github.io/factorem/): construct bespoke
asset pricing factors.  
\- [plotit](https://bautheac.github.io/plotit/): plot methods for the
finRes suite.

### Going further

See package vignettes for details:

``` r
library(finRes)

vignette(topic = "datasets", package = "finRes")

vignette(topic = "Bloomberg", package = "finRes")

vignette(topic = "asset pricing", package = "finRes")
```

### Coming next

finRes is still in the alpha stage of development; bugs have to be fixed
and some design flaws must be addressed in some of the packages. The
next stage of development will focus on fixing these issues, set up a
CI/CD pipeline for the suite as well as a testing framework. Each
individual package as well as the suite itself will thereafter
eventually be submitted to the Comprehensive R Archive Network (CRAN)
for public dissemination.

## References

<div id="refs" class="references">

<div id="ref-Armstrong_Rblpapi_2018">

Armstrong, Whit, Dirk Eddelbuettel, and John Laing. 2018. *Rblpapi: R
Interface to ’Bloomberg’*. <https://CRAN.R-project.org/package=Rblpapi>.

</div>

<div id="ref-Wickham_tidyverse_2017">

Wickham, Hadley. 2017. *Tidyverse: Easily Install and Load the
’Tidyverse’*. <https://CRAN.R-project.org/package=tidyverse>.

</div>

</div>
