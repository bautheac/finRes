finRes
================

<style> body {text-align: justify} </style>

<!-- [![Travis-CI Build Status](https://travis-ci.org/bautheac/finRes.svg?branch=master)](https://travis-ci.org/bautheac/finRes) -->

<!-- [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/bautheac/finRes?branch=master&svg=true)](https://ci.appveyor.com/project/bautheac/finRes) -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- There are only so many things that can be done in Excel; serious data munging and further statistical processing not only require flexibility but also computational power. Programming languages make these accessible and within that realm, the R programming language stands out of the crowd for statistical analysis, the workhorse of research in finance and financial economics. Programming in R can be rather daunting though. Fetching data requires the user to make API calls; once retrieved the data have to be stored somewhere which requires the user not only to communicate with databases but also to set them up entirely to meet there needs. With the data available comes the time of financial modelling, the dreadful part for most.   -->

<!-- The [finRes](https://bautheac.github.io/finRes/) suite strives to abstract complexity away in all these areas. Through a series of packages it facilitates financial data acquisition from Bloomberg, data storage and put forwards financial modelling solutions albeit limited to asset pricing at the time of writing. [finRes](https://bautheac.github.io/finRes/) leverages the power of the [tidyverse](www.tidyverse.org) of [Hadley Wickham](http://hadley.nz/) and the [RStudio](https://www.rstudio.com/) team [@Wickham_tidyverse_2017] with the suite organised as a set of packages that work in harmony because they share common data representations and 'API' design. This package is designed to make it easy to install and load multiple 'finRes' packages in a single step.   -->

<!-- Install the development version from github with `devtools::install_github("bautheac/finRes")`.   -->

The [finRes](https://bautheac.github.io/finRes/) suite provides a
collection of packages developed to facilitate data-science and/or
research in finance and financial economics. In particular, it provides
helper packages for retrieving and storing locally financial data from
Bloomberg as well as for processing this data further for financial
modeling for example, although the capabilities of the current version
are limited to asset pricing.
[finRes](https://bautheac.github.io/finRes/) is organised as a set of
packages that work in harmony because they share common data
representations and ‘API’ design. This package is designed to make it
easy to install and load multiple
‘[finRes](https://bautheac.github.io/finRes/)’ packages in a single
step.  
The development version can be installed from github using
[devtools](https://devtools.r-lib.org/) with
`devtools::install_github("bautheac/finRes")`.

### Datasets

[finRes](https://bautheac.github.io/finRes/) is home to a number of
packages that, although self-contained with consumption value on their
own, host datasets that play important roles in the
[finRes](https://bautheac.github.io/finRes/) suite, mostly in relation
to data collection, storage and wrangling but also to analytics and
asset pricing in particular. At the time of writing, the set of dataset
packages in [finRes](https://bautheac.github.io/finRes/) includes:
[BBGsymbols](https://bautheac.github.io/BBGsymbols/),
[fewISOs](https://bautheac.github.io/fewISOs/),
[GICS](https://bautheac.github.io/GICS/),
[FFresearch](https://bautheac.github.io/FFresearch/) and
[factors](https://bautheac.github.io/factors/).

### Bloomberg

The [finRes](https://bautheac.github.io/finRes/) suite is organised
around the data-science pipeline where preprocessing, including data
collection and wrangling, plays a major role.
[finRes](https://bautheac.github.io/finRes/) addresses the issue in two
complementary packages that work in conjunction with most of the dataset
packages above.  
On the one hand the [pullit](https://bautheac.github.io/pullit/) package
provides tools for data collection from Bloomberg. It returns clean and
tidy, ready-to-use, data objects for other packages further down the
pipeline to work with. On the other hand the
[storethat](https://bautheac.github.io/storethat/) package helps storing
the data retrieved with [pullit](https://bautheac.github.io/pullit/) for
off-Bloomberg consumption in R.  
Both [pullit](https://bautheac.github.io/pullit/) and
[storethat](https://bautheac.github.io/storethat/) work in tandem with
the [BBGsymbols](https://bautheac.github.io/BBGsymbols/) package. The
latter plays a central role in
[finRes](https://bautheac.github.io/finRes/) where it provides the
former the semantic required to interact with Bloomberg through the
interface provided by the [Rblpapi](https://github.com/Rblp/Rblpapi)
package (Armstrong, Eddelbuettel, and Laing 2021).

### Asset pricing

At the time of writing, the analytics part of the pipeline in
[finRes](https://bautheac.github.io/finRes/) focuses on asset pricing.  
On the one hand the [FFresearch](https://bautheac.github.io/FFresearch/)
package abovementioned provides data on classic asset pricing factors
and a number of sort portfolios. The data is pulled directly from
Kenneth French’s [data
library](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html)
and tidied up for seamless consumption in R. The
[factors](https://bautheac.github.io/factors/) package provides
complementary datasets for other popular factors in the literature
including those developed by Robert F. Stambaugh and collaborators:
Lubos Pastor, Yu Yuan, etc. On the other hand the
[factorem](https://bautheac.github.io/factorem/) package provides tools
for outright factor construction from data retrieved with
[pullit](https://bautheac.github.io/pullit/). The returned objects carry
corresponding return & positions time series and a number of methods to
help with performance analysis.

### Visualization

The bottom-end of the pipeline (communication) is addressed in the
[plotit](https://bautheac.github.io/plotit/) package that provides a
number of plot methods for [finRes](https://bautheac.github.io/finRes/)
objects.

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
\- [factors](https://bautheac.github.io/factors/): Clean time series
datasets for asset pricing factors popular in the literature.  
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

[finRes](https://bautheac.github.io/finRes/) is still in the alpha stage
of development; bugs have to be fixed and design flaws must be addressed
in some of the packages. Once these issues are addressed each individual
package as well as the suite itself will eventually be submitted to the
Comprehensive R Archive Network ([CRAN](https://cran.r-project.org/))
for larger public dissemination.

## References

<div id="refs" class="references">

<div id="ref-Armstrong_Rblpapi">

Armstrong, Whit, Dirk Eddelbuettel, and John Laing. 2021. *Rblpapi: R
Interface to ’Bloomberg’*. <https://CRAN.R-project.org/package=Rblpapi>.

</div>

</div>
