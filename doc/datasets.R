## ---- setup, include = FALSE--------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
folder <- "literature_files"; dir.create(folder)
download.file("https://www.dropbox.com/s/htnd7o9nnkk8ng8/references.bib?dl=1", paste(folder, "references.bib", sep = "/"))

## ----`BBGsymbols load`--------------------------------------------------------
library(BBGsymbols)

data(list = c("fields", "months", "rolls", "tickers_cftc", "tickers_futures"), package = "BBGsymbols")

## ----`BBGsymbols fields`, echo = FALSE----------------------------------------
tibble::as_tibble(fields)

## ----`BBGsymbols months`, echo = FALSE----------------------------------------
tibble::as_tibble(months)

## ----`BBGsymbols rolls`, echo = FALSE-----------------------------------------
tibble::as_tibble(rolls)

## ----`tickers_CFTC`, echo = FALSE---------------------------------------------
tibble::as_tibble(tickers_cftc)

## ----`tickers_futures`, echo = FALSE------------------------------------------
tibble::as_tibble(tickers_futures)

## ----`fewISOs load`-----------------------------------------------------------
library(fewISOs)

data(list = c("countries", "currencies", "exchanges"), package = "fewISOs")

## ----`fewISOs countries`, echo = FALSE----------------------------------------
tibble::as_tibble(countries)

## ----`fewISOs currencies`, echo = FALSE---------------------------------------
tibble::as_tibble(currencies)

## ----`fewISOs exchanges`, echo = FALSE----------------------------------------
tibble::as_tibble(exchanges)

## ----`GICS load`--------------------------------------------------------------
library(GICS)

data(list = c("standards"), package = "GICS")

## ----`GICS standards`, echo = FALSE-------------------------------------------
tibble::as_tibble(exchanges)

## ----`FFresearch load`--------------------------------------------------------
library(FFresearch)

data(list = c("factors", "portfolios_univariate", "portfolios_bivariate", "portfolios_trivariate",
              "portfolios_industries", "variables", "breakpoints"), package = "FFresearch")

## ---- `FFresearch portfolios_univariate`, echo = FALSE------------------------
tibble::as_tibble(portfolios_univariate)

## ---- `FFresearch portfolios_bivariate`, echo = FALSE-------------------------
tibble::as_tibble(portfolios_bivariate)

## ---- `FFresearch portfolios_trivariate`, echo = FALSE------------------------
tibble::as_tibble(portfolios_trivariate)

## ---- `FFresearchportfolios_industries`, echo = FALSE-------------------------
tibble::as_tibble(portfolios_industries)

## ---- `FFresearch factors`, echo = FALSE--------------------------------------
tibble::as_tibble(factors)

## ---- `FFresearch variables`, echo = FALSE------------------------------------
tibble::as_tibble(variables)

## ---- `FFresearch breakpoints`, echo = FALSE----------------------------------
tibble::as_tibble(breakpoints)

## ---- `factors load`----------------------------------------------------------
library(factors)

data(list = c("Fama & French", "Stambaugh et al"), package = "factors")

## ---- `Fama & French`, echo = FALSE-------------------------------------------
head(`Fama & French`)

## ---- `Stambaugh et al.`, echo = FALSE----------------------------------------
head(`Stambaugh et al`)

