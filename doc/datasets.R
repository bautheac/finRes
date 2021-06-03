## ---- setup, include = FALSE--------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
folder <- "literature_files"; dir.create(folder)
download.file("https://www.dropbox.com/s/htnd7o9nnkk8ng8/references.bib?dl=1", paste(folder, "references.bib", sep = "/"))

## ----`BBGsymbols load`--------------------------------------------------------
library(BBGsymbols)

data(list = c("fields", "months", "rolls", "tickers_cftc", "tickers_futures"), package = "BBGsymbols")

## ----`BBGsymbols fields`, echo = FALSE----------------------------------------
# data.table::as.data.table(fields)
tibble::as_tibble(fields)
# head(fields)

## ----`BBGsymbols months`, echo = FALSE----------------------------------------
months

## ----`BBGsymbols rolls`, echo = FALSE-----------------------------------------
rolls

## ----`tickers_CFTC`, echo = FALSE---------------------------------------------
tickers_cftc

## ----`tickers_futures`, echo = FALSE------------------------------------------
# data.table::as.data.table(tickers_futures)
tibble::as_tibble(tickers_futures)
# head(tickers_futures)

## ----`fewISOs load`-----------------------------------------------------------
library(fewISOs)

data(list = c("countries", "currencies", "exchanges"), package = "fewISOs")

## ----`fewISOs countries`, echo = FALSE----------------------------------------
countries

## ----`fewISOs currencies`, echo = FALSE---------------------------------------
currencies

## ----`fewISOs exchanges`, echo = FALSE----------------------------------------
exchanges

## ----`GICS load`--------------------------------------------------------------
library(GICS)

data(list = c("standards"), package = "GICS")

## ----`GICS standards`, echo = FALSE-------------------------------------------
exchanges

## ----`FFresearch load`--------------------------------------------------------
library(FFresearch)

data(list = c("factors", "portfolios_univariate", "portfolios_bivariate", "portfolios_trivariate",
              "portfolios_industries", "variables", "breakpoints"), package = "FFresearch")

## ---- `FFresearch portfolios_univariate`, echo = FALSE------------------------
portfolios_univariate

## ---- `FFresearch portfolios_bivariate`, echo = FALSE-------------------------
portfolios_bivariate

## ---- `FFresearch portfolios_trivariate`, echo = FALSE------------------------
portfolios_trivariate

## ---- `FFresearchportfolios_industries`, echo = FALSE-------------------------
portfolios_industries

## ---- `FFresearch factors`, echo = FALSE--------------------------------------
factors

## ---- `FFresearch variables`, echo = FALSE------------------------------------
variables

## ---- `FFresearch breakpoints`, echo = FALSE----------------------------------
breakpoints

## ---- `factors load`----------------------------------------------------------
library(factors)

data(list = c("fama_french", "stambaugh"), package = "factors")

## ---- fama_french, echo = FALSE-----------------------------------------------
fama_french

## ---- stambaugh, echo = FALSE-------------------------------------------------
stambaugh

