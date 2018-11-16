## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----`BBGsymbols load`---------------------------------------------------
library(BBGsymbols)

data(list = c("fields", "months", "rolls", "tickers_cftc", "tickers_futures"), package = "BBGsymbols")

## ----`BBGsymbols fields`, echo = FALSE-----------------------------------
tibble::as.tibble(fields)

## ----`BBGsymbols months`, echo = FALSE-----------------------------------
tibble::as.tibble(months)

## ----`BBGsymbols rolls`, echo = FALSE------------------------------------
tibble::as.tibble(rolls)

## ----`tickers_cftc`, echo = FALSE----------------------------------------
tibble::as.tibble(tickers_cftc)

## ----`tickers_futures`, echo = FALSE-------------------------------------
tibble::as.tibble(tickers_futures)

## ----`fewISOs load`------------------------------------------------------
library(fewISOs)

data(list = c("countries", "currencies", "exchanges"), package = "fewISOs")

## ----`fewISOs countries`, echo = FALSE-----------------------------------
tibble::as.tibble(countries)

## ----`fewISOs currencies`, echo = FALSE----------------------------------
tibble::as.tibble(currencies)

## ----`fewISOs exchanges`, echo = FALSE-----------------------------------
tibble::as.tibble(exchanges)

## ----`GICS load`---------------------------------------------------------
library(GICS)

data(list = c("standards"), package = "GICS")

## ----`GICS standards`, echo = FALSE--------------------------------------
tibble::as.tibble(exchanges)

## ----`FFresearch load`---------------------------------------------------
library(FFresearch)

data(list = c("factors", "portfolios_univariate", "portfolios_bivariate", "portfolios_trivariate",
              "portfolios_industries", "variables", "breakpoints"), package = "FFresearch")

## ---- `FFresearch portfolios_univariate`, echo = FALSE-------------------
tibble::as.tibble(portfolios_univariate)

## ---- `FFresearch portfolios_bivariate`, echo = FALSE--------------------
tibble::as.tibble(portfolios_bivariate)

## ---- `FFresearch portfolios_trivariate`, echo = FALSE-------------------
tibble::as.tibble(portfolios_trivariate)

## ---- `FFresearchportfolios_industries`, echo = FALSE--------------------
tibble::as.tibble(portfolios_industries)

## ---- `FFresearch factors`, echo = FALSE---------------------------------
tibble::as.tibble(factors)

## ---- `FFresearch variables`, echo = FALSE-------------------------------
tibble::as.tibble(variables)

## ---- `FFresearch breakpoints`, echo = FALSE-----------------------------
tibble::as.tibble(breakpoints)

