## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, eval = FALSE, comment = "#>")

## ---- `portfolios_univariate`, echo = FALSE------------------------------
#  head(portfolios_univariate)

## ---- `portfolios_bivariate`, echo = FALSE-------------------------------
#  head(portfolios_bivariate)

## ---- `portfolios_trivariate`, echo = FALSE------------------------------
#  head(portfolios_trivariate)

## ---- `portfolios_industries`, echo = FALSE------------------------------
#  head(portfolios_industries)

## ---- `factors`, echo = FALSE--------------------------------------------
#  head(factors)

## ---- `variables`, echo = FALSE------------------------------------------
#  head(variables)

## ---- `breakpoints`, echo = FALSE----------------------------------------
#  head(breakpoints)

## ----`pullit equity`, message = FALSE, warning = FALSE-------------------
#  library(pullit); library(lubridate)
#  
#  tickers_equity <- c("ADM US Equity", "CIVI US Equity", "GBX US Equity", "LIND US Equity",
#                      "SERV US Equity", "AE US Equity", "CLGX US Equity", "GDI US Equity",
#                      "LZB US Equity", "SGA US Equity", "AGCO US Equity", "CLR US Equity",
#                      "GHC US Equity", "MAN US Equity", "SITE US Equity", "AJRD US Equity",
#                      "COMM US Equity", "GME US Equity", "MEI US Equity", "SMP US Equity")
#  end <- Sys.Date(); start <- end - years(2L)
#  
#  equity_data_market <- BBG_equity_market(tickers_equity, start, end, verbose = FALSE)
#  
#  get_data(equity_data_market)

## ----`factorem factor`, message = FALSE, warning = FALSE-----------------
#  library(factorem)
#  
#  name <- "factorem"; ranking_period = 1L
#  factorem <- factorem(name = name, data = equity_data_market, ranking_period = ranking_period)

## ----`equity market`, message = FALSE, warning = FALSE-------------------
#  equity_market <- market_factor(data = equity_data_market)

## ----`equity momentum`, message = FALSE, warning = FALSE-----------------
#  ranking_period = 1L
#  equity_momentum <- momentum_factor(data = equity_data_market, ranking_period = ranking_period)

## ----`pullit futures`, message = FALSE, warning = FALSE------------------
#  tickers_futures <- c("C A Comdty", "CCA Comdty", "CLA Comdty", "CTA Comdty", "FCA Comdty",
#                       "GCA Comdty", "HGA Comdty", "HOA Comdty", "KCA Comdty", "KWA Comdty",
#                       "LBA Comdty", "LCA Comdty", "LHA Comdty", "NGA Comdty", "O A Comdty",
#                       "PAA Comdty", "S A Comdty", "SIA Comdty", "W A Comdty", "XBA Comdty")
#  end <- Sys.Date(); start <- end - years(2L)
#  
#  futures_data_TS <- BBG_futures_market(type = "term structure", tickers_futures,
#                                        start, end, verbose = FALSE)
#  
#  get_data(futures_data_TS)

## ----`futures market`, message = FALSE, warning = FALSE------------------
#  futures_market <- market_factor(data = futures_data_TS)

## ----`futures momentum`, message = FALSE, warning = FALSE----------------
#  ranking_period = 1L
#  futures_momentum <- momentum_factor(data = futures_data_TS, ranking_period = ranking_period)

## ----`futures CHP`, message = FALSE, warning = FALSE---------------------
#  futures_data_CFTC <- BBG_futures_CFTC(tickers_futures, start, end, verbose = FALSE)
#  
#  ranking_period = 1L
#  futures_CHP <- CHP_factor(price_data = futures_data_TS, CHP_data = futures_data_CFTC,
#                            ranking_period = ranking_period)

## ----`futures OI nearby`, message = FALSE, warning = FALSE---------------
#  ranking_period = 1L
#  futures_OI_nearby <- OI_nearby_factor(data = futures_data_TS, ranking_period = ranking_period)

## ----`futures OI aggregate`, message = FALSE, warning = FALSE------------
#  futures_data_agg <- BBG_futures_market(type = "aggregate", tickers_futures,
#                                         start, end, verbose = FALSE)
#  
#  ranking_period = 1L
#  futures_OI_aggregate <- OI_aggregate_factor(price_data = futures_data_market,
#                                              aggregate_data = futures_data_agg,
#                                              ranking_period = ranking_period)

## ----`futures TS`, message = FALSE, warning = FALSE----------------------
#  ranking_period = 1L
#  futures_TS <- TS_factor(data = futures_data_market, ranking_period = ranking_period)

## ----`factor name`-------------------------------------------------------
#  get_name(futures_TS)

## ----`factor positions`--------------------------------------------------
#  get_positions(futures_TS)

## ----`factor returns`----------------------------------------------------
#  get_returns(futures_TS)

## ----`factor data`-------------------------------------------------------
#  get_data(futures_TS)

## ----`factor params`-----------------------------------------------------
#  get_parameters(futures_TS)

## ----`factor call`-------------------------------------------------------
#  get_call(futures_TS)

## ----`factor summary`----------------------------------------------------
#  summary(futures_TS)

## ----`plot performance`--------------------------------------------------
#  library(plotit)
#  
#  plot_performance(futures_TS)

## ----`plot positions`----------------------------------------------------
#  plot_positions(futures_TS)

