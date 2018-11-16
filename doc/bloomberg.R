## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## ----`globals`, warnings = FALSE, message = FALSE------------------------
#  library(pullit); library(lubridate)
#  
#  end <- Sys.Date() - years(1L); start <- end - years(2L)

## ----`equity market`-----------------------------------------------------
#  equity_tickers <- c("ADM US Equity", "KHC US Equity", "XPO US Equity")
#  
#  equity_market <- BBG_equity_market(equity_tickers, start, end, verbose = FALSE)

## ----`equity balance sheet`----------------------------------------------
#  equity_BS <- BBG_equity_book(book = "balance sheet", equity_tickers, start, end, verbose = FALSE)

## ----`equity cash flow statement`----------------------------------------
#  equity_CF <- BBG_equity_book(book = "cash flow statement", equity_tickers, start, end, verbose = FALSE)

## ----`equity income statement`-------------------------------------------
#  equity_IS <- BBG_equity_book(book = "income statement", equity_tickers, start, end, verbose = FALSE)

## ----`equity key stats`--------------------------------------------------
#  equity_KS <- BBG_equity_book(book = "key stats", equity_tickers, start, end, verbose = FALSE)

## ----`equity ratios`-----------------------------------------------------
#  equity_R <- BBG_equity_book(book = "ratios", equity_tickers, start, end, verbose = FALSE)

## ----`equity info`-------------------------------------------------------
#  equity_info <- BBG_equity_info(equity_tickers, verbose = FALSE)

## ----`fund market`-------------------------------------------------------
#  fund_tickers <- c("SPY US Equity", "GLD US Equity", "EEM US Equity")
#  
#  fund_market <- BBG_fund_market(fund_tickers, start, end, verbose = FALSE)

## ----`fund info`---------------------------------------------------------
#  fund_info <- BBG_fund_info(fund_tickers, verbose = FALSE)

## ----`futures term structure`--------------------------------------------
#  futures_tickers <- c("C A Comdty", "EDA Comdty", "ESA Index")
#  
#  futures_TS <- BBG_futures_market(type = "term structure", active_contract_tickers = futures_tickers,
#                                   start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
#                                   roll_months = 0L, roll_adjustment = "N", verbose = FALSE)

## ----`futures aggregated`------------------------------------------------
#  futures_agg <- BBG_futures_market(type = "aggregate", active_contract_tickers = futures_tickers,
#                                    start, end, verbose = FALSE)

## ----`futures CFTC`------------------------------------------------------
#  futures_CFTC <- BBG_futures_CFTC(active_contract_tickers = futures_tickers, start, end, verbose = FALSE)

## ----`futures info`------------------------------------------------------
#  futures_info <- BBG_futures_info(futures_tickers, verbose = FALSE)

## ----`equity show`-------------------------------------------------------
#  equity_market

## ----`equity get_tickers`------------------------------------------------
#  get_tickers(equity_market)

## ----`equity get_fields`-------------------------------------------------
#  get_fields(equity_market)

## ----`equity get_data`---------------------------------------------------
#  get_data(equity_market)

## ----`equity get_call`---------------------------------------------------
#  get_call(equity_market)

## ----`equity get_periods`------------------------------------------------
#  get_periods(equity_market)

## ----`storethat store`---------------------------------------------------
#  library(storethat)
#  
#  db_create(path = "~/data-raw/")
#  
#  data <- list(equity_market, equity_BS, equity_CF, equity_IS, equity_KS, equity_R, equity_info,
#               fund_market, fund_info, futures_TS, futures_agg, futures_CFTC, futures_info)
#  
#  do.call(db_store, data)

## ----`storethat retrieve`------------------------------------------------
#  equity_market <- storethat_equity_market(equity_tickers, start, end, verbose = FALSE)

## ----`storethat update all`----------------------------------------------
#  storethat_update()

## ----`storethat update some`---------------------------------------------
#  storethat_update(instrument = "equity", book = "market")

## ----`plot term structure`-----------------------------------------------
#  library(plotit)
#  
#  plot_term_structure(object = futures_TS, ticker = "C A Comdty")

## ----`plot performance`--------------------------------------------------
#  plot_performance(object = fund_market, ticker = "GLD US Equity")

