## ----setup, echo = FALSE, message = FALSE, warning = FALSE---------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## ----`globals`, warnings = FALSE, message = FALSE, eval = TRUE-----------
library(pullit); library(lubridate)

end <- Sys.Date() - years(1L); start <- end - years(2L)

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

## ----`fund market BBG`---------------------------------------------------
#  fund_tickers <- c("SPY US Equity", "GLD US Equity", "EEM US Equity")
#  
#  fund_market <- BBG_fund_market(fund_tickers, start, end, verbose = FALSE)

## ----`fund market storethat`, echo = FALSE, eval = TRUE------------------
fund_tickers <- c("SPY US Equity", "GLD US Equity", "EEM US Equity")

fund_market <- storethat_fund_market(fund_tickers, start, end, verbose = FALSE)

## ----`fund info`---------------------------------------------------------
#  fund_info <- BBG_fund_info(fund_tickers, verbose = FALSE)

## ----`futures term structure BGG`----------------------------------------
#  futures_tickers <- c("C A Comdty", "EDA Comdty", "ESA Index")
#  
#  futures_TS <- BBG_futures_market(type = "term structure", active_contract_tickers = futures_tickers,
#                                   start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
#                                   roll_months = 0L, roll_adjustment = "N", verbose = FALSE)

## ----`futures term structure storethat`, echo = FALSE, eval = TRUE-------
futures_tickers <- c("C A Comdty", "EDA Comdty", "ESA Index")

futures_TS <- storethat_futures_market(type = "term structure", active_contract_tickers = futures_tickers, 
                                       start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L, 
                                       roll_months = 0L, roll_adjustment = "N", verbose = FALSE)

## ----`futures aggregated`------------------------------------------------
#  futures_agg <- BBG_futures_market(type = "aggregate", active_contract_tickers = futures_tickers,
#                                    start, end, verbose = FALSE)

## ----`futures CFTC`------------------------------------------------------
#  futures_CFTC <- BBG_futures_CFTC(active_contract_tickers = futures_tickers, start, end, verbose = FALSE)

## ----`futures info`------------------------------------------------------
#  futures_info <- BBG_futures_info(futures_tickers, verbose = FALSE)

## ----`futures show`, eval = TRUE-----------------------------------------
futures_TS

## ----`futures get_tickers`, eval = TRUE----------------------------------
get_active_contract_tickers(futures_TS)

## ----`futures get_fields`, eval = TRUE-----------------------------------
get_fields(futures_TS)

## ----`futures get_data`, eval = TRUE-------------------------------------
pullit::get_data(futures_TS)

## ----`futures get_call`, eval = TRUE-------------------------------------
pullit::get_call(futures_TS)

## ----`futures get_periods`, eval = TRUE----------------------------------
get_periods(futures_TS)

## ----`storethat store`---------------------------------------------------
#  library(storethat)
#  
#  db_create()
#  
#  db_store(object = futures_TS, file = "~/storethat.sqlite", verbose = FALSE)
#  db_store(object = fund_market, file = "~/storethat.sqlite", verbose = FALSE)

## ----`storethat retrieve`------------------------------------------------
#  equity_market <- storethat_equity_market(equity_tickers, start, end, verbose = FALSE)

## ----`storethat update all`----------------------------------------------
#  storethat_update(instrument = "equity")

## ----`storethat update some`---------------------------------------------
#  storethat_update(instrument = "equity", book = "market")

## ----`plot term structure`, fig.width = 7.5, fig.height = 5.5, fig.fullwidth = TRUE, eval = TRUE----
library(plotit)

plot_term_structure(object = futures_TS, ticker = "C A Comdty")

## ----`plot performance`, fig.width = 7.5, fig.height = 6.5, fig.fullwidth = TRUE, eval = TRUE----
plot_performance(object = fund_market, ticker = "GLD US Equity")

