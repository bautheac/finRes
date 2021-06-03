## ---- setup, echo = F, message = F, warning = F-------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>", warning = F, message = F, eval = T)
folder <- "literature_files"; dir.create(folder)
download.file("https://www.dropbox.com/s/htnd7o9nnkk8ng8/references.bib?dl=1", paste(folder, "references.bib", sep = "/"))
path <- here::here("development", "storethat.sqlite")

## ----`globals bbg`, warnings = F, message = F, eval = F, echo = F-------------
#  library(pullit); library(lubridate)
#  # end <- Sys.Date(); start <- end - years(2L)
#  start <- "2016-01-01"; end <- "2017-12-31"

## ----`globals storethat`, warnings = F, message = F, eval = T-----------------
library(pullit); library(lubridate)

start <- "2016-01-01"; end <- "2017-12-31"

## ----`equity market bbg`, eval = F, echo = T----------------------------------
#  equity_tickers <- c('BP/ LN Equity', 'WEIR LN Equity', 'AAPL US Equity')
#  
#  equity_market <- pull_equity_market(source = "Bloomberg", equity_tickers, start, end, verbose = F)
#  equity_market

## ----`equity market storethat`, eval = T, echo = F----------------------------
equity_tickers <- c('BP/ LN Equity', 'WEIR LN Equity', 'AAPL US Equity')

equity_market <- pull_equity_market(source = "storethat", equity_tickers, start, end, verbose = F, file = path)
equity_market

## ----`equity balance sheet bbg`, eval = F, echo = T---------------------------
#  equity_BS <- pull_equity_book(source = "Bloomberg", book = "balance sheet", equity_tickers,
#                                start, end, verbose = F)
#  equity_BS

## ----`equity balance sheet storethat`, eval = T, echo = F---------------------
equity_BS <- pull_equity_book(source = "storethat", book = "balance sheet", equity_tickers, 
                              start, end, verbose = F, file = path)
equity_BS

## ----`equity cash flow statement bbg`, eval = F, echo = T---------------------
#  equity_CF <- pull_equity_book(source = "Bloomberg", book = "cash flow statement", equity_tickers,
#                                start, end, verbose = F)
#  equity_CF

## ----`equity cash flow statement storethat`, eval = T, echo = F---------------
equity_CF <- pull_equity_book(source = "storethat", book = "cash flow statement", equity_tickers, 
                              start, end, verbose = F, file = path)
equity_CF

## ----`equity income statement bbg`, eval = F, echo = T------------------------
#  equity_IS <- pull_equity_book(source = "Bloomberg", book = "income statement", equity_tickers,
#                                start, end, verbose = F)
#  equity_IS

## ----`equity income statement storethat`, eval = T, echo = F------------------
equity_IS <- pull_equity_book(source = "storethat", book = "income statement", equity_tickers, 
                              start, end, verbose = F, file = path)
equity_IS

## ----`equity key stats bbg`, eval = F, echo = T-------------------------------
#  equity_KS <- pull_equity_book(source = "Bloomberg", book = "key stats", equity_tickers,
#                                start, end, verbose = F)
#  equity_KS

## ----`equity key stats storethat`, eval = T, echo = F-------------------------
equity_KS <- pull_equity_book(source = "storethat", book = "key stats", equity_tickers, 
                              start, end, verbose = F, file = path)
equity_KS

## ----`equity ratios bbg`, eval = F, echo = T----------------------------------
#  equity_R <- pull_equity_book(source = "Bloomberg", book = "ratios", equity_tickers,
#                               start, end, verbose = F)
#  equity_R

## ----`equity ratios storethat`, eval = T, echo = F----------------------------
equity_R <- pull_equity_book(source = "storethat", book = "ratios", equity_tickers, 
                             start, end, verbose = F, file = path)
equity_R

## ----`equity info bbg`, eval = F, echo = T------------------------------------
#  equity_info <- pull_equity_info(source = "Bloomberg", equity_tickers, verbose = F)
#  equity_info

## ----`equity info storethat`, eval = T, echo = F------------------------------
equity_info <- pull_equity_info(source = "storethat", equity_tickers, verbose = F, file = path)
equity_info

## ----`fund market bbg`, eval = F, echo = T------------------------------------
#  fund_tickers <- "SPY US Equity"
#  
#  fund_market <- pull_fund_market(source = "Bloomberg", fund_tickers, start, end, verbose = F)
#  fund_market

## ----`fund market storethat`, eval = T, echo = F------------------------------
fund_tickers <- "SPY US Equity"

fund_market <- pull_fund_market(source = "storethat", tickers = fund_tickers, 
                                start = start, end = end, verbose = F, file = path)
fund_market

## ----`fund info bbg`, eval = F, echo = T--------------------------------------
#  fund_info <- pull_fund_info(source = "Bloomberg", fund_tickers, verbose = F)
#  fund_info

## ----`fund info storethat`, eval = T, echo = F--------------------------------
fund_info <- pull_fund_info(source = "storethat", fund_tickers, verbose = F, file = path)
fund_info

## ----`futures term structure bbg`, echo = T, eval = F-------------------------
#  futures_tickers <- c("C A Comdty", "SIA Comdty")
#  
#  futures_TS <- pull_futures_market(source = "Bloomberg", type = "term structure",
#                                    active_contract_tickers = futures_tickers,
#                                    start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
#                                    roll_months = 0L, roll_adjustment = "N", verbose = F)
#  futures_TS

## ----`futures term structure storethat`, echo = F, eval = T-------------------
futures_tickers <- c("C A Comdty", "SIA Comdty")

futures_TS <- pull_futures_market(source = "storethat", type = "term structure", 
                                  active_contract_tickers = futures_tickers, 
                                  start, end, TS_positions = 1L:5L, roll_type = "A", roll_days = 0L, 
                                  roll_months = 0L, roll_adjustment = "N", verbose = F, file = path)
futures_TS

## ----`futures aggregated bbg`, echo = T, eval = F-----------------------------
#  futures_agg <- pull_futures_market(source = "Bloomberg", type = "aggregate",
#                                     active_contract_tickers = futures_tickers, start, end, verbose = F)
#  futures_agg

## ----`futures aggregated storethat`, echo = F, eval = T-----------------------
futures_agg <- pull_futures_market(source = "storethat", type = "aggregate", 
                                   active_contract_tickers = futures_tickers, start, end, verbose = F, file = path)
futures_agg

## ----`futures CFTC bbg`, echo = T, eval = F-----------------------------------
#  futures_CFTC <- pull_futures_CFTC(source = "Bloomberg", active_contract_tickers = futures_tickers,
#                                    start, end, verbose = F)
#  futures_CFTC

## ----`futures CFTC storethat`, echo = F, eval = T-----------------------------
futures_CFTC <- pull_futures_CFTC(source = "storethat", active_contract_tickers = futures_tickers, 
                                  start, end, verbose = F, file = path)
futures_CFTC

## ----`futures info bbg`, echo = T, eval = F-----------------------------------
#  futures_info <- pull_futures_info(source = "Bloomberg", futures_tickers, verbose = F)
#  futures_info

## ----`futures info storethat`, echo = F, eval = T-----------------------------
futures_info <- pull_futures_info(source = "storethat", futures_tickers, verbose = F, file = path)
futures_info

## ----`futures show`-----------------------------------------------------------
futures_TS

## ----`futures get_tickers`----------------------------------------------------
tickers <- get_active_contract_tickers(futures_TS)
tickers

## ----`futures get_fields`-----------------------------------------------------
fields <- get_fields(futures_TS)
head(fields)

## ----`futures get_data`-------------------------------------------------------
data <- pullit::get_data(futures_TS)
data

## ----`futures get_call`-------------------------------------------------------
call <- pullit::get_call(futures_TS)
call

## ----`storethat store`, eval = F----------------------------------------------
#  library(storethat)
#  
#  db_create()
#  
#  path = "~/storethat.sqlite"
#  
#  db_store(object = futures_TS, file = path, verbose = F)
#  db_store(object = fund_market, file = path, verbose = F)

## ----`storethat retrieve`-----------------------------------------------------
equity_market <- pull_equity_market(source = "storethat", equity_tickers, start, 
                                    end, verbose = F, file = path)
equity_market

## ----`storethat update all`, eval = F-----------------------------------------
#  storethat_update(instrument = "equity", file = path)

## ----`storethat update some`, eval = F----------------------------------------
#  storethat_update(instrument = "equity", book = "market")

## ----`plot term structure`, fig.width = 7.5, fig.height = 5.5, fig.fullwidth = T----
library(plotit)

plot(object = futures_TS, ticker = "C A Comdty")

## ----`plot performance`, fig.width = 7.5, fig.height = 6.5, fig.fullwidth = T----
plot(object = fund_market, ticker = "SPY US Equity")

