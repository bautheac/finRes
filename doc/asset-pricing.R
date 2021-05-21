## ---- setup, echo = F, message = F, warning = F-------------------------------
knitr::opts_chunk$set(collapse = T, eval = T, warning = F, message = F, comment = "#>")
folder <- "literature_files"; dir.create(folder)
download.file("https://www.dropbox.com/s/htnd7o9nnkk8ng8/references.bib?dl=1", paste(folder, "references.bib", sep = "/"))
path <- here::here("development", "storethat.sqlite")

## ---- `load FFresearch`, echo = F---------------------------------------------
library(FFresearch)
data(list = c(
  "factors", "portfolios_univariate", "portfolios_bivariate", "portfolios_trivariate",
  "portfolios_industries", "variables", "breakpoints"),
  package = "FFresearch"
  )

## ---- `portfolios_univariate`, echo = F---------------------------------------
head(portfolios_univariate)

## ---- `portfolios_bivariate`, echo = F----------------------------------------
head(portfolios_bivariate)

## ---- `portfolios_trivariate`, echo = F---------------------------------------
head(portfolios_trivariate)

## ---- `portfolios_industries`, echo = F---------------------------------------
head(portfolios_industries)

## ---- `factors`, echo = F-----------------------------------------------------
head(factors)

## ---- `variables`, echo = F---------------------------------------------------
head(variables)

## ---- `breakpoints`, echo = F-------------------------------------------------
head(breakpoints)

## ---- `factors load`----------------------------------------------------------
library(factors)

data(list = c("Fama & French", "Stambaugh et al"), package = "factors")

## ---- `Fama & French`, echo = FALSE-------------------------------------------
head(`Fama & French`)

## ---- `Stambaugh et al.`, echo = FALSE----------------------------------------
head(`Stambaugh et al`)

## ----`pullit equity BBG`, message = F, warning = F, echo = T, eval = F--------
#  library(pullit); library(lubridate)
#  
#  tickers_equity <- c("LZB US Equity", "SGA US Equity", "AGCO US Equity", "CLR US Equity",
#                      "GHC US Equity", "MAN US Equity", "SITE US Equity", "AJRD US Equity",
#                      "COMM US Equity", "GME US Equity", "MEI US Equity", "SMP US Equity")
#  start <- "2016-01-01"; end <- "2017-12-31"
#  
#  equity_data_market <- pull_equity_market(source = "Bloomberg", tickers_equity, start, end, verbose = F)
#  
#  get_data(equity_data_market)

## ----`pullit equity storethat`, message = F, warning = F, echo = F, eval = T----
library(pullit); library(lubridate)

# tickers_equity <- c("ADM US Equity", "CIVI US Equity", "GBX US Equity", "LIND US Equity", 
#                     "SERV US Equity", "AE US Equity", "CLGX US Equity", "GDI US Equity", 
#                     "LZB US Equity", "SGA US Equity", "AGCO US Equity", "CLR US Equity", 
#                     "GHC US Equity", "MAN US Equity", "SITE US Equity", "AJRD US Equity", 
#                     "COMM US Equity", "GME US Equity", "MEI US Equity", "SMP US Equity")
tickers_equity <- c("LZB US Equity", "SGA US Equity", "AGCO US Equity", "CLR US Equity", 
                    "GHC US Equity", "MAN US Equity", "SITE US Equity", "AJRD US Equity", 
                    "COMM US Equity", "GME US Equity", "MEI US Equity", "SMP US Equity")
start <- "2016-01-01"; end <- "2017-12-31"

equity_data_market <- pull_equity_market(
  source = "storethat", tickers = tickers_equity, start = start, end = end, 
  verbose = F, file = path
  )

pullit::get_data(equity_data_market)

## ----`factorem factor`, message = F, warning = F------------------------------
library(factorem)

ranking_period = 1L

factor <- factorem(
  name = "momentum", data = pullit::get_data(equity_data_market),
  ranking_period = ranking_period
  )
factor

## ----`equity market`, message = F, warning = F--------------------------------
equity_market <- market_factor(data = equity_data_market)
equity_market

## ----`equity momentum`, message = F, warning = F------------------------------
equity_momentum <- momentum_factor(data = equity_data_market, ranking_period = ranking_period)
equity_momentum

## ----`pullit futures bbg`, message = F, warning = F, echo = T, eval = F-------
#  tickers_futures <- c("C A Comdty", "CCA Comdty", "CLA Comdty", "CTA Comdty", "FCA Comdty",
#                       "GCA Comdty", "HGA Comdty", "HOA Comdty", "KCA Comdty", "KWA Comdty",
#                       "LBA Comdty", "LCA Comdty", "LHA Comdty", "NGA Comdty", "O A Comdty",
#                       "PAA Comdty", "S A Comdty", "SIA Comdty", "W A Comdty", "XBA Comdty")
#  
#  futures_data_TS <- pull_futures_market(source = "Bloomberg", type = "term structure", tickers_futures,
#                                         start, end, verbose = F)
#  
#  get_data(futures_data_TS)

## ----`pullit futures storethat`, message = F, warning = F, echo = F-----------
tickers_futures <- c(
  "C A Comdty", "CLA Comdty", "COA Comdty", "GCA Comdty", "HOA Comdty",
  "KWA Comdty", "LHA Comdty", "QSA Comdty", "S A Comdty", "SBA Comdty",
  "W A Comdty"
  )

futures_data_TS <- pull_futures_market(
  source = "storethat", type = "term structure", tickers_futures, start, end, 
  verbose = F, file = path
  )

pullit::get_data(futures_data_TS)

## ----`futures market`, message = F, warning = F-------------------------------
futures_market <- market_factor(data = futures_data_TS)
futures_market

## ----`futures momentum`, message = F, warning = F-----------------------------
ranking_period = 1L
futures_momentum <- momentum_factor(data = futures_data_TS, ranking_period = ranking_period)
futures_momentum

## ----`futures CHP bbg`, message = F, warning = F, echo = T, eval = F----------
#  futures_data_CFTC <- pull_futures_CFTC(source = "Bloomberg", tickers_futures, start, end, verbose = F)
#  
#  ranking_period = 1L
#  futures_CHP <- CHP_factor(price_data = futures_data_TS, CHP_data = futures_data_CFTC,
#                            ranking_period = ranking_period)
#  futures_CHP

## ----`futures CHP storethat`, message = F, warning = F, echo = F, eval = T----
futures_data_CFTC <- pull_futures_CFTC(
  source = "storethat", tickers_futures, start, end, verbose = F, file = path
  )

ranking_period = 1L
futures_CHP <- CHP_factor(
  price_data = futures_data_TS, CHP_data = futures_data_CFTC, 
  ranking_period = ranking_period
  )
futures_CHP

## ----`futures OI nearby`, message = F, warning = F----------------------------
ranking_period = 1L
futures_OI_nearby <- OI_nearby_factor(data = futures_data_TS, ranking_period = ranking_period)
futures_OI_nearby

## ----`futures OI aggregate bbg`, message = F, warning = F, echo = T, eval = F----
#  futures_data_agg <- pull_futures_market(source = "Bloomberg", type = "aggregate", tickers_futures,
#                                          start, end, verbose = F)
#  
#  ranking_period = 1L
#  futures_OI_aggregate <- OI_aggregate_factor(price_data = futures_data_market,
#                                              aggregate_data = futures_data_agg,
#                                              ranking_period = ranking_period)
#  futures_OI_aggregate

## ----`futures OI aggregate storethat`, message = F, warning = F, echo = F, eval = T----
futures_data_agg <- pull_futures_market(source = "storethat", type = "aggregate", tickers_futures,
                                        start, end, verbose = F, file = path)

ranking_period = 1L
futures_OI_aggregate <- OI_aggregate_factor(price_data = futures_data_TS, 
                                            aggregate_data = futures_data_agg, 
                                            ranking_period = ranking_period)
futures_OI_aggregate

## ----`futures TS`, message = F, warning = F-----------------------------------
ranking_period = 1L
futures_TS <- TS_factor(data = futures_data_TS, ranking_period = ranking_period)
futures_TS

## ----`factor name`------------------------------------------------------------
get_name(factor)

## ----`factor positions`-------------------------------------------------------
get_positions(factor)

## ----`factor returns`---------------------------------------------------------
get_returns(factor)

## ----`factor data`------------------------------------------------------------
factorem::get_data(factor)

## ----`factor params`----------------------------------------------------------
get_parameters(factor)

## ----`factor call`------------------------------------------------------------
get_call(factor)

## ----`factor summary`---------------------------------------------------------
summary(factor)

## ----`plot performance`, fig.width = 7.5, fig.height = 5.5, fig.fullwidth = T----
library(plotit)

plot(object = factor, type = "performance")

## ----`plot positions`, fig.width = 7.5, fig.height = 6.5, fig.fullwidth = T----
plot(object = factor, type = "positions")

