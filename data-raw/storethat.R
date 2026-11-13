# Builds inst/extdata/storethat.sqlite, the storethat database the vignettes
# read. Run from the package root: Rscript data-raw/storethat.R
#
# The database reproduces the author's original development/storethat.sqlite
# with two differences: every number is synthetic, and it holds only the
# tickers and dates (2016-2017) the vignettes use.
#
# - Schema: storethat::db_create(), plus the futures spot tables that storethat's
#   `spot` branch added to the original.
# - Fields: the original's support_fields, kept in data-raw/storethat-fields.csv.
# - Tickers: BBGsymbols, as pullit and storethat use them.
# - Values: simulated by data-raw/storethat-terminal.R, which stands in for the
#   Bloomberg terminal. pullit pulls from it with source = "Bloomberg" and
#   storethat::db_store() writes the result, the same path the original data
#   took.
#
# Needs: storethat, pullit, BBGsymbols, Rblpapi, rugarch, timeDate, testthat.

source("data-raw/storethat-terminal.R")

directory <- "inst/extdata"
file <- file.path(directory, "storethat.sqlite")
start <- "2016-01-01"; end <- "2017-12-31"
# storethat dates reference data with the day it was pulled; pin it.
pulled <- "2018-01-02"

dir.create(directory, recursive = TRUE, showWarnings = FALSE)
unlink(file)
storethat::db_create(path = directory, n = 10L, verbose = FALSE)


# futures spot tables and support fields, as in the original ####

con <- DBI::dbConnect(RSQLite::SQLite(), file)
for (period in sprintf("%02d", 1L:10L)) {
  DBI::dbExecute(con, paste0(
"CREATE TABLE data_futures_spot_", period, "( ticker_id INT UNSIGNED NOT NULL REFERENCES tickers_futures(id) ON UPDATE CASCADE ON DELETE CASCADE NOT DEFERRABLE INITIALLY IMMEDIATE,
                    field_id SMALLINT UNSIGNED REFERENCES support_fields(id) ON UPDATE CASCADE ON DELETE CASCADE NOT DEFERRABLE INITIALLY IMMEDIATE,
                    date_id INT UNSIGNED NOT NULL REFERENCES support_dates(id) ON UPDATE CASCADE ON DELETE CASCADE NOT DEFERRABLE INITIALLY IMMEDIATE,
                    value VARCHAR(20), UNIQUE (ticker_id, field_id, date_id)
    )"))
}
# The original's field list came from an older BBGsymbols plus the spot
# fields; data-raw/storethat-fields.csv is that list, ids included.
DBI::dbExecute(con, "DELETE FROM support_fields")
DBI::dbAppendTable(con, "support_fields",
                   utils::read.csv("data-raw/storethat-fields.csv", colClasses = c(id = "integer")))
DBI::dbDisconnect(con)


# pull from the simulated terminal and store ####

equity_tickers <- equity$ticker
futures_tickers <- futures$ticker
books <- c("key stats", "balance sheet", "cash flow statement", "income statement", "ratios")

with_terminal({
  store <- function(object) storethat::db_store(object = object, file = file, verbose = FALSE)

  store(pullit::pull_equity_market(source = "Bloomberg", equity_tickers, start, end, verbose = FALSE))
  for (book in books) {
    terminal$book <- book
    store(pullit::pull_equity_book(source = "Bloomberg", book = book, equity_tickers, start, end,
                                   verbose = FALSE))
  }
  terminal$book <- NULL
  store(pullit::pull_equity_info(source = "Bloomberg", equity_tickers, verbose = FALSE))

  store(pullit::pull_fund_market(source = "Bloomberg", fund$ticker, start, end, verbose = FALSE))
  store(pullit::pull_fund_info(source = "Bloomberg", fund$ticker, verbose = FALSE))

  store(pullit::pull_futures_market(source = "Bloomberg", type = "aggregate",
                                    active_contract_tickers = futures_tickers, start, end,
                                    verbose = FALSE))
  store(pullit::pull_futures_market(source = "Bloomberg", type = "term structure",
                                    active_contract_tickers = futures_tickers, start, end,
                                    TS_positions = 1L:5L, roll_type = "A", roll_days = 0L,
                                    roll_months = 0L, roll_adjustment = "N", verbose = FALSE))
  store(pullit::pull_futures_CFTC(source = "Bloomberg", active_contract_tickers = futures_tickers,
                                  start, end, verbose = FALSE))
  store(pullit::pull_futures_info(source = "Bloomberg", futures_tickers, verbose = FALSE))
})


# pin the reference-data date and compact ####

con <- DBI::dbConnect(RSQLite::SQLite(), file)
date_id <- DBI::dbGetQuery(con, "SELECT id FROM support_dates WHERE date = ?", params = list(pulled))$id
for (table in c("data_equity_info", "data_fund_info", "data_futures_info", "data_index_info"))
  DBI::dbExecute(con, paste("UPDATE", table, "SET date_id = ?"), params = list(date_id))
DBI::dbExecute(con, "VACUUM")
DBI::dbDisconnect(con)
