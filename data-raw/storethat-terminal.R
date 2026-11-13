# A simulated Bloomberg terminal for data-raw/storethat.R.
#
# Every number served here is synthetic. Names, tickers, exchange codes, GICS
# codes and futures contract specifications are public reference data; the
# tickers and contract specifications come from BBGsymbols. Price levels and
# company sizes are set to plausible orders of magnitude for 2016-2017 and
# everything else is simulated:
#
# - daily returns follow GARCH(1,1) paths with Student-t innovations
#   (rugarch::ugarchpath) in a one-factor structure (market or sector factor
#   plus an idiosyncratic GARCH path), giving fat tails, volatility
#   clustering and cross-sectional correlation;
# - futures term structures are built contract by contract from a spot factor
#   and a mean-reverting carry, so generic tickers roll and show contango or
#   backwardation;
# - quarterly fundamentals come from a small accounting model per firm, so
#   statements, key stats and ratios agree with each other and with prices;
# - CFTC positions are split by trader category so that long and short sides
#   both add up to open interest, with managed money chasing trends and
#   commercials taking the other side.
#
# `with_terminal()` mocks Rblpapi::bdh(), bdp(), blpConnect() and
# blpDisconnect() so that pullit's own Bloomberg code builds the objects that
# storethat::db_store() then writes.

`%>%` <- magrittr::`%>%`

utils::data(list = c("fields", "tickers_futures", "tickers_cftc"), package = "BBGsymbols",
            envir = environment())


# calendar ####

business_days <- function(start, end, holidays = as.Date(character())) {
  days <- seq(as.Date(start), as.Date(end), by = "day")
  days[! format(days, "%u") %in% c("6", "7") & ! days %in% holidays]
}

years <- 2015L:2018L
holidays <- list(
  US = as.Date(timeDate::holidayNYSE(years)),
  UK = as.Date(timeDate::holidayLONDON(years)),
  futures = as.Date(timeDate::holidayNYSE(years))
)

sim_start <- as.Date("2016-01-01"); sim_end <- as.Date("2017-12-31")
days <- lapply(holidays, function(h) business_days(sim_start, sim_end, h))


# GARCH(1,1) Student-t daily log returns ####

garch_returns <- function(n, vol, drift = 0, shape = 6, alpha = 0.08, persistence = 0.97) {
  variance <- vol ^ 2 / 252
  spec <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1L, 1L)),
    mean.model = list(armaOrder = c(0L, 0L), include.mean = TRUE),
    distribution.model = "std",
    fixed.pars = list(mu = drift / 252, omega = variance * (1 - persistence), alpha1 = alpha,
                      beta1 = persistence - alpha, shape = shape)
  )
  path <- rugarch::ugarchpath(spec, n.sim = n, n.start = 250L, m.sim = 1L,
                              rseed = sample.int(.Machine$integer.max, 1L))
  as.numeric(rugarch::fitted(path))
}

factor_returns <- function(factor, beta, vol, factor_vol, drift = 0) {
  idio_vol <- sqrt(max(vol ^ 2 - beta ^ 2 * factor_vol ^ 2, 0.1 ^ 2))
  beta * factor + garch_returns(length(factor), idio_vol, drift)
}

ar1 <- function(n, phi, sd) as.numeric(stats::filter(stats::rnorm(n, sd = sd), phi, method = "recursive"))

round_to <- function(x, tick) round(round(x / tick) * tick, 6L)

# open, high, low, bid, ask and mid around a close series
bar <- function(close, returns, tick) {
  n <- length(close); sigma <- pmax(abs(returns), stats::sd(returns) / 2)
  open <- c(close[1L], utils::head(close, -1L)) * exp(stats::rnorm(n, sd = 0.3 * sigma))
  high <- pmax(open, close) * exp(abs(stats::rnorm(n, sd = 0.5 * sigma)))
  low <- pmin(open, close) * exp(-abs(stats::rnorm(n, sd = 0.5 * sigma)))
  spread <- pmax(tick, close * stats::runif(n, 2e-4, 8e-4))
  data.frame(
    PX_OPEN = round_to(open, tick), PX_HIGH = round_to(high, tick), PX_LOW = round_to(low, tick),
    PX_LAST = round_to(close, tick), PX_BID = round_to(close - spread / 2, tick),
    PX_ASK = round_to(close + spread / 2, tick), PX_MID = round_to(close, tick / 2)
  )
}

volume <- function(returns, base) {
  shock <- abs(returns) / stats::sd(returns)
  round(base * exp(ar1(length(returns), 0.6, 0.25) + 0.25 * (shock - 1)))
}


# equities ####

equity <- tibble::tribble(
  ~ticker,           ~name,                         ~country, ~mic,   ~crncy, ~gics,      ~price, ~shares, ~revenue, ~fx,  ~vol, ~beta, ~gross, ~oper,  ~payout, ~turnover, ~ppe, ~lev, ~volume,  ~listed,
  "BP/ LN Equity",   "BP PLC",                      "GB",     "XLON", "GBp",  "10102010", 350,    18400,   180000,   1.35, 0.28, 1.1,   0.14,   0.03,   0.95,    0.70,      0.55, 0.22, 40e6,     NA,
  "WEIR LN Equity",  "Weir Group PLC/The",          "GB",     "XLON", "GBp",  "20106020", 1000,   218,     1950,     1.00, 0.38, 1.3,   0.32,   0.10,   0.45,    0.75,      0.20, 0.28, 0.9e6,    NA,
  "AAPL US Equity",  "Apple Inc",                   "US",     "XNGS", "USD",  "45202030", 105,    5500,    220000,   1.00, 0.23, 1.1,   0.385,  0.27,   0.25,    0.70,      0.10, 0.25, 35e6,     NA,
  "LZB US Equity",   "La-Z-Boy Inc",                "US",     "XNYS", "USD",  "25201020", 24,     50,      1550,     1.00, 0.28, 1.2,   0.42,   0.08,   0.25,    1.60,      0.20, 0.05, 0.5e6,    NA,
  "SGA US Equity",   "Saga Communications Inc",     "US",     "XNGS", "USD",  "25401020", 42,     5.9,     140,      1.00, 0.25, 0.8,   0.35,   0.18,   0.50,    0.65,      0.25, 0.10, 0.02e6,   NA,
  "AGCO US Equity",  "AGCO Corp",                   "US",     "XNYS", "USD",  "20106015", 45,     87,      7400,     1.00, 0.25, 1.3,   0.20,   0.05,   0.25,    1.00,      0.20, 0.25, 0.9e6,    NA,
  "CLR US Equity",   "Continental Resources Inc/OK","US",     "XNYS", "USD",  "10102020", 22,     372,     2000,     1.00, 0.55, 1.8,   0.55,   0.05,   0.00,    0.14,      0.85, 0.45, 3e6,      NA,
  "GHC US Equity",   "Graham Holdings Co",          "US",     "XNYS", "USD",  "25302010", 480,    5.7,     2500,     1.00, 0.22, 0.9,   0.40,   0.06,   0.15,    0.45,      0.20, 0.12, 0.03e6,   NA,
  "MAN US Equity",   "ManpowerGroup Inc",           "US",     "XNYS", "USD",  "20202010", 82,     72,      19500,    1.00, 0.25, 1.3,   0.17,   0.04,   0.25,    2.40,      0.03, 0.12, 0.8e6,    NA,
  "SITE US Equity",  "SiteOne Landscape Supply Inc","US",     "XNYS", "USD",  "20107010", 21,     39,      1650,     1.00, 0.30, 1.2,   0.32,   0.05,   0.00,    1.40,      0.08, 0.35, 0.4e6,    "2016-05-12",
  "AJRD US Equity",  "Aerojet Rocketdyne Holdings Inc", "US", "XNYS", "USD",  "20101010", 15.5,   64,      1750,     1.00, 0.30, 1.0,   0.15,   0.05,   0.00,    0.80,      0.15, 0.30, 0.6e6,    NA,
  "COMM US Equity",  "CommScope Holding Co Inc",    "US",     "XNGS", "USD",  "45201020", 25,     192,     4900,     1.00, 0.30, 1.3,   0.40,   0.12,   0.00,    0.70,      0.10, 0.50, 1.5e6,    NA,
  "GME US Equity",   "GameStop Corp",               "US",     "XNYS", "USD",  "25504050", 28,     104,     8900,     1.00, 0.38, 1.1,   0.32,   0.06,   0.35,    1.80,      0.08, 0.12, 3e6,      NA,
  "MEI US Equity",   "Methode Electronics Inc",     "US",     "XNYS", "USD",  "45203015", 31,     38,      800,      1.00, 0.30, 1.2,   0.27,   0.13,   0.20,    1.20,      0.20, 0.03, 0.25e6,   NA,
  "SMP US Equity",   "Standard Motor Products Inc", "US",     "XNYS", "USD",  "25101010", 37,     23,      1060,     1.00, 0.26, 1.0,   0.30,   0.09,   0.30,    1.30,      0.10, 0.10, 0.15e6,   NA
)

simulate_markets <- function() {
  us <- garch_returns(length(days$US), 0.13, 0.12)
  uk_on_us <- us[match(days$UK, days$US)]; uk_on_us[is.na(uk_on_us)] <- 0
  list(US = us, UK = 0.5 * uk_on_us + garch_returns(length(days$UK), 0.12, 0.06))
}

# inventory in quarters of cost of revenue; revenue per employee in millions
equity$inventory <- c(0.3, 0.9, 0.1, 0.6, 0, 1.2, 0.1, 0.1, 0, 1.0, 0.4, 0.7, 1.2, 0.5, 1.3)
equity$rev_per_employee <- c(2.5, 0.13, 1.9, 0.16, 0.13, 0.36, 1.6, 0.15, 0.7, 0.4, 0.3, 0.2, 0.17, 0.15, 0.25)

simulate_equity_market <- function(market) {
  out <- lapply(seq_len(nrow(equity)), function(i) {
    e <- equity[i, ]; region <- if (e$country == "GB") "UK" else "US"
    date <- days[[region]]
    r <- factor_returns(market[[region]], e$beta, e$vol, if (region == "UK") 0.14 else 0.13)
    keep <- if (is.na(e$listed)) rep(TRUE, length(date)) else date >= as.Date(e$listed)
    date <- date[keep]; r <- r[keep]; r[1L] <- 0
    tick <- if (e$crncy == "GBp") 0.5 else 0.01
    close <- e$price * exp(cumsum(r))
    quarter <- as.integer(format(date, "%Y")) * 4L + (as.integer(format(date, "%m")) - 1L) %/% 3L
    buyback <- stats::rnorm(length(unique(quarter)), if (e$payout > 0.2) -0.004 else 0.002, 0.003)
    shares <- e$shares * exp(cumsum(buyback))[match(quarter, unique(quarter))]
    px <- bar(close, r, tick)
    cbind(date = date, px,
          PX_VOLUME = volume(r, e$volume),
          EQY_SH_OUT = round(shares, 3L),
          CUR_MKT_CAP = round(px$PX_LAST * shares * 1e6 / if (e$crncy == "GBp") 100 else 1, -3L))
  })
  stats::setNames(out, equity$ticker)
}


# fundamentals: quarterly accounting model ####

quarter_ends <- seq(as.Date("2015-04-01"), as.Date("2018-01-01"), by = "quarter") - 1L

simulate_fundamentals <- function(e, market) {
  n <- length(quarter_ends)
  growth <- cumsum(stats::rnorm(n, 0.03 / 4, 0.03))
  season <- 1 + 0.04 * sin(2 * pi * (seq_len(n) / 4))
  rev <- e$revenue / 4 * exp(growth) * season
  gross <- e$gross + stats::rnorm(n, 0, 0.008)
  oper <- if (e$ticker == "CLR US Equity") seq(-0.30, 0.20, length.out = n) + stats::rnorm(n, 0, 0.02)
          else e$oper + stats::rnorm(n, 0, 0.01)
  da_ratio <- if (e$ppe > 0.5) 0.25 * e$ppe else 0.03
  cogs <- rev * (1 - gross); gp <- rev - cogs; opinc <- rev * oper; opex <- gp - opinc
  da <- rev * da_ratio; ebitda <- opinc + da
  assets <- 4 * rev / e$turnover * (1 + stats::rnorm(n, 0, 0.01))
  debt <- assets * e$lev; interest <- debt * 0.045 / 4
  pretax <- opinc - interest; tax <- pmax(pretax, 0) * 0.28; ni <- pretax - tax
  # balance sheet
  cash <- assets * if (e$ticker == "AAPL US Equity") 0.22 else 0.07
  receivables <- rev * 0.75; inventory <- cogs * e$inventory
  other_ca <- assets * 0.03; cur_assets <- cash + receivables + inventory + other_ca
  ppe <- assets * e$ppe; other_lt <- assets - cur_assets - ppe; noncur_assets <- ppe + other_lt
  st_debt <- debt * 0.15; lt_debt <- debt * 0.85
  payables <- cogs * 0.6; other_cl <- assets * 0.05
  cur_liab <- payables + st_debt + other_cl
  other_ncl <- assets * 0.08; noncur_liab <- lt_debt + other_ncl
  liab <- cur_liab + noncur_liab; equity_ <- assets - liab
  # shares and price at quarter end
  sh <- stats::approx(market$date, market$EQY_SH_OUT, quarter_ends, rule = 2)$y
  px <- stats::approx(market$date, market$PX_LAST, quarter_ends, rule = 2)$y
  px_ccy <- px * e$fx / if (e$crncy == "GBp") 100 else 1
  mcap <- px_ccy * sh
  dps <- pmax(e$payout * ni / sh, 0); if (e$payout == 0) dps[] <- 0
  dps <- round(stats::ave(dps, (seq_len(n) - 1L) %/% 4L, FUN = mean), 3L)
  # cash flow
  wc <- receivables + inventory - payables
  dwc <- -c(0, diff(wc)); cfo <- ni + da + dwc
  capex <- -rev * if (e$ppe > 0.5) 0.35 * e$ppe else 0.035
  cfi <- capex - abs(stats::rnorm(n, 0, 0.005)) * rev
  dividends <- -dps * sh; ddebt <- c(0, diff(debt))
  buyback <- -pmax(-c(0, diff(sh)), 0) * px_ccy
  cff <- dividends + ddebt + buyback; dcash <- cfo + cfi + cff
  fcf <- cfo + capex
  ttm <- function(x) as.numeric(stats::filter(x, rep(1, 4L), sides = 1L))
  yoy <- function(x) 100 * (x / c(rep(NA, 4L), utils::head(x, -4L)) - 1)
  seq_g <- function(x) 100 * (x / c(NA, utils::head(x, -1L)) - 1)
  ev <- mcap + debt - cash
  values <- list(
    SALES_REV_TURN = rev, IS_COGS_TO_FE_AND_PP_AND_G = cogs, GROSS_PROFIT = gp,
    IS_OPERATING_EXPN = opex, IS_OPER_INC = opinc, EBIT = opinc, EBITDA = ebitda, IS_DEPR_EXP = da,
    IS_INC_TAX_EXP = tax, IS_INC_BEF_XO_ITEM = ni, NET_INCOME = ni, EARN_FOR_COMMON = ni,
    IS_INT_EXPENSE = interest, IS_TOT_CASH_COM_DVD = -dividends,
    IS_AVG_NUM_SH_FOR_EPS = sh, IS_SH_FOR_DILUTED_EPS = sh * 1.01, BS_SH_OUT = sh,
    IS_EARN_BEF_XO_ITEMS_PER_SH = ni / sh, IS_DIL_EPS_BEF_XO = ni / (sh * 1.01),
    GROSS_MARGIN = 100 * gross, OPER_MARGIN = 100 * opinc / rev, PROF_MARGIN = 100 * ni / rev,
    EBITDA_TO_REVENUE = 100 * ebitda / rev, PRETAX_INC_TO_NET_SALES = 100 * pretax / rev,
    EQY_DPS = dps,
    `C&CE_AND_STI_DETAILED` = cash, BS_ACCT_NOTE_RCV = receivables, BS_INVENTORIES = inventory,
    OTHER_CURRENT_ASSETS_DETAILED = other_ca, BS_CUR_ASSET_REPORT = cur_assets,
    CASH_AND_MARKETABLE_SECURITIES = cash,
    BS_NET_FIX_ASSET = ppe, BS_OTHER_ASSETS_DEF_CHRG_OTHER = other_lt,
    BS_TOT_NON_CUR_ASSET = noncur_assets, BS_TOT_ASSET = assets,
    `ACCT_PAYABLE_&_ACCRUALS_DETAILED` = payables, BS_ST_BORROW = st_debt,
    OTHER_CURRENT_LIABS_SUB_DETAILED = other_cl, BS_CUR_LIAB = cur_liab, BS_LT_BORROW = lt_debt,
    OTHER_NONCUR_LIABS_SUB_DETAILED = other_ncl, NON_CUR_LIAB = noncur_liab, BS_TOT_LIAB2 = liab,
    TOTAL_EQUITY = equity_, TOT_LIAB_AND_EQY = assets, BS_TOT_CAP = equity_ + debt,
    NET_DEBT = debt - cash, SHORT_AND_LONG_TERM_DEBT = debt,
    NET_DEBT_TO_SHRHLDR_EQTY = 100 * (debt - cash) / equity_, CUR_RATIO = cur_assets / cur_liab,
    QUICK_RATIO = (cash + receivables) / cur_liab,
    NUM_OF_EMPLOYEES = round(4 * rev / e$rev_per_employee),
    CF_NET_INC = ni, CF_DEPR_AMORT = da, CF_CHNG_NON_CASH_WORK_CAP = dwc, CF_CASH_FROM_OPER = cfo,
    `CHG_IN_FXD_&_INTANG_AST_DETAILED` = capex, CAPITAL_EXPEND = capex, CF_CASH_FROM_INV_ACT = cfi,
    CF_DVD_PAID = dividends, PROC_FR_REPAYMNTS_BOR_DETAILED = ddebt,
    PROC_FR_REPURCH_EQTY_DETAILED = buyback, CFF_ACTIVITIES_DETAILED = cff,
    CF_CASH_FROM_FNC_ACT = cff, CF_NET_CHNG_CASH = dcash, CF_FREE_CASH_FLOW = fcf,
    FREE_CASH_FLOW_PER_SH = fcf / sh, CASH_FLOW_PER_SH = cfo / sh,
    TRAIL_12M_NET_SALES = ttm(rev), TRAIL_12M_EBITDA = ttm(ebitda), TRAIL_12M_OPER_INC = ttm(opinc),
    TRAIL_12M_FREE_CASH_FLOW = ttm(fcf),
    HISTORICAL_MARKET_CAP = mcap, DILUTED_MKT_CAP = mcap * 1.01, ENTERPRISE_VALUE = ev,
    PE_RATIO = ifelse(ttm(ni) > 0, mcap / ttm(ni), NA), PX_TO_BOOK_RATIO = mcap / equity_,
    PX_TO_SALES_RATIO = mcap / ttm(rev), PX_TO_CASH_FLOW = mcap / ttm(cfo),
    PX_TO_FREE_CASH_FLOW = ifelse(ttm(fcf) > 0, mcap / ttm(fcf), NA),
    EV_TO_T12M_SALES = ev / ttm(rev), EV_TO_T12M_EBITDA = ifelse(ttm(ebitda) > 0, ev / ttm(ebitda), NA),
    PX_LAST = px, BOOK_VAL_PER_SH = equity_ / sh, REVENUE_PER_SH = ttm(rev) / sh,
    EBITDA_PER_SH = ttm(ebitda) / sh, OPER_INC_PER_SH = ttm(opinc) / sh,
    RETURN_COM_EQY = 100 * ttm(ni) / equity_, RETURN_ON_ASSET = 100 * ttm(ni) / assets,
    RETURN_ON_CAP = 100 * ttm(opinc) * 0.72 / (equity_ + debt),
    EFF_TAX_RATE = ifelse(pretax > 0, 100 * tax / pretax, NA),
    DVD_PAYOUT_RATIO = ifelse(ttm(ni) > 0, 100 * ttm(-dividends) / ttm(ni), NA),
    SALES_GROWTH = yoy(rev), EBITDA_GROWTH = yoy(ebitda), ASSET_GROWTH = yoy(assets),
    REVENUE_SEQUENTIAL_GROWTH = seq_g(rev), TOTAL_EQUITY_1_YEAR_GROWTH = yoy(equity_),
    TOT_DEBT_TO_EBITDA = ifelse(ttm(ebitda) > 0, debt / ttm(ebitda), NA),
    NET_DEBT_TO_EBITDA = ifelse(ttm(ebitda) > 0, (debt - cash) / ttm(ebitda), NA),
    EBITDA_TO_INTEREST_EXPN = ttm(ebitda) / ttm(interest),
    COM_EQY_TO_TOT_ASSET = 100 * equity_ / assets, LT_DEBT_TO_TOT_EQY = 100 * lt_debt / equity_,
    TOT_DEBT_TO_TOT_EQY = 100 * debt / equity_, TOT_DEBT_TO_TOT_CAP = 100 * debt / (debt + equity_)
  )
  keep <- quarter_ends >= sim_start
  if (! is.na(e$listed)) keep <- keep & quarter_ends >= as.Date(e$listed)
  data.frame(date = quarter_ends[keep], lapply(values, function(v) signif(v[keep], 7L)),
             check.names = FALSE)
}


# fund ####

fund <- list(ticker = "SPY US Equity", price = 201, shares = 950, volume = 95e6)

simulate_fund_market <- function(market_us) {
  date <- days$US; n <- length(date)
  r <- market_us + stats::rnorm(n, 0, 0.0004); r[1L] <- 0
  close <- fund$price * exp(cumsum(r))
  px <- bar(close, r, 0.01)
  nav <- close * (1 + stats::rnorm(n, 0, 2e-4))
  shares <- fund$shares * exp(cumsum(stats::rnorm(n, 0.0004, 0.003)))
  flow <- c(0, diff(shares)) * nav
  stats::setNames(list(cbind(
    date = date, px, PX_VOLUME = volume(r, fund$volume),
    FUND_NET_ASSET_VAL = round(nav, 4L), CEF_PCT_PREM = round(100 * (close / nav - 1), 4L),
    EQY_SH_OUT = round(shares, 3L), FUND_FLOW = round(flow, 3L),
    FUND_TOTAL_ASSETS = round(nav * shares, 3L), CUR_MKT_CAP = round(px$PX_LAST * shares * 1e6, -3L),
    FUND_YIELD = round(2 + ar1(n, 0.995, 0.01), 4L)
  )), fund$ticker)
}


# futures ####

# price level, volatility, annualised carry (log slope of the curve), listed
# contract months, sector, front-month open interest and volume
futures <- tibble::tribble(
  ~ticker,       ~root, ~price, ~vol, ~carry, ~cycle,              ~sector,  ~oi,    ~volume, ~option,
  "C A Comdty",  "C",   360,    0.22,  0.08,  "3,5,7,9,12",        "grains", 6.5e5,  1.6e5,   0.35,
  "S A Comdty",  "S",   870,    0.18,  0.04,  "1,3,5,7,8,9,11",    "grains", 3.5e5,  1.1e5,   0.35,
  "W A Comdty",  "W",   470,    0.28,  0.10,  "3,5,7,9,12",        "grains", 2.2e5,  7.0e4,   0.30,
  "KWA Comdty",  "KW",  480,    0.28,  0.10,  "3,5,7,9,12",        "grains", 1.4e5,  3.0e4,   0.25,
  "LHA Comdty",  "LH",  60,     0.25,  0.03,  "2,4,5,6,7,8,10,12", "meats",  1.0e5,  3.0e4,   0.30,
  "SBA Comdty",  "SB",  15,     0.30, -0.02,  "3,5,7,10",          "softs",  4.0e5,  8.0e4,   0.30,
  "COA Comdty",  "CO",  37,     0.38,  0.10,  "1,2,3,4,5,6,7,8,9,10,11,12", "energy", 5.5e5, 2.5e5, 0.25,
  "CLA Comdty",  "CL",  37,     0.40,  0.12,  "1,2,3,4,5,6,7,8,9,10,11,12", "energy", 4.5e5, 5.0e5, 0.25,
  "HOA Comdty",  "HO",  110,    0.35,  0.06,  "1,2,3,4,5,6,7,8,9,10,11,12", "energy", 1.2e5, 8.0e4, 0.15,
  "QSA Comdty",  "QS",  330,    0.35,  0.06,  "1,2,3,4,5,6,7,8,9,10,11,12", "energy", 1.6e5, 1.0e5, 0.10,
  "GCA Comdty",  "GC",  1065,   0.15,  0.015, "2,4,6,8,10,12",     "metals", 3.2e5,  1.5e5,   0.20,
  "SIA Comdty",  "SI",  13.9,   0.22,  0.015, "3,5,7,9,12",        "metals", 1.2e5,  5.0e4,   0.20
) %>% dplyr::left_join(dplyr::select(tickers_futures, ticker, tick = `tick size`), by = "ticker")

sector_vol <- c(grains = 0.18, meats = 0.20, softs = 0.25, energy = 0.35, metals = 0.15)

# expiries of the listed contracts around the simulation window
expiries <- function(cycle) {
  months <- as.integer(strsplit(cycle, ",")[[1L]])
  d <- as.Date(unlist(lapply(2015L:2020L, function(y) sprintf("%d-%02d-15", y, months))))
  sort(d)
}

simulate_futures <- function() {
  date <- days$futures; n <- length(date)
  sectors <- lapply(sector_vol, function(v) garch_returns(n, v))
  out <- lapply(seq_len(nrow(futures)), function(i) {
    f <- futures[i, ]
    r <- factor_returns(sectors[[f$sector]], 0.8, f$vol, sector_vol[[f$sector]]); r[1L] <- 0
    spot <- log(f$price) + cumsum(r)
    carry <- f$carry + ar1(n, 0.995, 0.01 * f$vol)
    expiry <- expiries(f$cycle); roll <- expiry - 25L
    front <- vapply(date, function(d) which(roll > d)[1L], integer(1L))
    noise <- matrix(ar1(n * 5L, 0.95, 0.005 * f$vol), nrow = n)
    oi_w <- c(1, 0.55, 0.30, 0.17, 0.10); vol_w <- oi_w ^ 1.6
    oi_noise <- ar1(n, 0.98, 0.03); shock <- abs(r) / stats::sd(r)
    series <- lapply(1L:5L, function(k) {
      tau <- as.numeric(expiry[front + k - 1L] - date) / 365.25
      close <- exp(spot + carry * tau + noise[, k])
      px <- bar(close, r, f$tick)
      cbind(date = date, px,
            PX_VOLUME = round(f$volume * vol_w[k] * exp(ar1(n, 0.5, 0.3) + 0.3 * (shock - 1))),
            OPEN_INT = round(f$oi * oi_w[k] * exp(oi_noise + ar1(n, 0.9, 0.02))))
    })
    names(series) <- vapply(1L:5L, function(k) pullit::futures_ticker(f$ticker, k), character(1L))
    oi <- Reduce(`+`, lapply(series, `[[`, "OPEN_INT")) / 0.92
    vol <- Reduce(`+`, lapply(series, `[[`, "PX_VOLUME")) / 0.96
    aggregate <- data.frame(date = date, FUT_AGGTE_OPEN_INT = round(oi), FUT_AGGTE_VOL = round(vol))
    list(term_structure = series, aggregate = stats::setNames(list(aggregate), f$ticker),
         cftc = simulate_cftc(f, date, spot, oi))
  })
  list(
    market = do.call(c, c(lapply(out, `[[`, "term_structure"), lapply(out, `[[`, "aggregate"))),
    cftc = do.call(c, lapply(out, `[[`, "cftc"))
  )
}


# CFTC positions ####

# Weekly (Tuesday) positions. Within each format both sides add up to open
# interest; managed money follows 13-week price trends and producers absorb
# the other side, so commercial hedging pressure moves against momentum.
simulate_cftc <- function(f, date, spot, oi) {
  tue <- date[format(date, "%u") == "2"]; idx <- match(tue, date); n <- length(tue)
  trend <- c(rep(0, 13L), diff(spot[idx], lag = 13L)) / f$vol
  z <- 0.6 * trend + ar1(n, 0.9, 0.15)
  share <- function(base, sd = 0.06) base * exp(ar1(n, 0.9, sd))
  futures_only <- function(oi) {
    p <- list(
      mm_l = share(0.16) * exp(0.5 * z), mm_s = share(0.10) * exp(-0.5 * z), mm_d = share(0.08),
      or_l = share(0.08), or_s = share(0.05), or_d = share(0.05),
      sd_l = share(0.10), sd_s = share(0.04), sd_d = share(0.03),
      nr_l = share(0.08), nr_s = share(0.09)
    )
    long_rest <- with(p, mm_l + mm_d + or_l + or_d + sd_l + sd_d + nr_l)
    short_rest <- with(p, mm_s + mm_d + or_s + or_d + sd_s + sd_d + nr_s)
    p$pm_l <- pmax(1 - long_rest, 0.05); p$pm_s <- pmax(1 - short_rest, 0.05)
    scale <- 1 / pmax(long_rest + p$pm_l, short_rest + p$pm_s)
    lapply(p, function(x) round(x * scale * oi))
  }
  oi_fut <- round(oi[idx] * 0.97)
  positions <- list(
    `futures only` = c(futures_only(oi_fut), list(oi = oi_fut)),
    `futures & options` = c(futures_only(round(oi_fut * (1 + f$option))), list(oi = round(oi_fut * (1 + f$option))))
  )
  traders_base <- c(pm_l = 160, pm_s = 170, sd_l = 22, sd_s = 14, sd_d = 25, mm_l = 120, mm_s = 85,
                    mm_d = 110, or_l = 110, or_s = 70, or_d = 95) * f$oi / 3e5
  traders <- lapply(traders_base, function(b) pmax(round(b * exp(ar1(n, 0.95, 0.05))), 3))
  traders$mm_l <- pmax(round(traders$mm_l * exp(0.2 * z)), 3)
  traders$mm_s <- pmax(round(traders$mm_s * exp(-0.2 * z)), 3)

  value <- function(format, underlying, unit, participant, position) {
    p <- positions[[underlying]]
    t <- if (underlying == "futures & options") lapply(traders, function(x) round(x * 1.12)) else traders
    side <- function(l, s, d = NULL) switch(position, long = l, short = s, spreading = d,
                                            net = l - s, NULL)
    if (unit == "traders") {
      return(switch(format,
        disaggregated = switch(participant,
          `producer/merchant/processor/user` = side(t$pm_l, t$pm_s),
          `swap dealers` = side(t$sd_l, t$sd_s, t$sd_d),
          `managed money` = side(t$mm_l, t$mm_s, t$mm_d),
          `other reportables` = side(t$or_l, t$or_s, t$or_d)),
        legacy = switch(participant,
          commercial = side(t$pm_l + t$sd_l, t$pm_s + t$sd_s),
          `non-commercial` = side(t$mm_l + t$or_l, t$mm_s + t$or_s, t$mm_d + t$or_d),
          total = if (position == "total") round(0.8 * Reduce(`+`, t))
                  else side(round(0.85 * (t$pm_l + t$sd_l + t$mm_l + t$or_l)),
                            round(0.85 * (t$pm_s + t$sd_s + t$mm_s + t$or_s))))))
    }
    switch(format,
      disaggregated = switch(participant,
        `producer/merchant/processor/user` = side(p$pm_l, p$pm_s),
        `swap dealers` = side(p$sd_l, p$sd_s, p$sd_d),
        `managed money` = side(p$mm_l, p$mm_s, p$mm_d),
        `other reportables` = side(p$or_l, p$or_s, p$or_d)),
      legacy = switch(participant,
        commercial = side(p$pm_l + p$sd_l + p$sd_d, p$pm_s + p$sd_s + p$sd_d),
        `non-commercial` = side(p$mm_l + p$or_l, p$mm_s + p$or_s, p$mm_d + p$or_d),
        `non-reportable` = side(p$nr_l, p$nr_s),
        total = if (position == "open interest") p$oi else side(p$oi, p$oi)),
      supplemental = {
        cit_l <- round(0.8 * p$sd_l + 0.1 * p$or_l); cit_s <- round(0.3 * p$sd_s)
        switch(participant,
          `index traders` = side(cit_l, cit_s),
          `commercial - non-CIT` = side(p$pm_l + p$sd_l + p$sd_d - round(0.8 * p$sd_l),
                                        p$pm_s + p$sd_s + p$sd_d - cit_s),
          `non-commercial - non-CIT` = side(p$mm_l + p$or_l - round(0.1 * p$or_l), p$mm_s + p$or_s,
                                            p$mm_d + p$or_d))
      },
      NULL)
  }

  # Legacy format only: the commercial hedging pressure factor (factorem::CHP_factor)
  # is the one consumer, and it uses the legacy commercial / non-commercial split.
  rows <- dplyr::filter(tickers_cftc, `active contract ticker` == f$ticker, format == "legacy")
  series <- lapply(seq_len(nrow(rows)), function(j) {
    x <- rows[j, ]
    v <- value(x$format, x$underlying, x$unit, x$participant, x$position)
    if (is.null(v)) data.frame(date = as.Date(character()), PX_LAST = numeric())
    else data.frame(date = tue, PX_LAST = as.numeric(v))
  })
  stats::setNames(series, rows$ticker)
}


# reference data (Rblpapi::bdp) ####

reference <- function() {
  equity_info <- data.frame(
    LONG_COMP_NAME = equity$name, SECURITY_TYP = "Common Stock", ID_MIC_PRIM_EXCH = equity$mic,
    CNTRY_OF_INCORPORATION = equity$country, CRNCY = equity$crncy, ID_BB_GLOBAL = NA_character_,
    GICS_SECTOR = substr(equity$gics, 1L, 2L), GICS_INDUSTRY_GROUP = substr(equity$gics, 1L, 4L),
    GICS_INDUSTRY = substr(equity$gics, 1L, 6L), GICS_SUB_INDUSTRY = equity$gics,
    CIE_DES = NA_character_, row.names = equity$ticker, stringsAsFactors = FALSE
  )
  fund_info <- data.frame(
    LONG_COMP_NAME = "SPDR S&P 500 ETF Trust", LONG_PARENT_COMP_NAME = "State Street Corp",
    FUND_MASTER_NAME = "", ID_MIC_PRIM_EXCH = "ARCX", CRNCY = "USD", FD_INITIAL_VAL = "43.97",
    FUND_INIT_VAL_CRNCY = "USD", FUND_TYP = "ETF", SECURITY_IS_PRIVATE_EQUITY_FUND = "FALSE",
    FUND_OBJECTIVE_LONG = "Large-cap", FUND_STRATEGY = "Blend", GROWTH_FUND_INDICATOR = "FALSE",
    VALUE_FUND_INDICATOR = "FALSE", INVERSE_FUND_INDICATOR = "FALSE",
    FUND_ASSET_CLASS_FOCUS = "Equity", FUND_GEO_FOCUS = "U.S.", FUND_MKT_CAP_FOCUS = "Large-cap",
    FUND_LEVERAGE = "FALSE", FUND_TOTAL_ASSETS_CRNCY = "USD", NAV_CRNCY = "USD",
    ACTIVELY_MANAGED = "N", INVESTS_IN_SWAPS = "FALSE", DERIVATIVES_BASED = "FALSE",
    CURRENCY_HEDGED_INDICATOR = "FALSE", INVESTS_IN_PHYSICAL_COMMODITIES = "FALSE",
    ETF_UNDL_INDEX_TICKER = "SPX", FUND_BENCHMARK_PRIM = "SPX",
    row.names = fund$ticker, stringsAsFactors = FALSE
  )
  spec <- dplyr::filter(tickers_futures, ticker %in% futures$ticker)
  futures_info <- data.frame(
    SECURITY_NAME = paste(toupper(spec$name), "FUTURE"), FUTURES_CATEGORY = spec$subsector,
    CRNCY = spec$currency, ID_MIC_PRIM_EXCH = spec$MIC,
    FUT_NUM_GEN_AVAIL = as.character(spec$`term structure length`),
    FUT_CONT_SIZE = as.character(spec$`contract size`), FUT_TRADING_UNITS = spec$`trading unit`,
    FUT_VAL_PT = as.character(spec$`point value`), FUT_TICK_SIZE = as.character(spec$`tick size`),
    FUT_TICK_VAL = as.character(spec$`tick value`), ID_BB_GLOBAL = spec$FIGI,
    row.names = spec$ticker, stringsAsFactors = FALSE
  )
  list(equity = equity_info, fund = fund_info, futures = futures_info)
}


# the terminal ####

build_terminal <- function(seed = 20160101L) {
  set.seed(seed)
  market <- simulate_markets()
  equity_market <- simulate_equity_market(market)
  books <- stats::setNames(
    lapply(seq_len(nrow(equity)), function(i) simulate_fundamentals(equity[i, ], equity_market[[i]])),
    equity$ticker
  )
  fund_market <- simulate_fund_market(market$US)
  futures_data <- simulate_futures()
  list(
    historical = c(equity_market, fund_market, futures_data$market, futures_data$cftc),
    books = books, reference = reference(), book = NULL
  )
}

terminal <- new.env()

# Symbols listed twice within one book of BBGsymbols::fields cannot be stored
# by storethat (it joins on symbol), so the terminal returns nothing for them.
book_symbols <- function(book) {
  f <- dplyr::filter(fields, instrument == "equity", book == !! book)
  setdiff(f$symbol, f$symbol[duplicated(f$symbol)])
}

mock_bdh <- function(securities, fields, start.date, end.date = NULL, ..., con = NULL) {
  start.date <- as.Date(start.date); end.date <- as.Date(end.date)
  one <- function(security) {
    x <- if (! is.null(terminal$book)) {
      b <- terminal$books[[security]]
      ok <- intersect(fields, book_symbols(terminal$book))
      if (is.null(b)) NULL else b[, c("date", intersect(ok, names(b))), drop = FALSE]
    } else terminal$historical[[security]]
    if (is.null(x)) x <- data.frame(date = as.Date(character()))
    x <- x[x$date >= start.date & x$date <= end.date, , drop = FALSE]
    for (f in setdiff(fields, names(x))) x[[f]] <- rep(NA_real_, nrow(x))
    x <- x[, c("date", fields), drop = FALSE]
    if (! is.null(terminal$book)) x <- x[stats::complete.cases(x), , drop = FALSE]
    rownames(x) <- NULL; x
  }
  if (length(securities) == 1L) one(securities)
  else stats::setNames(lapply(securities, one), securities)
}

mock_bdp <- function(securities, fields, ..., con = NULL) {
  ref <- terminal$reference
  table <- if (all(securities %in% rownames(ref$futures))) ref$futures
           else if (all(securities %in% rownames(ref$fund))) ref$fund else ref$equity
  out <- table[securities, intersect(fields, names(table)), drop = FALSE]
  for (f in setdiff(fields, names(out))) out[[f]] <- NA_character_
  out[, fields, drop = FALSE]
}

with_terminal <- function(code) {
  list2env(build_terminal(), envir = terminal)
  testthat::with_mocked_bindings(
    code,
    blpConnect = function(...) structure(list(), class = "simulated_terminal"),
    blpDisconnect = function(con) invisible(NULL),
    bdh = mock_bdh, bdp = mock_bdp,
    .package = "Rblpapi"
  )
}
