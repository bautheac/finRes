# name ####

#' Accesses the \code{name} slot of S4 objects of class \linkS4class{AssetPricingFactor} output of
#'   \href{https://github.com/bautheac/AssetPricingFactor/}{\pkg{AssetPricingFactor}}.
#'
#'
#' @description Access method for the \code{name} slot of S4 objects of class
#'   \linkS4class{AssetPricingFactor} from the
#'   \href{https://github.com/bautheac/factorem/}{\pkg{factorem}}.
#'
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#'
#' @return A scalar \code{\link[base]{character}} vector containing the name of the factor.
#'
#'
#' @docType methods
#'
#' @rdname get_name-methods
#'
#'
#' @export
setGeneric("get_name", function(object) standardGeneric("get_name"))



# info ####

#' Accesses the \code{info} slot of S4 objects of class \linkS4class{DataInfo}
#'   output of \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#'
#' @description Access method for the \code{info} slot of S4 objects of class
#'   \linkS4class{DataInfo} from the \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#'
#' @param object an S4 object of class \linkS4class{DataInfo} or childs.
#'
#'
#' @return A \link[tibble]{tibble} showing qualitative data contained in the
#'   \code{object} provided.
#'
#'
#' @docType methods
#'
#' @rdname get_info-methods
#'
#'
#' @importFrom tibble tibble
#'
#'
#' @export
setGeneric("get_info", function(object) standardGeneric("get_info"))



# tickers ####

#' Accesses \code{tickers} slot of various S4 objects output of
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#'
#' @description Access method for the \code{tickers} slot of various S4 objects from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#'
#' @param object an S4 object of class \linkS4class{DataHistorical} or childs.
#'
#'
#' @return A \link[data.table]{data.table} showing the Bloomberg tickers for which the
#'   \code{object} provided contains data.
#'
#'
#' @docType methods
#'
#' @rdname get_tickers-methods
#'
#'
#' @importClassesFrom data.table data.table
#'
#'
#' @export
setGeneric("get_tickers", function(object) standardGeneric("get_tickers"))



#' Accesses the \code{active_contract_tickers} slot \linkS4class{FuturesMarket} S4 objects
#'   output of \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#' @description Access method for the \code{active_contract_tickers} slot of
#'   \linkS4class{FuturesMarket} S4 objects from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#'
#' @param object an S4 object of class \linkS4class{FuturesMarket} or childs.
#'
#'
#' @return A \link[data.table]{data.table} showing the futures active contract
#'   Bloomberg tickers for which the \code{object} provided contains data.
#'
#'
#' @docType methods
#'
#' @rdname get_active_contract_tickers-methods
#'
#'
#' @importClassesFrom data.table data.table
#'
#'
#' @export
setGeneric("get_active_contract_tickers", function(object) standardGeneric("get_active_contract_tickers"))



#' Accesses the \code{term_structure_tickers} slot \linkS4class{FuturesTS} S4
#'   objects output of \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#'
#' @description Access method for the \code{term_structure_tickers} slot of
#'   \linkS4class{FuturesTS} S4 objects from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#'
#' @param object an S4 object of class \linkS4class{FuturesTS}.
#'
#'
#' @return A \link[data.table]{data.table} showing the futures term structure
#'   Bloomberg tickers for which the \code{object} provided contains data.
#'
#'
#' @docType methods
#'
#' @rdname get_term_structure_tickers-methods
#'
#'
#' @importClassesFrom data.table data.table
#'
#'
#' @export
setGeneric("get_term_structure_tickers", function(object) standardGeneric("get_term_structure_tickers"))



#' Accesses the \code{cftc_tickers} slot \linkS4class{FuturesCFTC} S4 objects
#'   output of \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#'
#' @description Access method for the \code{cftc_tickers} slot of \linkS4class{FuturesCFTC}
#'   S4 objects from the \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#'
#' @param object an S4 object of class \linkS4class{FuturesCFTC}.
#'
#'
#' @return A \link[data.table]{data.table} showing the futures position
#'   Bloomberg tickers for which the \code{object} provided contains data.
#'
#'
#' @docType methods
#'
#' @rdname get_cftc_tickers-methods
#'
#'
#' @importClassesFrom data.table data.table
#'
#'
#' @export
setGeneric("get_cftc_tickers", function(object) standardGeneric("get_cftc_tickers"))



# fields ####

#' Accesses the \code{fields} slot of various S4 objects output of
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#'
#' @description Access method for the \code{fields} slot of various S4 objects from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#'
#' @param object an S4 object of class \linkS4class{DataInfo} or
#'   \linkS4class{DataHistorical} or childs.
#'
#'
#' @return A \link[data.table]{data.table} showing for each Bloomberg ticker, the
#'   Bloomberg fields for which the \code{object} provided contains data.
#'
#'
#' @docType methods
#'
#' @rdname get_fields-methods
#'
#'
#' @importClassesFrom data.table data.table
#'
#'
#' @export
setGeneric("get_fields", function(object) standardGeneric("get_fields"))



# data ####

#' Accesses the \code{data} slot of various S4 objects output of
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}.
#'
#'
#' @description Access method for the \code{data} slot of various S4 objects from the
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#'
#' @param object an S4 object of class \linkS4class{DataHistorical} and childs
#'   or \linkS4class{AssetPricingFactor}.
#'
#'
#' @return A \link[data.table]{data.table} showing for each Bloomberg ticker and
#'   Bloomberg field the data contained in the \code{object} provided.
#'
#'
#' @docType methods
#'
#' @rdname get_data-methods
#'
#'
#' @importClassesFrom data.table data.table
#'
#'
#' @export
setGeneric("get_data", function(object) standardGeneric("get_data"))



# returns ####

#' Accesses the \code{returns} slot of S4 objects of class \linkS4class{AssetPricingFactor}
#'   output of \href{https://github.com/bautheac/AssetPricingFactor/}{\pkg{AssetPricingFactor}}.
#'
#'
#' @description Access method for the \code{returns} slot of S4 objects of class
#'   \linkS4class{AssetPricingFactor} from the
#'   \href{https://github.com/bautheac/factorem/}{\pkg{factorem}} package.
#'
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A \code{\link[data.table]{data.table}} of factor returns historical returns.
#'
#'
#' @docType methods
#'
#' @rdname get_returns-methods
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setGeneric("get_returns", function(object) standardGeneric("get_returns"))



# positions ####

#' Accesses the \code{positions} slot of S4 objects of class \linkS4class{AssetPricingFactor}
#'   output of \href{https://github.com/bautheac/AssetPricingFactor/}{\pkg{AssetPricingFactor}}.
#'
#'
#' @description Access method for the \code{positions} slot of S4 objects of class
#'   \linkS4class{AssetPricingFactor} from the
#'   \href{https://github.com/bautheac/factorem/}{\pkg{factorem}} package.
#'
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#'
#' @return A \code{\link[data.table]{data.table}} of factor positions.
#'
#'
#' @docType methods
#'
#' @rdname get_positions-methods
#'
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setGeneric("get_positions", function(object) standardGeneric("get_positions"))




## parameters ####

#' Accesses the \code{parameters} slot of S4 objects of class \linkS4class{AssetPricingFactor}
#'   output of \href{https://github.com/bautheac/AssetPricingFactor/}{\pkg{AssetPricingFactor}}.
#'
#'
#' @description Access method for the \code{parameters} slot of S4 objects of class
#'   \linkS4class{AssetPricingFactor} from the
#'   \href{https://github.com/bautheac/factorem/}{\pkg{factorem}} package.
#'
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @return A \code{\link[tibble]{tibble}} containing the original parameters supplied for factor
#'   construction.
#'
#'
#' @docType methods
#'
#' @rdname get_parameters-methods
#'
#'
#' @importFrom tibble tibble
#'
#'
#' @export
setGeneric("get_parameters", function(object) standardGeneric("get_parameters"))



# call ####

#' Accesses the \code{call} slot of various S4 objects in the
#'   \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite.
#'
#'
#' @description Access method for the \code{call} slot of various S4
#'   objects in the
#'   \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite.
#'
#'
#' @param object an S4 object of classes
#'   \itemize{
#'     \item{\linkS4class{DataInfo} or \linkS4class{DataHistorical} and
#'       respective childs.}
#'     \item{\linkS4class{AssetPricingFactor}.}
#'   }
#'
#'
#' @return The original call to the constructor function.
#'
#'
#' @docType methods
#'
#' @rdname get_call-methods
#'
#'
#' @export
setGeneric("get_call", function(object) standardGeneric("get_call"))




# periods ####

#' Shows the time periods for which various S4 objects output of
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} contain data.
#'
#'
#' @description Returns a data.table showing, for various S4 objects output of
#'   \href{https://github.com/bautheac/pullit/}{\pkg{pullit}}, fo each Bloomberg
#'   ticker and field, the time periods for which the object contains data.
#'
#'
#' @param object an S4 object of class \linkS4class{DataHistorical} or childs.
#'
#'
#' @return A \link[data.table]{data.table} showing for each Bloomberg ticker and
#'   Bloomberg field the time periods for which the \code{object} provided
#'   contains data.
#'
#'
#' @docType methods
#'
#' @rdname get_periods-methods
#'
#'
#' @importClassesFrom data.table data.table
#'
#'
#' @export
setGeneric("get_periods", function(object) standardGeneric("get_periods"))




# summary ####

## performance summary ####
#' Factor performance summary
#'
#'
#' @description Factor performance summary method. Returns factor returns
#'   by month and year.
#'
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#' @param leg a character vector. 'long', 'short' and 'factor' display
#'   the returns for the long, short and the factor itself respectively.
#'   Defaults to 'factor'.
#'
#'
#' @return A \code{\link[base]{data.frame}} with months as colums and years as rows.
#'   Column total shows the cumulated return for the corresponding year.
#'
#'
#' @docType methods
#'
#' @rdname summary-methods
#'
#'
#' @export
setGeneric("summary", function(object, leg = "factor") standardGeneric("summary"))






# plot ####

#' Plots futures term structure over time.
#'
#' @description Plots historical futures term structure data contained in an S4 object
#'   of class \linkS4class{FuturesTS} from the \href{https://github.com/bautheac/pullit/}{\pkg{pullit}} package.
#'
#' @param object an S4 object of class \linkS4class{FuturesTS}.
#' @param ticker a scalar character vector. Active contract Bloomberg ticker to plot the term structure for.
#' @param frame a scalar integer vector. Animation speed parameter; the lower the faster.
#'
#' @docType methods
#' @rdname plot_term_structure-methods
#'
#' @export
setGeneric("plot_term_structure", function(object, ticker, frame) standardGeneric("plot_term_structure"))



#' Plots market performance for various S4 objects ouput of packages that belong to the
#'   \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite.
#'
#' @description Plots historical market performance inficators for various S4 objects
#'   from packages belonging to the \href{https://bautheac.github.io/finRes/}{\pkg{finRes}} suite.
#'
#' @param object an S4 object of class \linkS4class{DataHistorical} and childs or \linkS4class{AssetPricingFactor}.
#' @param ... extra parameters for \code{object} related methods.
#'
#' @examples \dontrun{
#'   library(finRes)
#'
#'   # funds ####
#'   ## pull data from Bloomberg with pullit ####
#'   fund <- BBG_fund_market(tickers = "SPY US Equity",
#'     start = as.character(Sys.Date() - 365L), end = as.character(Sys.Date()))
#'   ## plot fund performance ####
#'   plot_performance(fund, ticker = "SPY US Equity")
#'
#'   # asset pricing factors ####
#'   ## pull data from Bloomberg via pullit ####
#'   term_structure <- BBG_futures_market(type = 'term structure',
#'     active_contract_tickers = c("C A Comdty", "S A Comdty", "SMA Comdty", "BOA Comdty",
#'       "W A Comdty", "KWA Comdty", "MWA Comdty", "O A Comdty",
#'       "CHEA Comdty", "V6A Comdty", "FCA Comdty", "LCA Comdty", "LHA Comdty",
#'       "SBA Comdty", "CCA Comdty", "CTA Comdty", "KCA Comdty", "JOA Comdty", "LBA Comdty",
#'       "GCA Comdty", "SIA Comdty", "PAA Comdty", "PLA Comdty", "LAA Comdty", "LLA Comdty",
#'       "LNA Comdty", "LPA Comdty", "LTA Comdty", "LXA Comdty",
#'       "CLA Comdty", "COA Comdty", "DLA Comdty", "NGA Comdty", "HOA Comdty", "XBA Comdty"),
#'     start = as.character(Sys.Date() - (2L * 365L)), end = as.character(Sys.Date()),
#'     TS_positions = 1L, roll_type = "A", roll_days = 0L, roll_months = 0L, roll_adjustment = "N")
#'   ## construct an asset pricing factor with factorem ####
#'   factor <- momentum_factor(term_structure)
#'   ## plot factor performance ####
#'   plot_performance(factor)
#' }
#'
#'
#' @docType methods
#' @rdname plot_performance-methods
#'
#' @export
setGeneric("plot_performance", function(object, ...) standardGeneric("plot_performance"))



#' Plots factor positions summary by legs
#'
#' @description Plots a summary of a factor's positions by legs.
#'
#' @param object an S4 object of class \linkS4class{AssetPricingFactor}.
#'
#'
#' @examples \dontrun{
#'   library(finRes)
#'   term_structure <- BBG_futures_market(type = 'term structure',
#'     active_contract_tickers = c("C A Comdty", "S A Comdty", "SMA Comdty", "BOA Comdty",
#'       "W A Comdty", "KWA Comdty", "MWA Comdty", "O A Comdty",
#'       "CHEA Comdty", "V6A Comdty", "FCA Comdty", "LCA Comdty", "LHA Comdty",
#'       "SBA Comdty", "CCA Comdty", "CTA Comdty", "KCA Comdty", "JOA Comdty", "LBA Comdty",
#'       "GCA Comdty", "SIA Comdty", "PAA Comdty", "PLA Comdty", "LAA Comdty", "LLA Comdty",
#'       "LNA Comdty", "LPA Comdty", "LTA Comdty", "LXA Comdty",
#'       "CLA Comdty", "COA Comdty", "DLA Comdty", "NGA Comdty", "HOA Comdty", "XBA Comdty"),
#'     start = as.character(Sys.Date() - (2L * 365L)), end = as.character(Sys.Date()),
#'     TS_positions = 1L, roll_type = "A", roll_days = 0L, roll_months = 0L, roll_adjustment = "N")
#'   factor <- momentum_factor(term_structure)
#'   plot_positions(factor)
#' }
#'
#'
#' @docType methods
#' @rdname plot_positions-methods
#'
#'
#' @export
setGeneric("plot_positions", function(object) standardGeneric("plot_positions"))


