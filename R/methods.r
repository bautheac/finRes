# show methods ####

#' Show method for various S4 objects in the \href{https://bautheac.github.io/finRes/}{\pkg{finRes}}
#'   suite.
#'
#'
#' @param object an S4 object fron the \href{https://bautheac.github.io/finRes/}{\pkg{finRes}}
#'   suite.
#'
#'
#' @rdname show-methods
#'
#' @aliases show,DataInfo
#'
#'
#' @importClassesFrom pullit DataInfo
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", "DataInfo", function(object) {
  cat("S4 object of class",
      methods::is(object)[[1L]],
      "\n\nSlots inlude\n",
      "  info: access with get_info()\n",
      "  fields: access with get_fields()\n",
      "  call: access with get_call()")
})


#' @rdname show-methods
#'
#' @aliases show,FuturesHistorical
#'
#'
#' @importClassesFrom pullit FuturesHistorical
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", "FuturesHistorical", function(object) {
  cat("S4 object of class",
      methods::is(object)[[1L]],
      "\n\nSlots inlude\n",
      "  active_contract_tickers: access with get_active_contract_tickers()\n",
      "  fields: access with get_fields()\n",
      "  data: access with get_data()\n",
      "  call: access with get_call()\n",
      "\nSee also: get_periods()")
})


#' @rdname show-methods
#'
#' @aliases show,EquityHistorical
#'
#'
#' @importClassesFrom pullit EquityHistorical
#'
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", signature = "EquityHistorical", function(object) {
  cat("S4 object of class",
      methods::is(object)[[1L]],
      "\n\nSlots inlude\n",
      "  tickers: access with get_tickers()\n",
      "  fields: access with get_fields()\n",
      "  data: access with get_data()\n",
      "  call: access with get_call()\n",
      "\nSee also: get_periods()")
})

#' @rdname show-methods
#'
#' @aliases show,FundHistorical
#'
#'
#' @importClassesFrom pullit FundHistorical
#'
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", signature = "FundHistorical", function(object) {
  cat("S4 object of class",
      methods::is(object)[[1L]],
      "\n\nSlots inlude\n",
      "  tickers: access with get_tickers()\n",
      "  fields: access with get_fields()\n",
      "  data: access with get_data()\n",
      "  call: access with get_call()\n",
      "\nSee also: get_periods()")
})


#' @rdname show-methods
#'
#' @aliases show,FuturesTS
#'
#'
#' @importClassesFrom pullit FuturesTS
#'
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", "FuturesTS", function(object) {
  cat("S4 object of class",
      methods::is(object)[[1L]],
      "\n\nSlots inlude\n",
      "  active_contract_tickers: access with get_active_contract_tickers()\n",
      "  term_structure_tickers: access with get_term_structure_tickers()\n",
      "  fields: access with get_fields()\n",
      "  data: access with get_data()\n",
      "  call: access with get_call()\n",
      "\nSee also: get_periods()")
})


#' @rdname show-methods
#'
#' @aliases show,FuturesCFTC
#'
#'
#' @importClassesFrom pullit FuturesCFTC
#'
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", "FuturesCFTC", function(object) {
  cat("S4 object of class",
      methods::is(object)[[1L]],
      "\n\nSlots inlude\n",
      "  active_contract_tickers: access with get_active_contract_tickers()\n",
      "  cftc_tickers: access with get_cftc_tickers()\n",
      "  data: access with get_data()\n",
      "  call: access with get_call()\n",
      "\nSee also: get_periods()")
})


#' @rdname show-methods
#'
#' @aliases show,AssetPricingFactor
#'
#'
#' @importClassesFrom factorem AssetPricingFactor
#'
#' @importFrom methods show
#'
#'
#' @export
setMethod("show", signature(object = "AssetPricingFactor"), function(object) {
  cat(methods::is(object)[[1]], "\n",
      "  name: ", paste(object@name, "factor", " "), "\n",
      "  parameters\n",
      paste0(rep("    ", ncol(object@params)),
             gsub(pattern = "_", replacement = " ", names(object@params)),
             sapply(names(object@params), function(x) if (nchar(x) <= 10L) ":\t\t " else ":\t "),
             object@params[1L, ],
             rep("\n", ncol(object@params))),
      sep = ""
  )
})





# direct accessors ####

## name ####

#' @rdname get_name-methods
#'
#' @aliases get_name,AssetPricingFactor
#'
#'
#' @importClassesFrom factorem AssetPricingFactor
#'
#'
#' @export
setMethod("get_name", "AssetPricingFactor", function(object) object@name)




## info ####

#' @rdname get_info-methods
#'
#' @aliases get_info,DataInfo
#'
#'
#' @importClassesFrom pullit DataInfo
#'
#' @importFrom tibble tibble
#'
#'
#' @export
setMethod("get_info", signature = c("DataInfo"), function(object) object@info)



## tickers ####

#' @rdname get_active_contract_tickers-methods
#'
#' @aliases get_active_contract_tickers,FuturesHistorical
#'
#'
#' @importClassesFrom pullit FuturesHistorical
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_active_contract_tickers", signature = c("FuturesHistorical"), function(object) object@active_contract_tickers)

#' @rdname get_term_structure_tickers-methods
#'
#' @aliases get_term_structure_tickers,FuturesTS
#'
#'
#' @importClassesFrom pullit FuturesTS
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_term_structure_tickers", signature = c("FuturesTS"), function(object) object@term_structure_tickers)

#' @rdname get_cftc_tickers-methods
#'
#' @aliases get_cftc_tickers,FuturesCFTC
#'
#'
#' @importClassesFrom pullit FuturesCFTC
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_cftc_tickers", signature = c("FuturesCFTC"), function(object) object@cftc_tickers)

#' @rdname get_tickers-methods
#'
#' @aliases get_tickers,EquityHistorical
#'
#'
#' @importClassesFrom pullit EquityHistorical
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_tickers", signature = "EquityHistorical", function(object) object@tickers)

#' @rdname get_tickers-methods
#'
#' @aliases get_tickers,FundHistorical
#'
#'
#' @importClassesFrom pullit FundHistorical
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_tickers", signature = "FundHistorical", function(object) object@tickers)



## fields ####

#' @rdname get_fields-methods
#'
#' @aliases get_fields,DataInfo
#'
#'
#' @importClassesFrom pullit DataInfo
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_fields", "DataInfo", function(object) object@fields)

#' @rdname get_fields-methods
#'
#' @aliases get_fields,DataHistorical
#'
#'
#' @importClassesFrom pullit DataHistorical
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_fields", "DataHistorical", function(object) object@fields)



## data ####

#' @rdname get_data-methods
#'
#' @aliases get_data,AssetPricingFactor
#'
#'
#' @importClassesFrom factorem AssetPricingFactor
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_data", "AssetPricingFactor", function(object) object@data)


## data ####

#' @rdname get_data-methods
#'
#' @aliases get_data,DataHistorical
#'
#'
#' @importClassesFrom pullit DataHistorical
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_data", "DataHistorical", function(object) object@data)


## returns ####

#' @rdname get_returns-methods
#'
#' @aliases get_returns,AssetPricingFactor
#'
#'
#' @importClassesFrom factorem AssetPricingFactor
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_returns", "AssetPricingFactor", function(object) object@returns)



## positions ####

#' @rdname get_positions-methods
#'
#' @aliases get_positions,AssetPricingFactor
#'
#'
#' @importClassesFrom factorem AssetPricingFactor
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_positions", "AssetPricingFactor", function(object) object@positions)



## parameters ####

#' @rdname get_parameters-methods
#'
#' @aliases get_parameters,AssetPricingFactor
#'
#'
#' @importClassesFrom factorem AssetPricingFactor
#'
#' @importFrom tibble tibble
#'
#'
#' @export
setMethod("get_parameters", "AssetPricingFactor", function(object) object@parameters)




## call ####

#' @rdname get_call-methods
#'
#' @aliases get_call,DataInfo
#'
#'
#' @importClassesFrom pullit DataInfo
#'
#'
#' @export
setMethod("get_call", "DataInfo", function(object) object@call)

#' @rdname get_call-methods
#'
#' @aliases get_call,DataHistorical
#'
#'
#' @importClassesFrom pullit DataHistorical
#'
#'
#' @export
setMethod("get_call", "DataHistorical", function(object) object@call)


#' @rdname get_call-methods
#'
#' @aliases get_call,AssetPricingFactor
#'
#'
#' @importClassesFrom factorem AssetPricingFactor
#'
#'
#' @export
setMethod("get_call", "AssetPricingFactor", function(object) object@call)


