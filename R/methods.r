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




# periods ####

## DataHistorical ####

#' @rdname get_periods-methods
#'
#' @aliases get_periods,DataHistorical
#'
#'
#' @importClassesFrom pullit DataHistorical
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_periods", "DataHistorical", function(object) {

  dplyr::group_by(object@data, ticker, field) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ticker, field) %>%
    data.table::as.data.table()

})

## FuturesMarket ####

#' @rdname get_periods-methods
#'
#' @aliases get_periods,DataHistorical,FuturesMarket
#'
#'
#' @importClassesFrom pullit FuturesMarket
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_periods", "FuturesMarket", function(object) {

  data <- switch (class(object),
                  "FuturesTS" = dplyr::left_join(object@data,
                                                 dplyr::select(object@term_structure_tickers, `active contract ticker`, ticker),
                                                 by = "ticker") %>%
                    dplyr::group_by(`active contract ticker`, ticker, field) %>%
                    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>% dplyr::ungroup() %>%
                    dplyr::arrange(`active contract ticker`, ticker, field),
                  "FuturesAggregate" = dplyr::group_by(object@data, ticker, field) %>%
                    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>% dplyr::ungroup() %>%
                    dplyr::arrange(ticker, field)
  )

  data.table::as.data.table(data)

})


## FuturesCFTC ####

#' @rdname get_periods-methods
#'
#' @aliases get_periods,DataHistorical,FuturesCFTC
#'
#'
#' @importClassesFrom pullit FuturesMarket
#'
#' @importFrom data.table data.table
#'
#'
#' @export
setMethod("get_periods", "FuturesCFTC", function(object) {

    dplyr::left_join(object@data, object@cftc_tickers, by = "ticker") %>%
    dplyr::group_by(`active contract ticker`, ticker) %>%
    dplyr::summarise_at(dplyr::vars(date), dplyr::funs(start = min, end = max)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(`active contract ticker`, ticker) %>%
    data.table::as.data.table()

})



# summary ####

#' @rdname summary-methods
#'
#' @aliases summary,AssetPricingFactor
#'
#'
#' @importClassesFrom pullit FuturesMarket
#'
#'
#' @export
setMethod("summary", "AssetPricingFactor", function(object, leg = "factor") {

  PerformanceAnalytics::table.CalendarReturns(xts::xts(dplyr::select(object@returns, returns = !! leg), order.by = object@returns$date),
                                              digits = 1L, as.perc = TRUE, geometric = object@params$geometric) %>%
    dplyr::rename(total = returns) %>% magrittr::set_names(tolower(names(.)))

})






# plot ####

## FuturesTS ####

#' @rdname plot_term_structure-methods
#' @aliases plot_term_structure,DataHistorical,FuturesTS
#'
#' @examples \dontrun{
#'   library(finRes)
#'   term_structure <- BBG_futures_market(type = 'term structure',
#'     active_contract_tickers = "C A Comdty", start = as.character(Sys.Date() - 365L),
#'     end = as.character(Sys.Date()), TS_positions = 1L:10L,
#'     roll_type = "A", roll_days = 0L, roll_months = 0L, roll_adjustment = "N")
#'   plot_term_structure(term_structure, ticker = "C A Comdty")
#' }
#'
#' @importClassesFrom pullit FuturesTS
#'
#' @export
setMethod("plot_term_structure", "FuturesTS", function(object, ticker, frame) {

  if (! all(rlang::is_scalar_character(ticker), ticker %in% object@active_contract_tickers$`active contract ticker`))
    stop("The parameter 'ticker' must be supplied as a scalar character vector; one of '",
         paste(object@active_contract_tickers$`active contract ticker`, collapse = "', '"), "'.")

  data <- dplyr::semi_join(object@data,
                           dplyr::filter(object@term_structure_tickers, `active contract ticker` == !! ticker) %>%
                             dplyr::select(`active contract ticker`, ticker),
                           by = "ticker") %>%
    dplyr::left_join(dplyr::select(object@term_structure_tickers, ticker, position = `TS position`), by = "ticker") %>%
    dplyr::select(position, field, date, value) %>% dplyr::mutate(value = as.numeric(value)) %>%
    tidyr::spread(field, value) %>%
    dplyr::select(date, position, close = PX_LAST, `open interest` = OPEN_INT, volume = PX_VOLUME) %>%
    dplyr::arrange(date, position)

  suppressWarnings(plotly::plot_ly(data, x = ~position, y = ~close, color = ~`open interest`, size = ~volume, frame = ~date,
                                   text = ~paste0("close price:\t", close, "\nopen interest:\t", `open interest`, "\nvolume:\t", volume),
                                   hoverinfo = "text", type = "scatter", mode = "lines+markers", line = list(color = "black", width = 1L)) %>%
                     plotly::layout(title = ticker, xaxis = list(title = ""), yaxis = list(title = "close price"),
                                    legend = list(orientation = "h", xanchor = "center",x = 0.5)) %>%
                     plotly::animation_opts(frame = frame, transition = 0L, redraw = FALSE) %>%
                     plotly::animation_button(x = 1L, xanchor = "right", y = 0L, yanchor = "bottom") %>%
                     plotly::animation_slider(currentvalue = list(prefix = "", font = list(color = "black")))
  )
})



## FundMarket ####

#' @param ticker a scalar vector. Specifies the fund Bloomberg ticker to plot performance for.
#'
#' @rdname plot_performance-methods
#' @aliases plot_performance,DataHistorical,FundMarket
#'
#' @importClassesFrom pullit FundMarket
#' @import BBGsymbols
#'
#' @export
setMethod("plot_performance", "FundMarket", function(object, ticker) {

  if (! all(rlang::is_scalar_character(ticker), ticker %in% object@tickers$ticker))
    stop("The parameter 'ticker' must be supplied as a scalar character vector; one of '", paste(object@tickers$ticker, collapse = "', '"), "'.")

  data(list = c("fields"), package = "BBGsymbols", envir = environment())

  data <- dplyr::filter(object@data, ticker == !! ticker, field %in% c("CUR_MKT_CAP", "EQY_SH_OUT", "FUND_FLOW", "PX_LAST", "PX_VOLUME")) %>%
    dplyr::left_join(dplyr::filter(fields, instrument == "fund", book == "market") %>% dplyr::select(symbol, name), by = c("field" = "symbol")) %>%
    dplyr::select(ticker, field = name, date, value) %>%
    dplyr::mutate(field = forcats::as_factor(field))

  p <- ggplot2::ggplot(data = data, ggplot2::aes(x = date, y = value, colour = field)) +
    ggplot2::geom_line(alpha = 0.6) + ggplot2::xlab(NULL) + ggplot2::ylab(NULL) +
    ggplot2::facet_wrap(~field, scales = "free") +
    ggthemes::theme_tufte(base_size = 12L) +
    ggplot2::ggtitle(ticker) +
    ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(hjust = 0.5))

  plotly::ggplotly(p)

})




## AssetPricingFactor ####

#' @rdname plot_performance-methods
#' @aliases plot_performance,AssetPricingFactor
#'
#' @importClassesFrom factorem AssetPricingFactor
#'
#' @export
setMethod("plot_performance", "AssetPricingFactor", function(object) {

  data <- apply(dplyr::select(object@returns, tidyselect::matches("long|short|object")) + 1L,
                function(x) cumprod(x), MARGIN = 2L)
  data <- xts::xts(data, order.by = as.Date(object@returns$date))

  if (all(c("long", "short") %in% names(data))) dygraphs::dygraph(data, main = paste(object@name, "factor")) %>%
    dygraphs::dyLimit(1L, label = NULL, strokePattern = "solid", color = "blue")
  else dygraphs::dygraph(data[, "factor"], main = paste(object@name, "factor"))%>%
    dygraphs::dyLimit(1L, label = NULL, strokePattern = "solid", color = "blue")

})




#' @rdname plot_positions-methods
#' @aliases plot_positions,AssetPricingFactor
#'
#' @export
setMethod("plot_positions", "AssetPricingFactor", function(object) {
  data <- dplyr::group_by(object@positions, position) %>%
    tidyr::nest() %>%
    dplyr::mutate(proportion = purrr::map(data, function(x) dplyr::group_by(x, name) %>%
                                            dplyr::tally() %>%
                                            dplyr::mutate(n = n / nrow(x))
    )) %>%
    tidyr::unnest(proportion) %>%
    rbind(dplyr::group_by(data, name) %>%
            dplyr::tally() %>%
            dplyr::mutate(position = "factor", n = n / nrow(object@positions))) %>%
    dplyr::rename(proportion = n)

  ggplot2::ggplot(data = data, mapping = ggplot2::aes(name, proportion, fill = name)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_x_discrete(breaks = NULL) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::facet_wrap(~position, ncol = 1L) +
    ggthemes::theme_tufte() +
    ggplot2::theme(legend.title = ggplot2::element_blank())

})



