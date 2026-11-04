nepse_read_dataset <- function(dataset_id, start = NULL, end = NULL) {
  man <- nepse_manifest_for(dataset_id, start = start, end = end)
  rows <- lapply(seq_len(nrow(man)), function(i) nepse_read_archive(man[i, , drop = FALSE]))
  nepse_bind(rows)
}

nepse_filter_symbols <- function(data, symbol) {
  if (is.null(symbol)) return(data)
  if (!is.character(symbol) || anyNA(symbol) || !length(symbol)) {
    nepse_stop("`symbol` must be a non-empty character vector.")
  }
  wanted <- toupper(trimws(symbol))
  available <- unique(data$symbol)
  missing <- setdiff(wanted, toupper(available))
  if (length(missing)) {
    hint <- utils::head(available[order(available)], 8)
    nepse_stop(
      "Symbol(s) not found in the requested archive range: %s. Use nepse_search() to find listed symbols. Examples: %s",
      paste(missing, collapse = ", "), paste(hint, collapse = ", ")
    )
  }
  data[toupper(data$symbol) %in% wanted, , drop = FALSE]
}

nepse_to_long <- function(data, id_columns, value_columns) {
  provenance <- intersect(nepse_provenance_columns(), names(data))
  blocks <- lapply(value_columns, function(field) {
    out <- data[, c(id_columns, provenance), drop = FALSE]
    out$field <- field
    out$value <- as.numeric(data[[field]])
    out
  })
  out <- nepse_bind(blocks)
  out[, c(id_columns, "field", "value", setdiff(names(out), c(id_columns, "field", "value"))), drop = FALSE]
}

#' Retrieve archived share prices
#'
#' Returns one row per symbol and trading day from the validated GitHub archive.
#' The first call downloads and caches the monthly partition files that cover
#' the requested dates; later calls reuse those content-addressed copies.
#'
#' @param symbol Optional character vector of symbols, matched case-insensitively.
#' @param start,end Optional ISO dates or `Date` values. When both are `NULL`
#'   the whole archive is returned.
#' @param long If `TRUE`, return one row per symbol, date, and field instead of
#'   one wide row per symbol and date.
#' @param source `"archive"` (the default) reads the GitHub data branch;
#'   `"live"` scrapes the current ShareSansar page and does not use the archive.
#' @param refresh If `TRUE`, re-read the archive manifest before answering.
#' @return A tibble of standardized daily prices.
#' @export
nepse_prices <- function(symbol = NULL, start = NULL, end = NULL,
                         long = FALSE, source = c("archive", "live"),
                         refresh = FALSE) {
  source <- match.arg(source)
  nepse_assert_flag(long, "long")
  nepse_assert_flag(refresh, "refresh")
  start <- nepse_parse_date_arg(start, "start")
  end <- nepse_parse_date_arg(end, "end")
  if (!is.null(start) && !is.null(end) && start > end) {
    nepse_stop("`start` must not be later than `end`.")
  }

  if (identical(source, "live")) {
    if (!is.null(start) || !is.null(end)) {
      date <- start %||% end
      if (!identical(start, end)) {
        nepse_stop(
          "The live ShareSansar source returns one trading day at a time; supply `start` and `end` equal, or use `source = \"archive\"`."
        )
      }
      data <- nepse_scrape_prices(date)
    } else {
      data <- nepse_scrape_prices()
    }
    data <- nepse_filter_symbols(data, symbol)
    data <- data[nepse_in_range(data$trading_date, start, end), , drop = FALSE]
    if (isTRUE(long)) {
      data <- nepse_to_long(
        data, c("trading_date", "symbol", "company_name"),
        setdiff(nepse_prices_columns(), c("trading_date", "symbol", "company_name"))
      )
    }
    return(tibble::as_tibble(data))
  }

  if (isTRUE(refresh)) invisible(nepse_index(force = TRUE))
  data <- nepse_read_dataset("prices", start = start, end = end)
  data <- nepse_filter_symbols(data, symbol)
  data <- data[nepse_in_range(data$trading_date, start, end), , drop = FALSE]
  data <- data[order(data$symbol, data$trading_date), , drop = FALSE]
  if (isTRUE(long)) {
    data <- nepse_to_long(
      data, c("trading_date", "symbol", "company_name"),
      setdiff(nepse_prices_columns(), c("trading_date", "symbol", "company_name"))
    )
  }
  tibble::as_tibble(data)
}

#' Retrieve archived sector sub-index levels
#'
#' @param index Optional character vector of index names, matched
#'   case-insensitively. Use [nepse_indices()] without `index` to see names.
#' @param start,end Optional ISO dates or `Date` values.
#' @param refresh If `TRUE`, re-read the archive manifest before answering.
#' @return A tibble of daily index levels. `ohlc_consistent` flags whether the
#'   published high is at least the published low; `source_note` explains any
#'   publisher anomaly while preserving the original values.
#' @export
nepse_indices <- function(index = NULL, start = NULL, end = NULL,
                          refresh = FALSE) {
  nepse_assert_flag(refresh, "refresh")
  start <- nepse_parse_date_arg(start, "start")
  end <- nepse_parse_date_arg(end, "end")
  if (isTRUE(refresh)) invisible(nepse_index(force = TRUE))
  data <- nepse_read_dataset("indices", start = start, end = end)
  if (!is.null(index)) {
    if (!is.character(index) || anyNA(index) || !length(index)) {
      nepse_stop("`index` must be a non-empty character vector.")
    }
    wanted <- tolower(trimws(index))
    available <- unique(data$index_name)
    missing <- setdiff(wanted, tolower(available))
    if (length(missing)) {
      nepse_stop(
        "Unknown index name(s): %s. Available: %s",
        paste(missing, collapse = ", "), paste(available, collapse = ", ")
      )
    }
    data <- data[tolower(data$index_name) %in% wanted, , drop = FALSE]
  }
  data <- data[nepse_in_range(data$trading_date, start, end), , drop = FALSE]
  data <- data[order(data$trading_date, data$index_name), , drop = FALSE]
  tibble::as_tibble(data)
}

#' Summarize archived market activity
#'
#' Aggregates the share-price archive into one row per trading day.
#'
#' @param start,end Optional ISO dates or `Date` values.
#' @param refresh If `TRUE`, re-read the archive manifest before answering.
#' @return A tibble with turnover, volume, transaction counts, and the number of
#'   advancing, declining, and unchanged symbols.
#' @export
nepse_market <- function(start = NULL, end = NULL, refresh = FALSE) {
  data <- nepse_prices(start = start, end = end, refresh = refresh)
  if (!nrow(data)) nepse_stop("No archived prices are available for this range.")
  dates <- sort(unique(data$trading_date))
  rows <- lapply(dates, function(day) {
    sub <- data[data$trading_date == day, , drop = FALSE]
    tibble::tibble(
      trading_date = day,
      symbols = nrow(sub),
      symbols_traded = sum(sub$volume > 0, na.rm = TRUE),
      total_volume = sum(sub$volume, na.rm = TRUE),
      total_turnover = sum(sub$turnover, na.rm = TRUE),
      total_transactions = sum(sub$transactions, na.rm = TRUE),
      advancers = sum(sub$diff > 0, na.rm = TRUE),
      decliners = sum(sub$diff < 0, na.rm = TRUE),
      unchanged = sum(sub$diff == 0, na.rm = TRUE)
    )
  })
  tibble::as_tibble(nepse_bind(rows))
}

#' List archived trading dates
#'
#' Reads the archive manifest only; no partition files are downloaded.
#'
#' @param refresh If `TRUE`, re-read the archive manifest first.
#' @return A `Date` vector of trading days with archived prices.
#' @export
nepse_dates <- function(refresh = FALSE) {
  nepse_assert_flag(refresh, "refresh")
  man <- nepse_index(force = refresh)$manifest
  man <- man[man$dataset_id == "prices" & man$current, , drop = FALSE]
  daily <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", man$partition)
  dates <- sort(unique(man$first_date[daily]))
  as.Date(dates)
}

#' Retrieve the latest archived snapshot
#'
#' @param long Passed to [nepse_prices()].
#' @param refresh If `TRUE`, re-read the archive manifest first.
#' @return A tibble of prices for the most recent archived trading day.
#' @export
nepse_latest <- function(long = FALSE, refresh = FALSE) {
  dates <- nepse_dates(refresh = refresh)
  if (!length(dates)) nepse_stop("The archive has no trading dates yet.")
  last <- max(dates, na.rm = TRUE)
  nepse_prices(start = last, end = last, long = long)
}

#' List archived symbols
#'
#' @param refresh If `TRUE`, re-read the archive manifest first.
#' @return A tibble of symbols, company names, and first/last archived dates.
#' @export
nepse_symbols <- function(refresh = FALSE) {
  nepse_assert_flag(refresh, "refresh")
  if (isTRUE(refresh)) invisible(nepse_index(force = TRUE))
  man <- nepse_manifest_for("symbols")
  data <- nepse_read_archive(man[1L, , drop = FALSE])
  data <- data[order(data$symbol), , drop = FALSE]
  tibble::as_tibble(data)
}
