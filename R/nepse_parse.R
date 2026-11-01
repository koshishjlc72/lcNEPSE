nepse_prices_columns <- function() {
  c(
    "trading_date", "symbol", "company_name", "conf", "open", "high", "low",
    "close", "ltp", "close_ltp_diff", "close_ltp_pct", "vwap", "volume",
    "prev_close", "turnover", "transactions", "diff", "range", "diff_pct",
    "range_pct", "vwap_pct", "avg_120d", "avg_180d", "high_52w", "low_52w"
  )
}

nepse_indices_columns <- function() {
  c(
    "trading_date", "index_name", "open", "high", "low", "close", "turnover",
    "point_change", "pct_change", "high_52w", "low_52w"
  )
}

nepse_symbols_columns <- function() {
  c("symbol", "company_name", "first_seen", "last_seen")
}

nepse_provenance_columns <- function() {
  c(
    "source_url", "retrieved_at", "source_sha256", "archive_path",
    "archive_updated_at", "parser_version"
  )
}

nepse_add_archive_provenance <- function(data, row, parser_version) {
  data$source_url <- row$source_url[[1L]]
  data$retrieved_at <- row$updated_at[[1L]]
  data$source_sha256 <- row$sha256[[1L]]
  data$archive_path <- row$path[[1L]]
  data$archive_updated_at <- row$updated_at[[1L]]
  data$parser_version <- parser_version
  ordered <- c(names(data)[!names(data) %in% nepse_provenance_columns()],
               nepse_provenance_columns())
  tibble::as_tibble(data[, ordered, drop = FALSE])
}

nepse_archive_path <- function(row) {
  nepse_partition_path(row)
}

nepse_parse_prices_archive <- function(path, row) {
  data <- nepse_read_csv(path, required = nepse_prices_columns())
  if (!nrow(data)) nepse_stop("Archived price partition `%s` has no rows.", path)
  out <- tibble::tibble(
    trading_date = nepse_parse_date(data$trading_date, "trading_date"),
    symbol = nepse_clean_text(data$symbol),
    company_name = nepse_clean_text(data$company_name),
    conf = nepse_parse_number(data$conf, "conf"),
    open = nepse_parse_number(data$open, "open"),
    high = nepse_parse_number(data$high, "high"),
    low = nepse_parse_number(data$low, "low"),
    close = nepse_parse_number(data$close, "close"),
    ltp = nepse_parse_number(data$ltp, "ltp"),
    close_ltp_diff = nepse_parse_number(data$close_ltp_diff, "close_ltp_diff"),
    close_ltp_pct = nepse_parse_percent(data$close_ltp_pct, "close_ltp_pct"),
    vwap = nepse_parse_number(data$vwap, "vwap"),
    volume = nepse_parse_number(data$volume, "volume"),
    prev_close = nepse_parse_number(data$prev_close, "prev_close"),
    turnover = nepse_parse_number(data$turnover, "turnover"),
    transactions = nepse_parse_number(data$transactions, "transactions"),
    diff = nepse_parse_number(data$diff, "diff"),
    range = nepse_parse_number(data$range, "range"),
    diff_pct = nepse_parse_percent(data$diff_pct, "diff_pct"),
    range_pct = nepse_parse_percent(data$range_pct, "range_pct"),
    vwap_pct = nepse_parse_percent(data$vwap_pct, "vwap_pct"),
    avg_120d = nepse_parse_number(data$avg_120d, "avg_120d"),
    avg_180d = nepse_parse_number(data$avg_180d, "avg_180d"),
    high_52w = nepse_parse_number(data$high_52w, "high_52w"),
    low_52w = nepse_parse_number(data$low_52w, "low_52w")
  )
  out$source_url <- nepse_clean_text(data$source_url)
  out$retrieved_at <- nepse_clean_text(data$retrieved_at)
  nepse_validate_prices(out, error = TRUE)
  nepse_add_archive_provenance(
    out, row, nepse_parser_version_for("prices")
  )
}

nepse_parse_indices_archive <- function(path, row) {
  data <- nepse_read_csv(path, required = nepse_indices_columns())
  if (!nrow(data)) nepse_stop("Archived index partition `%s` has no rows.", path)
  out <- tibble::tibble(
    trading_date = nepse_parse_date(data$trading_date, "trading_date"),
    index_name = nepse_clean_text(data$index_name),
    open = nepse_parse_number(data$open, "open"),
    high = nepse_parse_number(data$high, "high"),
    low = nepse_parse_number(data$low, "low"),
    close = nepse_parse_number(data$close, "close"),
    turnover = nepse_parse_number(data$turnover, "turnover"),
    point_change = nepse_parse_number(data$point_change, "point_change"),
    pct_change = nepse_parse_percent(data$pct_change, "pct_change"),
    high_52w = nepse_parse_number(data$high_52w, "high_52w"),
    low_52w = nepse_parse_number(data$low_52w, "low_52w")
  )
  out$source_url <- nepse_clean_text(data$source_url)
  out$retrieved_at <- nepse_clean_text(data$retrieved_at)
  nepse_validate_indices(out, error = TRUE)
  nepse_add_archive_provenance(
    out, row, nepse_parser_version_for("indices")
  )
}

nepse_parse_symbols_archive <- function(path, row) {
  data <- nepse_read_csv(path, required = nepse_symbols_columns())
  out <- tibble::tibble(
    symbol = nepse_clean_text(data$symbol),
    company_name = nepse_clean_text(data$company_name),
    first_seen = nepse_parse_date(data$first_seen, "first_seen", allow_na = TRUE),
    last_seen = nepse_parse_date(data$last_seen, "last_seen", allow_na = TRUE)
  )
  if (any(!nzchar(out$symbol))) nepse_stop("Symbol master contains an empty symbol.")
  if (anyDuplicated(out$symbol)) {
    nepse_stop("Symbol master contains duplicate symbols.")
  }
  nepse_add_archive_provenance(
    out, row, nepse_parser_version_for("symbols")
  )
}

nepse_read_archive <- function(row) {
  path <- nepse_partition_path(row)
  switch(
    row$dataset_id[[1L]],
    prices = nepse_parse_prices_archive(path, row),
    indices = nepse_parse_indices_archive(path, row),
    symbols = nepse_parse_symbols_archive(path, row),
    nepse_stop("No archive reader is registered for `%s`.", row$dataset_id[[1L]])
  )
}
