# Fixtures for offline archive tests ---------------------------------------
# A local mirror is written to a temporary directory; no network is used.

fixture_price_table <- function(dates, symbols = c("NABIL", "NTC", "UPPER")) {
  grid <- expand.grid(
    trading_date = format(as.Date(dates), "%Y-%m-%d"), symbol = symbols,
    stringsAsFactors = FALSE
  )
  names <- c(
    NABIL = "Nabil Bank Limited",
    NTC = "Nepal Doorsanchar Company Limited",
    UPPER = "Upper Tamakoshi Hydropower Limited"
  )
  base <- c(NABIL = 500, NTC = 900, UPPER = 300)
  i <- seq_len(nrow(grid))
  close <- unname(base[grid$symbol]) + 10 * i
  out <- data.frame(
    trading_date = grid$trading_date,
    symbol = grid$symbol,
    company_name = unname(names[grid$symbol]),
    conf = round(20 + i / 10, 2),
    open = close - 4,
    high = close + 6,
    low = close - 8,
    close = close,
    ltp = close,
    close_ltp_diff = 0,
    close_ltp_pct = 0,
    vwap = close + 1,
    volume = 1000 * i,
    prev_close = close - 2,
    turnover = (close + 1) * 1000 * i,
    transactions = i,
    diff = 2,
    range = 14,
    diff_pct = 0.4,
    range_pct = 2.8,
    vwap_pct = 0.2,
    avg_120d = close - 20,
    avg_180d = close - 30,
    high_52w = close + 100,
    low_52w = close - 100,
    source_url = "https://www.sharesansar.com/today-share-price",
    retrieved_at = "2026-10-30T10:00:00Z",
    stringsAsFactors = FALSE
  )
  out[order(out$trading_date, out$symbol), , drop = FALSE]
}

fixture_index_table <- function(dates, indices = c("Banking SubIndex", "Microfinance Index")) {
  grid <- expand.grid(
    trading_date = format(as.Date(dates), "%Y-%m-%d"), index_name = indices,
    stringsAsFactors = FALSE
  )
  i <- seq_len(nrow(grid))
  data.frame(
    trading_date = grid$trading_date,
    index_name = grid$index_name,
    open = 1000 + 10 * i,
    high = 1010 + 10 * i,
    low = 990 + 10 * i,
    close = 1005 + 10 * i,
    turnover = 1e7 * i,
    point_change = 5 * i,
    pct_change = 0.5,
    high_52w = 1200 + 10 * i,
    low_52w = 800 + 10 * i,
    ohlc_consistent = TRUE,
    source_note = "",
    source_url = "https://www.sharesansar.com/",
    retrieved_at = "2026-10-30T10:00:00Z",
    stringsAsFactors = FALSE
  )
}

fixture_symbol_table <- function(symbols = c("NABIL", "NTC", "UPPER"),
                                 first = as.Date("2026-09-01"),
                                 last = as.Date("2026-10-05")) {
  data.frame(
    symbol = symbols,
    company_name = c("Nabil Bank Limited", "Nepal Doorsanchar Company Limited",
                     "Upper Tamakoshi Hydropower Limited"),
    first_seen = format(first, "%Y-%m-%d"),
    last_seen = format(last, "%Y-%m-%d"),
    source_url = "https://www.sharesansar.com/today-share-price",
    retrieved_at = "2026-10-30T10:00:00Z",
    stringsAsFactors = FALSE
  )
}

archive_write_file <- function(root, relative_path, data) {
  target <- file.path(root, relative_path)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data, target, row.names = FALSE, na = "")
  digest::digest(file = target, algo = "sha256")
}

archive_row <- function(dataset_id, partition, path, data, sha256,
                        first_date = NA, last_date = NA,
                        source_url = "https://www.sharesansar.com/today-share-price",
                        parser_version = NULL, current = "true") {
  if (is.null(parser_version)) {
    parser_version <- switch(
      dataset_id,
      prices = "prices-1.0.0",
      indices = "indices-1.0.0",
      symbols = "symbols-1.0.0"
    )
  }
  data.frame(
    dataset_id = dataset_id,
    partition = partition,
    path = path,
    first_date = ifelse(is.na(first_date), "", format(first_date, "%Y-%m-%d")),
    last_date = ifelse(is.na(last_date), "", format(last_date, "%Y-%m-%d")),
    rows = nrow(data),
    sha256 = sha256,
    updated_at = "2026-10-30T12:00:00Z",
    source_url = source_url,
    parser_version = parser_version,
    current = current,
    stringsAsFactors = FALSE
  )
}

build_archive <- function(root,
                          months = list(
                            "2026-09" = as.Date(c("2026-09-01", "2026-09-02")),
                            "2026-10" = as.Date(c("2026-10-01", "2026-10-02", "2026-10-05"))
                          ),
                          manifest = NULL) {
  if (is.null(manifest)) manifest <- data.frame()
  for (month in names(months)) {
    dates <- months[[month]]
    daily_data <- fixture_price_table(dates)
    monthly_daily <- data.frame()
    for (i in seq_along(dates)) {
      day <- dates[[i]]
      label <- format(day, "%Y-%m-%d")
      data <- daily_data[daily_data$trading_date == label, , drop = FALSE]
      relative <- file.path("data", "prices", "daily", format(day, "%Y"), paste0(label, ".csv"))
      sha <- archive_write_file(root, relative, data)
      manifest <- rbind(manifest, archive_row(
        "prices", label, relative, data, sha, day, day
      ))
      monthly_daily <- rbind(monthly_daily, data)
    }
    relative <- file.path("data", "prices", "monthly", paste0(month, ".csv"))
    sha <- archive_write_file(root, relative, monthly_daily)
    manifest <- rbind(manifest, archive_row(
      "prices", month, relative, monthly_daily, sha,
      min(dates), max(dates)
    ))

    index_daily <- fixture_index_table(dates)
    monthly_indices <- data.frame()
    for (i in seq_along(dates)) {
      day <- dates[[i]]
      label <- format(day, "%Y-%m-%d")
      data <- index_daily[index_daily$trading_date == label, , drop = FALSE]
      relative <- file.path("data", "indices", "daily", format(day, "%Y"), paste0(label, ".csv"))
      sha <- archive_write_file(root, relative, data)
      manifest <- rbind(manifest, archive_row(
        "indices", label, relative, data, sha, day, day,
        source_url = "https://www.sharesansar.com/"
      ))
      monthly_indices <- rbind(monthly_indices, data)
    }
    relative <- file.path("data", "indices", "monthly", paste0(month, ".csv"))
    sha <- archive_write_file(root, relative, monthly_indices)
    manifest <- rbind(manifest, archive_row(
      "indices", month, relative, monthly_indices, sha,
      min(dates), max(dates), source_url = "https://www.sharesansar.com/"
    ))
  }

  symbols <- fixture_symbol_table()
  relative <- "data/symbols.csv"
  sha <- archive_write_file(root, relative, symbols)
  manifest <- rbind(manifest, archive_row(
    "symbols", "current", relative, symbols, sha
  ))

  utils::write.csv(manifest, file.path(root, "manifest.csv"), row.names = FALSE, na = "")
  manifest
}

local_archive <- function(env = parent.frame()) {
  root <- withr::local_tempdir(pattern = "nepse-mirror", .local_envir = env)
  cache <- withr::local_tempdir(pattern = "nepse-cache", .local_envir = env)
  withr::local_options(
    list(
      lcNEPSE.mirror_dir = root,
      lcNEPSE.cache_dir = cache,
      lcNEPSE.index_ttl = 0
    ),
    .local_envir = env
  )
  manifest <- build_archive(root)
  list(root = root, cache = cache, manifest = manifest)
}

fixture_path <- function(name) {
  testthat::test_path("fixtures", name)
}

# A scraped price table with the trading date attached, matching what
# nepse_scrape_prices() returns before the archiver writes it.
fixture_scraped_prices <- function() {
  doc <- xml2::read_html(fixture_path("prices-page.html"))
  data <- nepse_parse_price_table(nepse_find_table(doc, nepse_prices_source_labels()))
  data$trading_date <- as.Date("2026-10-30")
  data[, c("trading_date", setdiff(names(data), "trading_date")), drop = FALSE]
}
