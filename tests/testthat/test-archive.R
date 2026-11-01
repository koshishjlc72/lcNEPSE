test_that("an archived range can be queried", {
  arc <- local_archive()
  expect_true(file.exists(file.path(arc$root, "manifest.csv")))

  data <- nepse_prices(start = "2026-10-01", end = "2026-10-02")
  expect_equal(nrow(data), 6L)
  expect_equal(sort(unique(data$trading_date)),
               as.Date(c("2026-10-01", "2026-10-02")))
  expect_equal(sort(unique(data$symbol)), c("NABIL", "NTC", "UPPER"))
  expect_equal(names(data)[1:6],
               c("trading_date", "symbol", "company_name", "conf", "open", "high"))
  expect_true(all(c("source_sha256", "archive_path", "parser_version") %in% names(data)))
  expect_equal(unique(data$parser_version), "prices-1.0.0")
})

test_that("the whole archive is returned when no range is given", {
  local_archive()
  data <- nepse_prices()
  expect_equal(nrow(data), 15L)
  expect_equal(length(unique(data$trading_date)), 5L)
})

test_that("symbols are matched case-insensitively and unknown symbols fail", {
  local_archive()
  data <- nepse_prices("nabil")
  expect_equal(nrow(data), 5L)
  expect_equal(unique(data$symbol), "NABIL")
  expect_error(nepse_prices("NOTREAL"), "Symbol\\(s\\) not found")
})

test_that("long format has one row per symbol, date, and field", {
  local_archive()
  long <- nepse_prices("NABIL", long = TRUE)
  expect_equal(nrow(long), 5L * 22L)
  expect_true(all(c("field", "value") %in% names(long)))
  expect_equal(sort(unique(long$field)), sort(setdiff(
    c("conf", "open", "high", "low", "close", "ltp", "close_ltp_diff",
      "close_ltp_pct", "vwap", "volume", "prev_close", "turnover",
      "transactions", "diff", "range", "diff_pct", "range_pct", "vwap_pct",
      "avg_120d", "avg_180d", "high_52w", "low_52w"),
    character()
  )))
})

test_that("index levels are available", {
  local_archive()
  data <- nepse_indices(start = "2026-10-01", end = "2026-10-05")
  expect_equal(nrow(data), 6L)
  expect_equal(sort(unique(data$index_name)),
               c("Banking SubIndex", "Microfinance Index"))
  expect_error(nepse_indices("Not An Index"), "Unknown index")
})

test_that("trading dates come from the manifest without downloads", {
  arc <- local_archive()
  dates <- nepse_dates()
  expect_equal(dates, as.Date(c(
    "2026-09-01", "2026-09-02", "2026-10-01", "2026-10-02", "2026-10-05"
  )))
  expect_length(list.files(file.path(arc$cache, "archive")), 0L)
})

test_that("the latest snapshot and symbol master are available", {
  local_archive()
  latest <- nepse_latest()
  expect_equal(nrow(latest), 3L)
  expect_equal(unique(latest$trading_date), as.Date("2026-10-05"))

  symbols <- nepse_symbols()
  expect_equal(nrow(symbols), 3L)
  expect_equal(sort(symbols$symbol), c("NABIL", "NTC", "UPPER"))
  expect_equal(unique(symbols$parser_version), "symbols-1.0.0")
})

test_that("search matches symbols and company names", {
  local_archive()
  expect_equal(nepse_search("nabil")$symbol, "NABIL")
  expect_equal(nepse_search("doorsanchar")$symbol, "NTC")
  expect_equal(nrow(nepse_search("nothing matches")), 0L)
})

test_that("market summaries aggregate the archive", {
  local_archive()
  market <- nepse_market()
  expect_equal(nrow(market), 5L)
  expect_equal(market$symbols, rep(3L, 5L))
  expect_equal(market$advancers, rep(3L, 5L))
  expect_equal(market$decliners, rep(0L, 5L))
  expect_true(all(market$total_transactions > 0))
  expect_true(all(market$total_turnover > 0))
})

test_that("catalog and metadata report coverage", {
  local_archive()
  catalog <- nepse_catalog()
  expect_equal(nrow(catalog), 3L)
  prices <- catalog[catalog$dataset_id == "prices", , drop = FALSE]
  expect_equal(prices$first_date, as.Date("2026-09-01"))
  expect_equal(prices$last_date, as.Date("2026-10-05"))
  expect_equal(prices$partitions, 2L)
  expect_equal(prices$rows, 15L)

  meta <- nepse_metadata("prices")
  expect_equal(meta$dataset_id, "prices")
  expect_equal(meta$rows, 15L)
  expect_equal(meta$archive_url, nepse_archive_url())
  expect_error(nepse_metadata("nope"), "Unknown dataset")
})

test_that("raw partitions are readable", {
  local_archive()
  raw <- nepse_read_raw("prices", "2026-10")
  expect_equal(nrow(raw), 9L)
  expect_type(raw$close, "character")
  expect_error(nepse_read_raw("prices", "1999-01"), "not available")
  all_raw <- nepse_read_raw("prices")
  expect_equal(names(all_raw), c("2026-09", "2026-10"))
})

test_that("update reports new and unchanged partitions", {
  local_archive()
  fresh <- nepse_update(dry_run = TRUE)
  expect_true(all(fresh$status == "new"))

  invisible(nepse_index())
  again <- nepse_update(dry_run = TRUE)
  expect_true(all(again$status == "unchanged"))

  expect_silent(nepse_update())
})

test_that("a tampered partition is rejected by hash verification", {
  arc <- local_archive()
  target <- file.path(arc$root, "data", "prices", "monthly", "2026-10.csv")
  lines <- readLines(target)
  lines[2] <- sub(",510,", ",9999,", lines[2], fixed = TRUE)
  writeLines(lines, target)
  expect_error(nepse_prices(start = "2026-10-01", end = "2026-10-02"), "hash mismatch")
})

test_that("the cache can be inspected and cleared", {
  arc <- local_archive()
  empty <- nepse_cache_info()
  expect_equal(nrow(empty), 0L)

  invisible(nepse_prices(start = "2026-10-01", end = "2026-10-02"))
  info <- nepse_cache_info()
  expect_equal(nrow(info), 1L)
  expect_equal(info$dataset_id, "prices")
  expect_true(info$bytes > 0)
  expect_equal(attr(info, "cache_dir"), normalizePath(arc$cache, winslash = "/"))

  nepse_clear_cache()
  expect_false(dir.exists(arc$cache))
})
