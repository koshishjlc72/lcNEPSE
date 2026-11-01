# End-to-end tests for the data-raw archiver, with scrapers stubbed out.
# data-raw/ is excluded from the built package, so these skip during R CMD check.

archive_lib <- function() {
  testthat::test_path("..", "..", "data-raw", "archive-lib.R")
}

stub_price_day <- function(day = as.Date("2026-10-30"), n = 25L, shift = 0) {
  i <- seq_len(n)
  tibble::tibble(
    trading_date = as.Date(rep(day, n)),
    symbol = sprintf("SYM%02d", i),
    company_name = sprintf("Symbol %02d Limited", i),
    conf = 20 + i / 10,
    open = 100 + i,
    high = 110 + i,
    low = 90 + i,
    close = 105 + i + shift,
    ltp = 105 + i + shift,
    close_ltp_diff = 0,
    close_ltp_pct = 0,
    vwap = 106 + i,
    volume = 1000 * i,
    prev_close = 104 + i,
    turnover = 1000 * i * (106 + i),
    transactions = i,
    diff = 1,
    range = 20,
    diff_pct = 1,
    range_pct = 5,
    vwap_pct = 0.5,
    avg_120d = 100 + i,
    avg_180d = 98 + i,
    high_52w = 200 + i,
    low_52w = 50 + i,
    source_url = "https://www.sharesansar.com/today-share-price",
    retrieved_at = "2026-10-30T10:00:00Z"
  )
}

stub_index_day <- function(day = as.Date("2026-10-30"), n = 6L) {
  i <- seq_len(n)
  tibble::tibble(
    trading_date = as.Date(rep(day, n)),
    index_name = sprintf("Sector %d Index", i),
    open = 1000 + i,
    high = 1010 + i,
    low = 990 + i,
    close = 1005 + i,
    turnover = 1e7 * i,
    point_change = i,
    pct_change = 0.5,
    high_52w = 1200 + i,
    low_52w = 800 + i,
    source_url = "https://www.sharesansar.com/",
    retrieved_at = "2026-10-30T10:00:00Z"
  )
}

test_that("the archiver writes a complete archive readable by the package", {
  lib <- archive_lib()
  skip_if_not(file.exists(lib), "data-raw is not available in the built package")

  target <- withr::local_tempdir()
  cache <- withr::local_tempdir()
  source(lib, local = TRUE)
  nepse_scrape_prices <- function(date = NULL) stub_price_day()
  nepse_scrape_indices <- function() stub_index_day()

  expect_true(archive_update(target))
  expect_true(file.exists(file.path(target, "manifest.csv")))
  expect_true(file.exists(file.path(target, "data", "prices", "daily", "2026", "2026-10-30.csv")))
  expect_true(file.exists(file.path(target, "data", "prices", "monthly", "2026-10.csv")))
  expect_true(file.exists(file.path(target, "data", "symbols.csv")))
  expect_true(file.exists(file.path(target, "data", "indices", "monthly", "2026-10.csv")))

  withr::local_options(
    lcNEPSE.mirror_dir = target,
    lcNEPSE.cache_dir = cache,
    lcNEPSE.index_ttl = 0
  )
  catalog <- nepse_catalog(refresh = TRUE)
  expect_equal(sort(catalog$dataset_id), c("indices", "prices", "symbols"))
  expect_equal(catalog$rows[catalog$dataset_id == "prices"], 25L)

  prices <- nepse_prices()
  expect_equal(nrow(prices), 25L)
  expect_equal(unique(prices$trading_date), as.Date("2026-10-30"))
  expect_silent(nepse_validate(prices))

  indices <- nepse_indices()
  expect_equal(nrow(indices), 6L)
  expect_silent(nepse_validate(indices))

  expect_equal(nrow(nepse_symbols()), 25L)
  expect_equal(nrow(nepse_market()), 1L)
})

test_that("re-archiving an unchanged day writes nothing", {
  lib <- archive_lib()
  skip_if_not(file.exists(lib), "data-raw is not available in the built package")

  target <- withr::local_tempdir()
  source(lib, local = TRUE)
  nepse_scrape_prices <- function(date = NULL) stub_price_day()
  nepse_scrape_indices <- function() stub_index_day()

  expect_true(archive_update(target))
  manifest_before <- readLines(file.path(target, "manifest.csv"))
  expect_false(archive_update(target))
  expect_equal(readLines(file.path(target, "manifest.csv")), manifest_before)
})

test_that("a revised day keeps the previous content as a versioned file", {
  lib <- archive_lib()
  skip_if_not(file.exists(lib), "data-raw is not available in the built package")

  target <- withr::local_tempdir()
  cache <- withr::local_tempdir()
  source(lib, local = TRUE)
  nepse_scrape_prices <- function(date = NULL) stub_price_day()
  nepse_scrape_indices <- function() stub_index_day()
  archive_update(target)

  nepse_scrape_prices <- function(date = NULL) stub_price_day(shift = 7)
  expect_true(archive_update(target))

  daily <- list.files(file.path(target, "data", "prices", "daily", "2026"), full.names = TRUE)
  expect_equal(length(daily), 2L)
  expect_true(any(grepl("@[0-9a-f]{8}\\.csv$", daily)))

  withr::local_options(
    lcNEPSE.mirror_dir = target,
    lcNEPSE.cache_dir = cache,
    lcNEPSE.index_ttl = 0
  )
  prices <- nepse_prices()
  expect_equal(nrow(prices), 25L)
  expect_equal(unique(prices$close), 105 + seq_len(25) + 7)
})

test_that("a second trading day extends the monthly partition", {
  lib <- archive_lib()
  skip_if_not(file.exists(lib), "data-raw is not available in the built package")

  target <- withr::local_tempdir()
  source(lib, local = TRUE)
  nepse_scrape_prices <- function(date = NULL) stub_price_day()
  nepse_scrape_indices <- function() stub_index_day()
  archive_update(target)

  nepse_scrape_prices <- function(date = NULL) stub_price_day(day = as.Date("2026-10-29"))
  nepse_scrape_indices <- function() stub_index_day(day = as.Date("2026-10-29"))
  archive_update(target)

  monthly <- utils::read.csv(file.path(target, "data", "prices", "monthly", "2026-10.csv"))
  expect_equal(nrow(monthly), 50L)
  expect_equal(sort(unique(monthly$trading_date)),
               c("2026-10-29", "2026-10-30"))
})
