#!/usr/bin/env Rscript
# Gate the published archive: read it exactly as an installed lcNEPSE would,
# with an empty cache, and fail the workflow if anything is inconsistent.
#
# Usage:
#   Rscript data-raw/verify-archive.R <archive-dir>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("usage: Rscript data-raw/verify-archive.R <archive-dir>", call. = FALSE)
}
target <- args[[1L]]
if (!dir.exists(target)) stop("Archive directory does not exist.", call. = FALSE)
if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the package root.", call. = FALSE)
}

pkgload::load_all(".", quiet = TRUE, export_all = TRUE)

cache <- tempfile("lcNEPSE-verify-")
dir.create(cache, recursive = TRUE)
on.exit(unlink(cache, recursive = TRUE, force = TRUE), add = TRUE)
options(
  lcNEPSE.mirror_dir = normalizePath(target, winslash = "/"),
  lcNEPSE.cache_dir = cache,
  lcNEPSE.index_ttl = 0
)

fail <- function(...) stop(sprintf(...), call. = FALSE)

catalog <- nepse_catalog(refresh = TRUE)
stopifnot(is.data.frame(catalog))
if (nrow(catalog) != 3L) fail("Expected three registered datasets, found %d.", nrow(catalog))
for (i in seq_len(nrow(catalog))) {
  if (catalog$rows[[i]] < 1L) fail("Dataset `%s` has no archived rows.", catalog$dataset_id[[i]])
}

prices <- nepse_prices()
invisible(nepse_validate(prices))
dates <- nepse_dates()
if (!length(dates)) fail("No trading dates in the archive.")
if (anyNA(prices$trading_date)) fail("Prices contain missing trading dates.")
if (any(!prices$trading_date %in% dates)) fail("Prices contain dates absent from the manifest.")
if (anyDuplicated(paste(prices$trading_date, prices$symbol))) fail("Duplicate symbol-date rows.")
if (any(prices$close < 0, na.rm = TRUE)) fail("Negative closing prices found.")
# Old sessions can publish 0 for an instrument that did not trade. The latest
# session must be fully priced, so a zero there is a real failure.
latest_day <- max(prices$trading_date)
if (any(prices$close[prices$trading_date == latest_day] <= 0, na.rm = TRUE)) {
  fail("Non-positive closing price in the latest session (%s).", format(latest_day))
}
if (any(prices$high < prices$low, na.rm = TRUE)) fail("A high price is below its low.")

indices <- nepse_indices()
invisible(nepse_validate(indices))
if (!"NEPSE Index" %in% indices$index_name) fail("The NEPSE Index series is absent.")
if (length(unique(indices$index_name)) < 10L) {
  fail("Only %d index series are present.", length(unique(indices$index_name)))
}
if (min(indices$trading_date) >= min(dates)) {
  fail("Historical index coverage does not predate the share-price archive.")
}

symbols <- nepse_symbols()
if (any(!unique(prices$symbol) %in% symbols$symbol)) {
  fail("Some priced symbols are missing from the symbol master.")
}

market <- nepse_market()
if (nrow(market) != length(dates)) fail("Market summary does not cover every trading date.")
if (any(market$total_turnover < 0)) fail("Negative turnover in the market summary.")

# Every monthly partition must be byte-identical to the hash in the manifest.
manifest <- nepse_index(force = TRUE)$manifest
monthly <- manifest[
  manifest$dataset_id %in% c("prices", "indices") &
    grepl("^[0-9]{4}-[0-9]{2}$", manifest$partition),
  ,
  drop = FALSE
]
for (i in seq_len(nrow(monthly))) {
  path <- file.path(target, monthly$path[[i]])
  if (!file.exists(path)) fail("Missing partition file: %s", monthly$path[[i]])
  observed <- digest::digest(file = path, algo = "sha256")
  if (!identical(observed, monthly$sha256[[i]])) {
    fail("Hash mismatch for %s", monthly$path[[i]])
  }
}

cat(sprintf(
  "VERIFIED datasets=%d trading_dates=%d price_rows=%d index_rows=%d symbols=%d partitions=%d\n",
  nrow(catalog), length(dates), nrow(prices), nrow(indices), nrow(symbols),
  nrow(monthly)
))
cat(sprintf(
  "Coverage %s to %s\n", format(min(dates)), format(max(dates))
))
