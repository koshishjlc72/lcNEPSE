#!/usr/bin/env Rscript
# Backfill all available ShareSansar NEPSE and sub-index history into monthly
# content-addressed partitions on the data-archive worktree.
#
# Usage:
#   Rscript data-raw/backfill-indices.R <archive-dir> [from] [to]

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 3L) {
  stop(
    "usage: Rscript data-raw/backfill-indices.R <archive-dir> [from] [to]",
    call. = FALSE
  )
}
target <- args[[1L]]
from <- if (length(args) >= 2L) as.Date(args[[2L]]) else as.Date("2010-01-01")
to <- if (length(args) >= 3L) as.Date(args[[3L]]) else Sys.Date()
if (is.na(from) || is.na(to) || from > to) stop("Invalid date range.", call. = FALSE)
if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the package root.", call. = FALSE)
}
if (!dir.exists(target)) dir.create(target, recursive = TRUE)

pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
source("data-raw/archive-lib.R")

indices <- nepse_scrape_index_history(start = from, end = to)
if (!nrow(indices)) stop("The source returned no index history.", call. = FALSE)
manifest <- archive_read_manifest(target)
step <- archive_indices_history(target, manifest, indices)
archive_write_manifest(target, step$manifest)
cat(sprintf(
  "INDEX_HISTORY rows=%d series=%d coverage=%s..%s changed=%s\n",
  nrow(indices), length(unique(indices$index_name)),
  format(min(indices$trading_date)), format(max(indices$trading_date)),
  toupper(as.character(step$changed))
))
