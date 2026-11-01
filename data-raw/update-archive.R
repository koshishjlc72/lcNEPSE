#!/usr/bin/env Rscript
# Archive one trading day of ShareSansar data into the `data-archive` worktree.
#
# Usage:
#   Rscript data-raw/update-archive.R <archive-dir> [--date=YYYY-MM-DD] \
#     [--datasets=prices,indices]
#
# Run from the package root. The script prints CHANGED=TRUE when it wrote
# something, which the workflow uses to decide whether to commit.

args <- commandArgs(trailingOnly = TRUE)
usage <- paste(
  "usage: Rscript data-raw/update-archive.R <archive-dir>",
  "[--date=YYYY-MM-DD] [--datasets=prices,indices]"
)
if (!length(args)) stop(usage, call. = FALSE)
target <- args[[1L]]
opts <- list()
for (arg in args[-1L]) {
  if (!grepl("^--[a-z]+=", arg)) stop(usage, call. = FALSE)
  key <- sub("^--([a-z]+)=.*$", "\\1", arg)
  opts[[key]] <- sub("^--[a-z]+=", "", arg)
}
date <- if (is.null(opts$date)) NULL else as.Date(opts$date)
datasets <- if (is.null(opts$datasets)) {
  c("prices", "indices")
} else {
  strsplit(opts$datasets, ",", fixed = TRUE)[[1L]]
}

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the package root.", call. = FALSE)
}
if (!dir.exists(target)) dir.create(target, recursive = TRUE)

pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
source("data-raw/archive-lib.R")

changed <- archive_update(target, date = date, datasets = datasets)
cat(if (isTRUE(changed)) "CHANGED=TRUE\n" else "CHANGED=FALSE\n")
