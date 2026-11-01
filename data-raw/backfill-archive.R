#!/usr/bin/env Rscript
# Backfill historical trading days through ShareSansar's date filter.
#
# Usage:
#   Rscript data-raw/backfill-archive.R <archive-dir> <from> <to>
#
# Days that ShareSansar has no data for (weekends and market holidays) are
# skipped with a message; the run continues. Every archived day is validated
# by update-archive.R, and the workflow only publishes after verify-archive.R
# passes.

args <- commandArgs(trailingOnly = TRUE)
usage <- "usage: Rscript data-raw/backfill-archive.R <archive-dir> <from> <to>"
if (length(args) != 3L) stop(usage, call. = FALSE)
target <- args[[1L]]
from <- as.Date(args[[2L]])
to <- as.Date(args[[3L]])
if (is.na(from) || is.na(to) || from > to) stop(usage, call. = FALSE)

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the package root.", call. = FALSE)
}
if (!dir.exists(target)) dir.create(target, recursive = TRUE)

pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
source("data-raw/archive-lib.R")

dates <- seq(from, to, by = "day")
done <- 0L
skipped <- character()

for (day in dates) {
  message(sprintf("== %s", format(day, "%Y-%m-%d")))
  result <- tryCatch(
    archive_update(target, date = day, datasets = c("prices", "indices")),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    skipped <- c(skipped, sprintf("%s (%s)", format(day), conditionMessage(result)))
    message(sprintf("  skipped: %s", conditionMessage(result)))
  } else {
    done <- done + 1L
  }
}

message(sprintf("Archived %d day(s); skipped %d.", done, length(skipped)))
if (length(skipped)) {
  message("Skipped days:\n  ", paste(skipped, collapse = "\n  "))
}
cat(if (done > 0L) "CHANGED=TRUE\n" else "CHANGED=FALSE\n")
