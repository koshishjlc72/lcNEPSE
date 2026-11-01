# Archive location ---------------------------------------------------------

nepse_default_mirror_url <- function() {
  "https://raw.githubusercontent.com/koshishjlc72/lcNEPSE/data-archive"
}

nepse_assert_trusted_url <- function(url) {
  nepse_assert_scalar_string(url, "url")
  if (!grepl("^https://", url, ignore.case = TRUE)) {
    nepse_stop("Only HTTPS archive downloads are allowed: %s", url)
  }
  host <- sub("^https://([^/:]+).*$", "\\1", tolower(url))
  if (!identical(host, "raw.githubusercontent.com")) {
    nepse_stop(
      "Refusing an archive download outside raw.githubusercontent.com: %s", host
    )
  }
  invisible(TRUE)
}

# Returns a descriptor for the active archive location. `mirror_dir` exists for
# offline and test use, and for people who mirror the data branch internally.
nepse_mirror <- function() {
  dir <- getOption("lcNEPSE.mirror_dir", NULL)
  if (!is.null(dir)) {
    nepse_assert_scalar_string(dir, "lcNEPSE.mirror_dir")
    dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
    if (!dir.exists(dir)) {
      nepse_stop("`lcNEPSE.mirror_dir` does not exist: %s", dir)
    }
    return(list(mode = "dir", base = dir))
  }
  url <- getOption("lcNEPSE.mirror_url", nepse_default_mirror_url())
  nepse_assert_trusted_url(url)
  list(mode = "url", base = sub("/+$", "", url))
}

#' Archive location used by lcNEPSE
#'
#' Returns the GitHub data branch from which partitions are read, or the local
#' directory when `options(lcNEPSE.mirror_dir = ...)` is set.
#'
#' @return A single character value.
#' @export
nepse_archive_url <- function() {
  nepse_mirror()$base
}

nepse_join_path <- function(base, path) {
  paste0(sub("/+$", "", base), "/", sub("^/+", "", path))
}

nepse_fetch_archive_file <- function(relative_path, dest, mirror = nepse_mirror()) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (identical(mirror$mode, "dir")) {
    source_path <- file.path(mirror$base, relative_path)
    if (!file.exists(source_path)) {
      nepse_stop("Archive file is missing from the local mirror: %s", relative_path)
    }
    if (!file.copy(source_path, dest, overwrite = TRUE)) {
      nepse_stop("Failed to copy archive file `%s`.", relative_path)
    }
    return(dest)
  }
  url <- nepse_join_path(mirror$base, relative_path)
  nepse_assert_trusted_url(url)
  res <- nepse_http_get_raw(url, attempts = 3L)
  writeBin(res$content, dest)
  dest
}

# Manifest ----------------------------------------------------------------

nepse_manifest_columns <- function() {
  c(
    "dataset_id", "partition", "path", "first_date", "last_date", "rows",
    "sha256", "updated_at", "source_url", "parser_version", "current"
  )
}

nepse_empty_manifest <- function() {
  tibble::tibble(
    dataset_id = character(), partition = character(), path = character(),
    first_date = as.Date(character()), last_date = as.Date(character()),
    rows = integer(), sha256 = character(), updated_at = character(),
    source_url = character(), parser_version = character(), current = logical()
  )
}

nepse_validate_manifest <- function(x) {
  required <- nepse_manifest_columns()
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    nepse_stop(
      "Archive manifest is missing column(s): %s", paste(missing, collapse = ", ")
    )
  }
  x <- x[, required, drop = FALSE]
  if (!nrow(x)) nepse_stop("Archive manifest is empty.")

  known <- nepse_dataset_ids()
  bad_dataset <- setdiff(unique(x$dataset_id), known)
  if (length(bad_dataset)) {
    nepse_stop(
      "Archive manifest references unregistered dataset(s): %s",
      paste(bad_dataset, collapse = ", ")
    )
  }
  if (anyNA(x$dataset_id) || any(!nzchar(x$dataset_id))) {
    nepse_stop("Archive manifest contains a missing dataset ID.")
  }
  if (anyNA(x$partition) || any(!nzchar(x$partition))) {
    nepse_stop("Archive manifest contains a missing partition label.")
  }
  if (anyDuplicated(x$path)) {
    nepse_stop("Archive manifest contains duplicate paths.")
  }

  safe_path <- grepl("^[A-Za-z0-9_./@-]+$", x$path) &
    !grepl("(^|/)\\.\\.(/|$)", x$path) &
    !startsWith(x$path, "/") &
    !grepl("\\\\", x$path)
  if (any(!safe_path)) nepse_stop("Archive manifest contains an unsafe asset path.")

  if (any(!grepl("^[0-9a-f]{64}$", x$sha256))) {
    nepse_stop("Archive manifest contains an invalid SHA-256 value.")
  }

  current <- tolower(trimws(x$current))
  if (any(!current %in% c("true", "false"))) {
    nepse_stop("Archive manifest contains an invalid current flag.")
  }
  x$current <- current == "true"

  x$rows <- suppressWarnings(as.integer(x$rows))
  if (anyNA(x$rows) || any(x$rows < 1L)) {
    nepse_stop("Archive manifest contains an invalid row count.")
  }

  x$first_date <- as.Date(ifelse(nzchar(x$first_date), x$first_date, NA_character_))
  x$last_date <- as.Date(ifelse(nzchar(x$last_date), x$last_date, NA_character_))
  dated <- x$dataset_id %in% c("prices", "indices")
  if (any(dated & (is.na(x$first_date) | is.na(x$last_date)))) {
    nepse_stop("Dated archive partitions must carry first and last dates.")
  }
  if (any(dated & x$first_date > x$last_date)) {
    nepse_stop("Archive manifest contains a partition whose first date is after its last date.")
  }

  if (anyNA(x$source_url) || any(!nzchar(x$source_url))) {
    nepse_stop("Archive manifest contains a missing source URL.")
  }
  if (any(!grepl("^https://www\\.sharesansar\\.com/", x$source_url))) {
    nepse_stop("Archive manifest claims a source outside sharesansar.com.")
  }

  expected_parser <- vapply(
    x$dataset_id, nepse_parser_version_for, character(1), USE.NAMES = FALSE
  )
  if (any(x$parser_version != expected_parser)) {
    bad <- which(x$parser_version != expected_parser)[[1L]]
    nepse_stop(
      "Archive parser version for `%s` is `%s`; this package requires `%s`.",
      x$dataset_id[[bad]], x$parser_version[[bad]], expected_parser[[bad]]
    )
  }

  dated_current <- x[x$current & x$dataset_id %in% c("prices", "indices"), , drop = FALSE]
  if (nrow(dated_current)) {
    key <- paste(dated_current$dataset_id, dated_current$partition, sep = "\r")
    duplicated_key <- duplicated(key)
    if (any(duplicated_key)) {
      nepse_stop(
        "Archive manifest has more than one current file for partition `%s`.",
        dated_current$partition[[which(duplicated_key)[[1L]]]]
      )
    }
  }
  symbols_current <- sum(x$current & x$dataset_id == "symbols")
  if (symbols_current != 1L) {
    nepse_stop("Archive manifest must contain exactly one current symbol master.")
  }

  x$updated_at <- ifelse(nzchar(x$updated_at), x$updated_at, NA_character_)
  tibble::as_tibble(x)
}

nepse_fetch_manifest <- function(mirror = nepse_mirror()) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  nepse_fetch_archive_file("manifest.csv", tmp, mirror = mirror)
  out <- nepse_read_csv(tmp, required = nepse_manifest_columns())
  nepse_validate_manifest(out)
}

#' Read the archive index
#'
#' The index is the validated `manifest.csv` of the GitHub data branch, cached
#' locally for `getOption("lcNEPSE.index_ttl", 900)` seconds.
#'
#' @param force If `TRUE`, bypass the cached index and re-read the manifest.
#' @return A list with `manifest`, `fetched_at`, and `mirror`.
#' @export
nepse_index <- function(force = FALSE) {
  nepse_assert_flag(force, "force")
  mirror <- nepse_mirror()
  cached <- nepse_read_cached_index()
  if (!isTRUE(force) && !is.null(cached)) {
    age <- as.numeric(
      difftime(Sys.time(), cached$fetched_at, units = "secs")
    )
    same_mirror <- identical(cached$mirror$base, mirror$base) &&
      identical(cached$mirror$mode, mirror$mode)
    if (same_mirror && is.finite(age) && age >= 0 && age < nepse_index_ttl()) {
      return(cached)
    }
  }
  manifest <- nepse_fetch_manifest(mirror)
  index <- list(
    manifest = manifest,
    fetched_at = Sys.time(),
    mirror = mirror
  )
  nepse_write_cached_index(index)
  index
}

nepse_manifest_for <- function(dataset_id, start = NULL, end = NULL) {
  man <- nepse_index()$manifest
  man <- man[man$dataset_id == dataset_id & man$current, , drop = FALSE]
  if (!nrow(man)) {
    nepse_stop(
      "The archive has no current `%s` data. Run nepse_update() or check %s.",
      dataset_id, nepse_archive_url()
    )
  }
  if (dataset_id %in% c("prices", "indices")) {
    # Analysis reads the monthly partition; the dated daily files in the same
    # dataset are the provenance layer and are requested individually.
    man <- man[grepl("^[0-9]{4}-[0-9]{2}$", man$partition), , drop = FALSE]
  }
  if (!is.null(start) || !is.null(end)) {
    start <- start %||% as.Date("1900-01-01")
    end <- end %||% as.Date("2999-12-31")
    man <- man[
      (is.na(man$first_date) | man$last_date >= start) &
        (is.na(man$last_date) | man$first_date <= end),
      ,
      drop = FALSE
    ]
  }
  if (!nrow(man)) {
    nepse_stop(
      "The archive has no `%s` data for the requested date range.", dataset_id
    )
  }
  man[order(man$partition), , drop = FALSE]
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Partitions --------------------------------------------------------------

nepse_partition_cache_path <- function(row) {
  root <- nepse_ensure_cache()$archive
  dir <- file.path(root, row$dataset_id[[1L]], row$partition[[1L]])
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, paste0(row$sha256[[1L]], ".csv"))
}

nepse_partition_path <- function(row) {
  target <- nepse_partition_cache_path(row)
  if (file.exists(target)) return(target)
  tmp <- tempfile(tmpdir = dirname(target), fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  nepse_fetch_archive_file(row$path[[1L]], tmp)
  observed <- nepse_file_sha256(tmp)
  if (!identical(observed, row$sha256[[1L]])) {
    nepse_stop(
      "Archive hash mismatch for `%s`: manifest declares %s but downloaded %s.",
      row$path[[1L]], row$sha256[[1L]], observed
    )
  }
  if (!file.rename(tmp, target)) {
    if (!file.copy(tmp, target, overwrite = TRUE)) {
      nepse_stop("Failed to store archive partition `%s`.", row$path[[1L]])
    }
    unlink(tmp)
  }
  target
}
