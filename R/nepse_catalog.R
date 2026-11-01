nepse_coverage <- function(man, dataset_id) {
  rows <- man[man$dataset_id == dataset_id & man$current, , drop = FALSE]
  if (!nrow(rows)) {
    return(list(
      first_date = as.Date(NA), last_date = as.Date(NA), partitions = 0L,
      rows = 0L, updated_at = NA_character_
    ))
  }
  dated <- grepl("^[0-9]{4}-[0-9]{2}$", rows$partition)
  dated_rows <- rows[dated, , drop = FALSE]
  list(
    first_date = if (nrow(dated_rows)) min(dated_rows$first_date, na.rm = TRUE) else as.Date(NA),
    last_date = if (nrow(dated_rows)) max(dated_rows$last_date, na.rm = TRUE) else as.Date(NA),
    partitions = if (dataset_id == "symbols") 1L else sum(dated),
    rows = if (dataset_id == "symbols") sum(rows$rows) else sum(dated_rows$rows),
    updated_at = if (all(is.na(rows$updated_at))) NA_character_ else max(rows$updated_at, na.rm = TRUE)
  )
}

#' List datasets and archive coverage
#'
#' @param archive If `TRUE` (the default), read the archive manifest to report
#'   date coverage, partition counts, and last update times.
#' @param refresh If `TRUE`, re-read the archive manifest first.
#' @return A tibble with one row per dataset.
#' @export
nepse_catalog <- function(archive = TRUE, refresh = FALSE) {
  nepse_assert_flag(archive, "archive")
  nepse_assert_flag(refresh, "refresh")
  reg <- nepse_dataset_registry()
  reg$archive_url <- nepse_archive_url()
  if (!isTRUE(archive)) {
    reg$first_date <- as.Date(NA)
    reg$last_date <- as.Date(NA)
    reg$partitions <- NA_integer_
    reg$rows <- NA_integer_
    reg$updated_at <- NA_character_
    return(tibble::as_tibble(reg))
  }
  man <- nepse_index(force = refresh)$manifest
  coverage <- lapply(reg$dataset_id, function(id) nepse_coverage(man, id))
  reg$first_date <- as.Date(vapply(coverage, function(x) format(x$first_date), character(1)))
  reg$last_date <- as.Date(vapply(coverage, function(x) format(x$last_date), character(1)))
  reg$partitions <- vapply(coverage, function(x) as.integer(x$partitions), integer(1))
  reg$rows <- vapply(coverage, function(x) as.integer(x$rows), integer(1))
  reg$updated_at <- vapply(coverage, function(x) x$updated_at, character(1))
  tibble::as_tibble(reg)
}

#' Search archived symbols
#'
#' @param query Search text; matching is case-insensitive and covers symbols
#'   and company names.
#' @param refresh If `TRUE`, re-read the archive manifest first.
#' @return A tibble of matching symbols.
#' @export
nepse_search <- function(query, refresh = FALSE) {
  nepse_assert_scalar_string(query, "query")
  symbols <- nepse_symbols(refresh = refresh)
  haystack <- tolower(paste(symbols$symbol, symbols$company_name))
  out <- symbols[grepl(tolower(query), haystack, fixed = TRUE), , drop = FALSE]
  out <- out[order(out$symbol), , drop = FALSE]
  tibble::as_tibble(out)
}

#' Describe a dataset and its archive coverage
#'
#' @param dataset_id A dataset ID from [nepse_catalog()].
#' @param refresh If `TRUE`, re-read the archive manifest first.
#' @return A one-row tibble with registry and coverage metadata.
#' @export
nepse_metadata <- function(dataset_id, refresh = FALSE) {
  nepse_assert_scalar_string(dataset_id, "dataset_id")
  reg <- nepse_dataset_row(dataset_id)
  man <- nepse_index(force = refresh)$manifest
  coverage <- nepse_coverage(man, dataset_id)
  out <- tibble::tibble(
    dataset_id = reg$dataset_id,
    title = reg$title,
    frequency = reg$frequency,
    unit = reg$unit,
    source_page = reg$source_page,
    source_url = reg$source_url,
    parser_version = reg$parser_version,
    first_date = coverage$first_date,
    last_date = coverage$last_date,
    partitions = coverage$partitions,
    rows = coverage$rows,
    updated_at = coverage$updated_at,
    archive_url = nepse_archive_url(),
    notes = reg$notes
  )
  tibble::as_tibble(out)
}

#' Read an archived partition without standardizing it
#'
#' @param dataset_id A dataset ID from [nepse_catalog()].
#' @param partition Optional partition label: `"YYYY-MM"` for prices and
#'   indices, or `"current"` for the symbol master. If `NULL`, every current
#'   partition is returned in a named list.
#' @param refresh If `TRUE`, re-read the archive manifest first.
#' @return A tibble of character columns, or a named list of such tibbles.
#' @export
nepse_read_raw <- function(dataset_id, partition = NULL, refresh = FALSE) {
  nepse_assert_scalar_string(dataset_id, "dataset_id")
  nepse_dataset_row(dataset_id)
  man <- nepse_index(force = refresh)$manifest
  man <- man[man$dataset_id == dataset_id & man$current, , drop = FALSE]
  if (!nrow(man)) nepse_stop("The archive has no current `%s` data.", dataset_id)
  man <- man[order(man$partition), , drop = FALSE]
  if (is.null(partition)) {
    current <- man
    if (dataset_id %in% c("prices", "indices")) {
      current <- man[grepl("^[0-9]{4}-[0-9]{2}$", man$partition), , drop = FALSE]
    }
    out <- lapply(seq_len(nrow(current)), function(i) {
      nepse_read_csv(nepse_partition_path(current[i, , drop = FALSE]))
    })
    names(out) <- current$partition
    return(out)
  }
  nepse_assert_scalar_string(partition, "partition")
  hit <- man[man$partition == partition, , drop = FALSE]
  if (!nrow(hit)) {
    nepse_stop(
      "Partition `%s` is not available. Current partitions: %s",
      partition, paste(man$partition, collapse = ", ")
    )
  }
  nepse_read_csv(nepse_partition_path(hit[1L, , drop = FALSE]))
}

#' Check the archive for new or revised partitions
#'
#' Compares the cached archive manifest with the published one. With
#' `dry_run = TRUE` the local index is left untouched.
#'
#' @param dry_run If `TRUE`, report differences without refreshing the cache.
#' @return A tibble with one row per published partition and its status.
#' @export
nepse_update <- function(dry_run = FALSE) {
  nepse_assert_flag(dry_run, "dry_run")
  old <- nepse_read_cached_index()
  manifest <- if (isTRUE(dry_run)) {
    nepse_fetch_manifest()
  } else {
    nepse_index(force = TRUE)$manifest
  }
  manifest <- manifest[manifest$current, , drop = FALSE]
  old_manifest <- if (is.null(old)) nepse_empty_manifest() else old$manifest
  old_manifest <- old_manifest[old_manifest$current, , drop = FALSE]
  keys <- paste(manifest$dataset_id, manifest$partition, sep = "\r")
  old_keys <- paste(old_manifest$dataset_id, old_manifest$partition, sep = "\r")
  match_index <- match(keys, old_keys)
  status <- ifelse(
    is.na(match_index), "new",
    ifelse(manifest$sha256 == old_manifest$sha256[match_index], "unchanged", "updated")
  )
  out <- tibble::tibble(
    dataset_id = manifest$dataset_id,
    partition = manifest$partition,
    status = status,
    rows = manifest$rows,
    sha256 = manifest$sha256,
    updated_at = manifest$updated_at
  )
  out <- out[order(out$dataset_id, out$partition), , drop = FALSE]
  tibble::as_tibble(out)
}
