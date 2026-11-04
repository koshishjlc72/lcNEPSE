# Archive writer -----------------------------------------------------------
# Shared by data-raw/update-archive.R and data-raw/backfill-archive.R.
# It runs in the GitHub Actions job, inside a worktree of the `data-archive`
# branch, and writes the layout read by the installed package.

archive_manifest_columns <- function() {
  c(
    "dataset_id", "partition", "path", "first_date", "last_date", "rows",
    "sha256", "updated_at", "source_url", "parser_version", "current"
  )
}

archive_empty_manifest <- function() {
  out <- data.frame(matrix(ncol = length(archive_manifest_columns()), nrow = 0))
  names(out) <- archive_manifest_columns()
  out
}

archive_read_manifest <- function(target) {
  path <- file.path(target, "manifest.csv")
  if (!file.exists(path)) return(archive_empty_manifest())
  utils::read.csv(
    path, stringsAsFactors = FALSE, colClasses = "character",
    check.names = FALSE, na.strings = character()
  )
}

archive_write_manifest <- function(target, manifest) {
  manifest <- manifest[, archive_manifest_columns(), drop = FALSE]
  manifest <- manifest[order(manifest$dataset_id, manifest$partition), , drop = FALSE]
  utils::write.csv(manifest, file.path(target, "manifest.csv"), row.names = FALSE, na = "")
  invisible(manifest)
}

archive_row <- function(dataset_id, partition, path, rows, sha256,
                        first_date, last_date, source_url, parser_version,
                        current = "true") {
  data.frame(
    dataset_id = dataset_id,
    partition = partition,
    path = path,
    first_date = ifelse(is.na(first_date), "", format(as.Date(first_date), "%Y-%m-%d")),
    last_date = ifelse(is.na(last_date), "", format(as.Date(last_date), "%Y-%m-%d")),
    rows = as.character(rows),
    sha256 = sha256,
    updated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    source_url = source_url,
    parser_version = parser_version,
    current = current,
    stringsAsFactors = FALSE
  )
}

archive_drop_path <- function(manifest, path) {
  manifest[manifest$path != path, , drop = FALSE]
}

archive_set_current <- function(manifest, dataset_id, partition, keep_path) {
  hit <- manifest$dataset_id == dataset_id & manifest$partition == partition &
    manifest$path != keep_path
  manifest$current[hit] <- "false"
  manifest
}

archive_csv_hash <- function(data, na = "") {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(data, tmp, row.names = FALSE, na = na)
  digest::digest(file = tmp, algo = "sha256")
}

archive_put_csv <- function(target, relative_path, data) {
  absolute <- file.path(target, relative_path)
  dir.create(dirname(absolute), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data, absolute, row.names = FALSE, na = "")
  list(path = relative_path, sha256 = digest::digest(file = absolute, algo = "sha256"))
}

archive_read_partition <- function(target, relative_path) {
  utils::read.csv(
    file.path(target, relative_path), stringsAsFactors = FALSE,
    colClasses = "character", check.names = FALSE, na.strings = character()
  )
}

# Dated dataset (prices or indices) ---------------------------------------

archive_day <- function(target, manifest, dataset_id, data, parser_version,
                        sort_columns) {
  if (!nrow(data)) return(list(manifest = manifest, changed = FALSE))
  days <- unique(data$trading_date)
  if (length(days) != 1L) {
    stop("Refusing to archive rows from more than one trading date.")
  }
  day <- days[[1L]]
  label <- format(day, "%Y-%m-%d")
  rel <- sprintf(
    "data/%s/daily/%s/%s.csv", dataset_id, format(day, "%Y"), label
  )
  sha <- archive_csv_hash(data)
  existing <- manifest[manifest$path == rel, , drop = FALSE]
  if (nrow(existing) == 1L && identical(existing$sha256[[1L]], sha) &&
      identical(existing$current[[1L]], "true")) {
    return(list(manifest = manifest, changed = FALSE, day = day))
  }

  if (nrow(existing) && file.exists(file.path(target, rel))) {
    old_sha <- existing$sha256[[1L]]
    if (!identical(old_sha, sha)) {
      versioned <- sub(
        "\\.csv$", paste0("@", substr(old_sha, 1L, 8L), ".csv"), rel
      )
      file.rename(file.path(target, rel), file.path(target, versioned))
      manifest$path[manifest$path == rel] <- versioned
      manifest$current[manifest$path == versioned] <- "false"
      message(sprintf(
        "  revision: previous %s content preserved as %s", label, versioned
      ))
    }
  }

  written <- archive_put_csv(target, rel, data)
  manifest <- archive_drop_path(manifest, rel)
  manifest <- rbind(manifest, archive_row(
    dataset_id, label, rel, nrow(data), written$sha256, day, day,
    source_url = data$source_url[[1L]], parser_version = parser_version
  ))
  manifest <- archive_set_current(manifest, dataset_id, label, rel)
  list(manifest = manifest, changed = TRUE, day = day)
}

archive_month <- function(target, manifest, dataset_id, month, parser_version,
                          sort_columns) {
  daily <- manifest[
    manifest$dataset_id == dataset_id & manifest$current == "true" &
      grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", manifest$partition),
    ,
    drop = FALSE
  ]
  daily <- daily[substr(daily$partition, 1L, 7L) == month, , drop = FALSE]
  if (!nrow(daily)) return(list(manifest = manifest, changed = FALSE))

  parts <- lapply(daily$path, function(path) archive_read_partition(target, path))
  data <- do.call(rbind, parts)
  order_args <- c(lapply(sort_columns, function(col) data[[col]]), list(na.last = TRUE))
  data <- data[do.call(order, order_args), , drop = FALSE]
  data <- unique(data)

  rel <- sprintf("data/%s/monthly/%s.csv", dataset_id, month)
  sha <- archive_csv_hash(data)
  existing <- manifest[manifest$path == rel, , drop = FALSE]
  if (nrow(existing) == 1L && identical(existing$sha256[[1L]], sha) &&
      identical(existing$current[[1L]], "true")) {
    return(list(manifest = manifest, changed = FALSE))
  }

  written <- archive_put_csv(target, rel, data)
  first <- min(as.Date(daily$first_date))
  last <- max(as.Date(daily$last_date))
  manifest <- archive_drop_path(manifest, rel)
  manifest <- rbind(manifest, archive_row(
    dataset_id, month, rel, nrow(data), written$sha256, first, last,
    source_url = data$source_url[[1L]], parser_version = parser_version
  ))
  manifest <- archive_set_current(manifest, dataset_id, month, rel)
  list(manifest = manifest, changed = TRUE)
}

# Symbol master ------------------------------------------------------------

archive_symbols <- function(target, manifest, prices) {
  rel <- "data/symbols.csv"
  path <- file.path(target, rel)
  existing <- if (file.exists(path)) {
    utils::read.csv(
      path, stringsAsFactors = FALSE, colClasses = "character",
      check.names = FALSE, na.strings = character()
    )
  } else {
    NULL
  }
  today <- unique(prices$trading_date)
  if (length(today) != 1L) stop("Symbol master requires one trading date.")
  today <- format(today[[1L]], "%Y-%m-%d")

  incoming <- data.frame(
    symbol = prices$symbol,
    company_name = prices$company_name,
    first_seen = today,
    last_seen = today,
    stringsAsFactors = FALSE
  )
  incoming <- incoming[order(incoming$symbol), , drop = FALSE]

  if (is.null(existing) || !nrow(existing)) {
    combined <- incoming
  } else {
    combined <- existing
    matched <- match(incoming$symbol, combined$symbol)
    update <- !is.na(matched)
    combined$first_seen[matched[update]] <- pmin(
      combined$first_seen[matched[update]], incoming$first_seen[update]
    )
    combined$last_seen[matched[update]] <- pmax(
      combined$last_seen[matched[update]], incoming$last_seen[update]
    )
    fresh_name <- incoming$company_name[update]
    combined$company_name[matched[update]] <- ifelse(
      is.na(fresh_name) | !nzchar(fresh_name),
      combined$company_name[matched[update]], fresh_name
    )
    combined <- rbind(combined, incoming[!update, , drop = FALSE])
    combined <- combined[order(combined$symbol), , drop = FALSE]
  }

  sha <- archive_csv_hash(combined)
  existing_row <- manifest[manifest$path == rel, , drop = FALSE]
  if (nrow(existing_row) == 1L && identical(existing_row$sha256[[1L]], sha) &&
      identical(existing_row$current[[1L]], "true")) {
    return(list(manifest = manifest, changed = FALSE))
  }
  written <- archive_put_csv(target, rel, combined)
  manifest <- archive_drop_path(manifest, rel)
  manifest <- rbind(manifest, archive_row(
    "symbols", "current", rel, nrow(combined), written$sha256,
    first_date = NA, last_date = NA,
    source_url = prices$source_url[[1L]], parser_version = "symbols-1.0.0"
  ))
  list(manifest = manifest, changed = TRUE)
}

# Historical index partitions --------------------------------------------

archive_index_history_month <- function(target, manifest, month, incoming,
                                        parser_version = "indices-1.0.0") {
  incoming <- as.data.frame(incoming, stringsAsFactors = FALSE)
  incoming$trading_date <- format(as.Date(incoming$trading_date), "%Y-%m-%d")
  incoming[] <- lapply(incoming, as.character)
  current <- manifest[
    manifest$dataset_id == "indices" & manifest$partition == month &
      manifest$current == "true",
    , drop = FALSE
  ]
  existing <- if (nrow(current) == 1L &&
                  file.exists(file.path(target, current$path[[1L]]))) {
    archive_read_partition(target, current$path[[1L]])
  } else {
    NULL
  }
  if (!is.null(existing)) {
    if (!"ohlc_consistent" %in% names(existing)) {
      old_high <- suppressWarnings(as.numeric(existing$high))
      old_low <- suppressWarnings(as.numeric(existing$low))
      if (any(is.na(old_high) != !nzchar(existing$high)) ||
          any(is.na(old_low) != !nzchar(existing$low))) {
        stop("Existing index partition contains an unparseable high or low value.")
      }
      existing$ohlc_consistent <- is.na(old_high) | is.na(old_low) | old_high >= old_low
    }
    if (!"source_note" %in% names(existing)) {
      existing$source_note <- ifelse(
        tolower(existing$ohlc_consistent) == "true", "",
        "Publisher reports high below low; original values preserved unchanged."
      )
    }
    missing <- setdiff(names(incoming), names(existing))
    if (length(missing)) {
      stop(sprintf(
        "Existing index partition is missing unsupported columns: %s",
        paste(missing, collapse = ", ")
      ))
    }
    existing <- existing[, names(incoming), drop = FALSE]

    # A later fetch of unchanged published values is not a data revision.
    # Retain the first query URL and retrieval timestamp for byte-stable
    # partitions; replace provenance only when a published value changes.
    incoming_key <- paste(incoming$trading_date, incoming$index_name, sep = "\r")
    existing_key <- paste(existing$trading_date, existing$index_name, sep = "\r")
    matched <- match(incoming_key, existing_key)
    compare_fields <- setdiff(names(incoming), c("source_url", "retrieved_at"))
    for (i in which(!is.na(matched))) {
      j <- matched[[i]]
      same <- all(vapply(compare_fields, function(field) {
        identical(incoming[[field]][[i]], existing[[field]][[j]])
      }, logical(1)))
      if (same) {
        incoming$source_url[[i]] <- existing$source_url[[j]]
        incoming$retrieved_at[[i]] <- existing$retrieved_at[[j]]
      }
    }
  }
  combined <- if (is.null(existing)) incoming else rbind(existing, incoming)
  key <- paste(combined$trading_date, combined$index_name, sep = "\r")
  combined <- combined[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  combined <- combined[
    order(combined$trading_date, combined$index_name, na.last = TRUE),
    , drop = FALSE
  ]
  sha <- archive_csv_hash(combined)
  if (nrow(current) == 1L && identical(current$sha256[[1L]], sha)) {
    return(list(manifest = manifest, changed = FALSE))
  }

  # Historical responses can be revised by the publisher. Use a
  # content-addressed path so every previously published monthly file remains
  # available while exactly one version is marked current in the manifest.
  rel <- sprintf(
    "data/indices/monthly/%s@%s.csv", month, substr(sha, 1L, 12L)
  )
  written <- archive_put_csv(target, rel, combined)
  first <- min(as.Date(combined$trading_date))
  last <- max(as.Date(combined$trading_date))
  manifest <- archive_drop_path(manifest, rel)
  manifest <- rbind(manifest, archive_row(
    "indices", month, rel, nrow(combined), written$sha256, first, last,
    source_url = "https://www.sharesansar.com/index-history-data",
    parser_version = parser_version
  ))
  manifest <- archive_set_current(manifest, "indices", month, rel)
  list(manifest = manifest, changed = TRUE)
}

archive_indices_history <- function(target, manifest, indices,
                                    parser_version = "indices-1.0.0") {
  if (!nrow(indices)) return(list(manifest = manifest, changed = FALSE))
  nepse_validate_indices(indices, error = TRUE)
  months <- sort(unique(format(indices$trading_date, "%Y-%m")))
  changed <- FALSE
  for (month in months) {
    part <- indices[format(indices$trading_date, "%Y-%m") == month, , drop = FALSE]
    step <- archive_index_history_month(
      target, manifest, month, part, parser_version = parser_version
    )
    manifest <- step$manifest
    changed <- changed || step$changed
  }
  list(manifest = manifest, changed = changed)
}

# Top-level update ---------------------------------------------------------

archive_update <- function(target, date = NULL, datasets = c("prices", "indices")) {
  stopifnot(dir.exists(target))
  manifest <- archive_read_manifest(target)
  changed <- FALSE

  prices <- NULL
  if ("prices" %in% datasets) {
    prices <- nepse_scrape_prices(date)
    nepse_validate_prices(prices, error = TRUE, min_rows = 20L)
    step <- archive_day(
      target, manifest, "prices", prices, "prices-1.0.0",
      c("trading_date", "symbol")
    )
    manifest <- step$manifest
    changed <- changed || step$changed
    if (step$changed) {
      month <- format(unique(prices$trading_date), "%Y-%m")
      monthly <- archive_month(
        target, manifest, "prices", month, "prices-1.0.0",
        c("trading_date", "symbol")
      )
      manifest <- monthly$manifest
      changed <- changed || monthly$changed
    }
    symbols <- archive_symbols(target, manifest, prices)
    manifest <- symbols$manifest
    changed <- changed || symbols$changed
    message(sprintf(
      "prices: %s rows for %s (%s)", nrow(prices),
      format(unique(prices$trading_date), "%Y-%m-%d"),
      if (step$changed) "archived" else "already current"
    ))
  }

  if ("indices" %in% datasets) {
    index_start <- if (is.null(date)) Sys.Date() - 45L else as.Date(date)
    index_end <- if (is.null(date)) Sys.Date() else as.Date(date)
    indices <- nepse_scrape_index_history(start = index_start, end = index_end)
    if (!nrow(indices)) {
      message(sprintf(
        "indices: no published observations from %s through %s",
        format(index_start, "%Y-%m-%d"), format(index_end, "%Y-%m-%d")
      ))
    } else {
      step <- archive_indices_history(target, manifest, indices)
      manifest <- step$manifest
      changed <- changed || step$changed
      message(sprintf(
        "indices: %s rows across %s series from %s through %s (%s)",
        nrow(indices), length(unique(indices$index_name)),
        format(min(indices$trading_date), "%Y-%m-%d"),
        format(max(indices$trading_date), "%Y-%m-%d"),
        if (step$changed) "archived" else "already current"
      ))
    }
  }

  archive_write_manifest(target, manifest)
  invisible(changed)
}
