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
    indices <- nepse_scrape_indices()
    nepse_validate_indices(indices, error = TRUE, min_rows = 5L)
    index_day <- unique(indices$trading_date)
    if (!is.null(date) && !identical(index_day, as.Date(date))) {
      message(sprintf(
        "indices: skipping; the site only exposes the latest session (%s), not %s",
        format(index_day, "%Y-%m-%d"), format(as.Date(date), "%Y-%m-%d")
      ))
    } else {
      step <- archive_day(
        target, manifest, "indices", indices, "indices-1.0.0",
        c("trading_date", "index_name")
      )
      manifest <- step$manifest
      changed <- changed || step$changed
      if (step$changed) {
        month <- format(index_day, "%Y-%m")
        monthly <- archive_month(
          target, manifest, "indices", month, "indices-1.0.0",
          c("trading_date", "index_name")
        )
        manifest <- monthly$manifest
        changed <- changed || monthly$changed
      }
      message(sprintf(
        "indices: %s rows for %s (%s)", nrow(indices),
        format(index_day, "%Y-%m-%d"),
        if (step$changed) "archived" else "already current"
      ))
    }
  }

  archive_write_manifest(target, manifest)
  invisible(changed)
}
