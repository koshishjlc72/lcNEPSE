nepse_cache_dir <- function() {
  path <- getOption("lcNEPSE.cache_dir", tools::R_user_dir("lcNEPSE", "cache"))
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

nepse_cache_paths <- function() {
  root <- nepse_cache_dir()
  list(
    root = root,
    archive = file.path(root, "archive"),
    index = file.path(root, "archive-index.rds")
  )
}

nepse_ensure_cache <- function() {
  paths <- nepse_cache_paths()
  dir.create(paths$archive, recursive = TRUE, showWarnings = FALSE)
  paths
}

nepse_index_ttl <- function() {
  ttl <- getOption("lcNEPSE.index_ttl", 900)
  if (!is.numeric(ttl) || length(ttl) != 1L || is.na(ttl) || ttl < 0) {
    nepse_stop("`lcNEPSE.index_ttl` must be one non-negative number of seconds.")
  }
  as.numeric(ttl)
}

nepse_read_cached_index <- function() {
  path <- nepse_cache_paths()$index
  if (!file.exists(path)) return(NULL)
  out <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(out) || !all(c("manifest", "fetched_at", "mirror") %in% names(out))) {
    return(NULL)
  }
  out
}

nepse_write_cached_index <- function(index) {
  path <- nepse_cache_paths()$index
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(index, tmp)
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
  }
  invisible(path)
}

#' Inspect the lcNEPSE cache
#'
#' Reports the archive partitions that have been downloaded to the user cache.
#'
#' @return A tibble describing cached archive content.
#' @export
nepse_cache_info <- function() {
  index <- nepse_read_cached_index()
  if (is.null(index)) {
    out <- tibble::tibble(
      dataset_id = character(), partition = character(), rows = integer(),
      bytes = numeric(), path = character(), sha256 = character()
    )
  } else {
    files <- list.files(
      nepse_cache_paths()$archive, recursive = TRUE, full.names = TRUE,
      pattern = "\\.csv$"
    )
    if (!length(files)) {
      out <- tibble::tibble(
        dataset_id = character(), partition = character(), rows = integer(),
        bytes = numeric(), path = character(), sha256 = character()
      )
    } else {
      parts <- do.call(rbind, strsplit(
        sub(paste0(nepse_cache_paths()$archive, "/"), "", files, fixed = TRUE),
        "/", fixed = TRUE
      ))
      out <- tibble::tibble(
        dataset_id = parts[, 1L],
        partition = parts[, 2L],
        bytes = as.numeric(file.info(files)$size),
        path = files
      )
      out$rows <- NA_integer_
      out$sha256 <- sub("\\.csv$", "", basename(files))
      man <- index$manifest
      key <- paste(man$dataset_id, man$partition, man$sha256, sep = "\r")
      out$rows <- man$rows[match(
        paste(out$dataset_id, out$partition, out$sha256, sep = "\r"), key
      )]
      out <- out[, c("dataset_id", "partition", "rows", "bytes", "path", "sha256")]
    }
  }
  attr(out, "cache_dir") <- nepse_cache_dir()
  attr(out, "index_fetched_at") <- if (is.null(index)) NA_character_ else index$fetched_at
  tibble::as_tibble(out)
}

#' Clear the lcNEPSE runtime cache
#'
#' Removes the cached archive index and downloaded partitions from the user
#' cache. Installed package files are never touched.
#'
#' @return Invisibly, `TRUE`.
#' @export
nepse_clear_cache <- function() {
  path <- nepse_cache_dir()
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}
