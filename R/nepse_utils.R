# Internal utilities ------------------------------------------------------

nepse_stop <- function(..., call. = FALSE) {
  stop(sprintf(...), call. = call.)
}

nepse_now_utc <- function() {
  format(Sys.time(), tz = "UTC", usetz = TRUE)
}

nepse_today <- function() {
  as.Date(format(Sys.time(), tz = "Asia/Kathmandu"))
}

nepse_clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  stringr::str_squish(x)
}

nepse_assert_scalar_string <- function(x, name, allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(TRUE))
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    nepse_stop("`%s` must be one non-empty character value.", name)
  }
  invisible(TRUE)
}

nepse_assert_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    nepse_stop("`%s` must be `TRUE` or `FALSE`.", name)
  }
  invisible(TRUE)
}

nepse_empty_to_na <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "-", "--", "...", "NA", "N/A", "n.a.", "N/A.")] <- NA_character_
  x
}

# Strict numeric converter used on archived files: an unexpected token is a
# corruption signal, not something to coerce silently to NA.
nepse_parse_number <- function(x, field, n = length(x)) {
  raw <- nepse_empty_to_na(x)
  clean <- gsub(",", "", raw, fixed = TRUE)
  clean <- gsub("\u00a0", "", clean, fixed = TRUE)
  out <- suppressWarnings(as.numeric(clean))
  bad <- !is.na(clean) & is.na(out)
  if (any(bad)) {
    nepse_stop(
      "Field `%s` contains %d non-numeric value(s); first offending value: `%s`.",
      field, sum(bad), clean[which(bad)[[1L]]]
    )
  }
  if (length(out) != n) out <- rep_len(out, n)
  out
}

nepse_parse_percent <- function(x, field) {
  raw <- nepse_empty_to_na(x)
  raw <- gsub("%", "", raw, fixed = TRUE)
  nepse_parse_number(raw, field)
}

nepse_parse_logical <- function(x, field) {
  raw <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(raw))
  out[raw == "true"] <- TRUE
  out[raw == "false"] <- FALSE
  if (anyNA(out)) {
    nepse_stop("Field `%s` contains a missing or invalid logical value.", field)
  }
  out
}

nepse_parse_date <- function(x, field, allow_na = FALSE) {
  x <- trimws(as.character(x))
  out <- as.Date(x)
  if (!allow_na && anyNA(out)) {
    nepse_stop("Field `%s` contains a missing or unparseable ISO date.", field)
  }
  out
}

nepse_parse_date_arg <- function(x, name) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "Date")) {
    if (length(x) != 1L || is.na(x)) nepse_stop("`%s` must be one non-missing date.", name)
    return(x)
  }
  nepse_assert_scalar_string(x, name)
  out <- as.Date(x, format = "%Y-%m-%d")
  if (is.na(out) || !identical(format(out, "%Y-%m-%d"), x)) {
    nepse_stop("`%s` must use ISO date format YYYY-MM-DD.", name)
  }
  out
}

nepse_iso_date <- function(x) {
  format(as.Date(x), "%Y-%m-%d")
}

nepse_month_key <- function(x) {
  format(as.Date(x), "%Y-%m")
}

nepse_in_range <- function(x, start = NULL, end = NULL) {
  keep <- rep(TRUE, length(x))
  if (!is.null(start)) keep <- keep & x >= start
  if (!is.null(end)) keep <- keep & x <= end
  keep
}

nepse_package_file <- function(...) {
  path <- system.file(..., package = "lcNEPSE")
  if (!nzchar(path)) {
    candidate <- file.path(getwd(), ...)
    if (file.exists(candidate)) path <- candidate
  }
  if (!nzchar(path) || !file.exists(path)) {
    nepse_stop("Package file not found: %s", file.path(...))
  }
  path
}

nepse_read_csv <- function(path, required = NULL) {
  out <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character()
  )
  if (!is.null(required)) {
    missing <- setdiff(required, names(out))
    if (length(missing)) {
      nepse_stop(
        "`%s` is missing required column(s): %s",
        basename(path), paste(missing, collapse = ", ")
      )
    }
  }
  tibble::as_tibble(out)
}

nepse_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(x, tmp, row.names = FALSE, na = "")
  if (!file.rename(tmp, path)) {
    if (!file.copy(tmp, path, overwrite = TRUE)) {
      nepse_stop("Failed to write `%s`.", path)
    }
    unlink(tmp)
  }
  invisible(path)
}

nepse_file_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

nepse_bind <- function(xs) {
  xs <- Filter(function(x) !is.null(x) && nrow(x), xs)
  if (!length(xs)) return(tibble::tibble())
  dplyr::bind_rows(xs)
}
