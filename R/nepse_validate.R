nepse_validate_prices <- function(data, error = TRUE, min_rows = 1L) {
  problems <- character()
  required <- nepse_prices_columns()
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    problems <- c(problems, paste("Missing columns:", paste(missing, collapse = ", ")))
  } else {
    if (!inherits(data$trading_date, "Date")) {
      problems <- c(problems, "`trading_date` is not a Date vector")
    } else if (anyNA(data$trading_date)) {
      problems <- c(problems, "`trading_date` contains missing dates")
    }
    if (!is.character(data$symbol) || anyNA(data$symbol) || any(!nzchar(data$symbol))) {
      problems <- c(problems, "`symbol` contains missing values")
    }
    if (!is.numeric(data$close)) {
      problems <- c(problems, "`close` is not numeric")
    } else if (anyNA(data$close)) {
      problems <- c(problems, "`close` contains missing values")
    }
    key <- paste(data$trading_date, data$symbol, sep = "\r")
    if (anyDuplicated(key)) {
      problems <- c(problems, "Duplicate symbol-date combinations")
    }
    numeric_fields <- setdiff(required, c("trading_date", "symbol", "company_name"))
    for (field in numeric_fields) {
      if (!is.numeric(data[[field]])) {
        problems <- c(problems, sprintf("`%s` is not numeric", field))
      }
    }
    non_negative <- c("open", "high", "low", "close", "vwap", "volume", "turnover", "transactions")
    for (field in non_negative) {
      value <- data[[field]]
      if (any(!is.na(value) & value < 0)) {
        problems <- c(problems, sprintf("`%s` contains negative values", field))
      }
    }
    ordered <- !is.na(data$high) & !is.na(data$low) & data$high < data$low
    if (any(ordered)) {
      problems <- c(
        problems,
        sprintf("`high` is below `low` in %d row(s); columns may be misaligned", sum(ordered))
      )
    }
    if (nrow(data) < min_rows) {
      problems <- c(problems, sprintf("Only %d row(s) present; at least %d expected", nrow(data), min_rows))
    }
  }
  result <- list(valid = !length(problems), problems = unique(problems), rows = nrow(data))
  if (length(problems) && isTRUE(error)) {
    nepse_stop("Price validation failed: %s", paste(unique(problems), collapse = "; "))
  }
  if (length(problems)) return(result)
  invisible(data)
}

nepse_validate_indices <- function(data, error = TRUE, min_rows = 1L) {
  problems <- character()
  required <- nepse_indices_columns()
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    problems <- c(problems, paste("Missing columns:", paste(missing, collapse = ", ")))
  } else {
    if (!inherits(data$trading_date, "Date")) {
      problems <- c(problems, "`trading_date` is not a Date vector")
    } else if (anyNA(data$trading_date)) {
      problems <- c(problems, "`trading_date` contains missing dates")
    }
    if (!is.character(data$index_name) || anyNA(data$index_name) || any(!nzchar(data$index_name))) {
      problems <- c(problems, "`index_name` contains missing values")
    }
    if (anyNA(data$close)) problems <- c(problems, "`close` contains missing values")
    key <- paste(data$trading_date, data$index_name, sep = "\r")
    if (anyDuplicated(key)) problems <- c(problems, "Duplicate index-date combinations")
    if (!is.logical(data$ohlc_consistent) || anyNA(data$ohlc_consistent)) {
      problems <- c(problems, "`ohlc_consistent` must be a non-missing logical vector")
    } else {
      expected <- is.na(data$high) | is.na(data$low) | data$high >= data$low
      if (any(data$ohlc_consistent != expected)) {
        problems <- c(problems, "`ohlc_consistent` does not match the published high/low values")
      }
      flagged <- !data$ohlc_consistent
      if (any(flagged & !nzchar(data$source_note))) {
        problems <- c(problems, "Inconsistent OHLC rows require a non-empty `source_note`")
      }
    }
    if (nrow(data) < min_rows) {
      problems <- c(problems, sprintf("Only %d row(s) present; at least %d expected", nrow(data), min_rows))
    }
  }
  result <- list(valid = !length(problems), problems = unique(problems), rows = nrow(data))
  if (length(problems) && isTRUE(error)) {
    nepse_stop("Index validation failed: %s", paste(unique(problems), collapse = "; "))
  }
  if (length(problems)) return(result)
  invisible(data)
}

#' Validate standardized lcNEPSE data
#'
#' Validation checks schema, types, duplicate observations, and the internal
#' consistency of the published snapshot. It does not reject unusual prices.
#'
#' @param data A tibble returned by [nepse_prices()] or [nepse_indices()].
#' @param error If `TRUE`, stop on failure; otherwise return a diagnostic list.
#' @return Invisibly, the input data when valid; otherwise a diagnostic list
#'   when `error = FALSE`.
#' @export
nepse_validate <- function(data, error = TRUE) {
  nepse_assert_flag(error, "error")
  if (!is.data.frame(data)) nepse_stop("`data` must be a data frame.")
  if ("index_name" %in% names(data)) {
    return(nepse_validate_indices(data, error = error))
  }
  if ("symbol" %in% names(data)) {
    return(nepse_validate_prices(data, error = error))
  }
  nepse_stop("`data` is neither a share-price nor an index table.")
}
