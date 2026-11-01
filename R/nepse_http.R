# HTTP helpers ------------------------------------------------------------
# Both the archive client and the ShareSansar scrapers go through these
# wrappers so that user agent, timeout, and failure behaviour stay consistent.

nepse_user_agent <- function() {
  paste0(
    "lcNEPSE/", utils::packageVersion("lcNEPSE"),
    " (+https://github.com/koshishjlc72/lcNEPSE)"
  )
}

nepse_new_handle <- function(cookiejar = NULL, referer = NULL) {
  handle <- curl::new_handle()
  curl::handle_setopt(
    handle,
    useragent = nepse_user_agent(),
    timeout = 60,
    connecttimeout = 30,
    followlocation = TRUE,
    accept_encoding = "gzip, deflate"
  )
  if (!is.null(referer)) curl::handle_setopt(handle, referer = referer)
  if (!is.null(cookiejar)) {
    curl::handle_setopt(handle, cookiefile = cookiejar, cookiejar = cookiejar)
  }
  handle
}

nepse_http_failure <- function(res, url) {
  nepse_stop(
    "HTTP request failed with status %s for %s.", res$status_code, url
  )
}

nepse_http_get_raw <- function(url, handle = NULL, attempts = 3L) {
  last <- NULL
  for (attempt in seq_len(attempts)) {
    if (!is.null(handle)) {
      res <- tryCatch(
        curl::curl_fetch_memory(url, handle = handle),
        error = function(e) e
      )
    } else {
      res <- tryCatch(
        curl::curl_fetch_memory(url, handle = nepse_new_handle()),
        error = function(e) e
      )
    }
    if (!inherits(res, "error") && res$status_code == 200L) return(res)
    last <- if (inherits(res, "error")) res else res$status_code
    if (attempt < attempts) Sys.sleep(min(2^attempt, 8))
  }
  if (inherits(last, "error")) {
    nepse_stop("Could not reach %s: %s", url, conditionMessage(last))
  }
  nepse_stop("Could not reach %s after %d attempts (HTTP %s).", url, attempts, last)
}

nepse_http_get_text <- function(url, handle = NULL, attempts = 3L) {
  res <- nepse_http_get_raw(url, handle = handle, attempts = attempts)
  text <- rawToChar(res$content)
  Encoding(text) <- "UTF-8"
  text
}

nepse_http_get_file <- function(url, dest, attempts = 3L) {
  res <- nepse_http_get_raw(url, attempts = attempts)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  writeBin(res$content, dest)
  dest
}

nepse_http_post_form <- function(url, fields, handle, referer = NULL,
                                 attempts = 2L) {
  body <- paste(
    paste0(
      curl::curl_escape(names(fields)),
      "=",
      curl::curl_escape(as.character(fields))
    ),
    collapse = "&"
  )
  if (!is.null(referer)) curl::handle_setopt(handle, referer = referer)
  curl::handle_setopt(handle, post = TRUE, postfields = body)
  on.exit(curl::handle_setopt(handle, post = FALSE, postfields = NULL), add = TRUE)
  nepse_http_get_raw(url, handle = handle, attempts = attempts)
}
