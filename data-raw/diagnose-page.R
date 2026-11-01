# Diagnostic helper: probes how ShareSansar's date filter is actually driven.
# Run by diagnose-page.yml only.

dir.create("diagnose", showWarnings = FALSE)
base <- "https://www.sharesansar.com/today-share-price"

fetch <- function(label, url, post = FALSE, fields = NULL, ajax = FALSE) {
  handle <- curl::new_handle()
  curl::handle_setopt(
    handle,
    useragent = "lcNEPSE diagnostic (+https://github.com/koshishjlc72/lcNEPSE)",
    followlocation = TRUE, timeout = 60
  )
  if (ajax) curl::handle_setopt(handle, httpheader = "X-Requested-With: XMLHttpRequest")
  if (post) {
    body <- paste(
      paste0(curl::curl_escape(names(fields)), "=", curl::curl_escape(unlist(fields))),
      collapse = "&"
    )
    curl::handle_setopt(handle, post = TRUE, postfields = body)
  }
  res <- try(curl::curl_fetch_memory(url, handle = handle), silent = TRUE)
  cat("\n###", label, "\n")
  if (inherits(res, "try-error")) {
    cat("error:", conditionMessage(attr(res, "condition")), "\n")
    return(invisible(NULL))
  }
  cat("status:", res$status_code, "bytes:", length(res$content), "\n")
  txt <- rawToChar(res$content)
  Encoding(txt) <- "UTF-8"
  writeLines(substr(txt, 1, 300), stdout())
  doc <- try(xml2::read_html(res$content), silent = TRUE)
  if (inherits(doc, "try-error")) return(invisible(NULL))
  table <- NULL
  tables <- xml2::xml_find_all(doc, "//table")
  for (t in tables) {
    h <- xml2::xml_find_all(t, ".//th")
    if ("Symbol" %in% xml2::xml_text(h) && "Open" %in% xml2::xml_text(h)) {
      table <- t
      break
    }
  }
  if (is.null(table)) {
    cat("price table: none\n")
    return(invisible(NULL))
  }
  headers <- xml2::xml_find_all(table, ".//th")
  cat("price table rows:", length(xml2::xml_find_all(table, ".//tbody/tr")), "\n")
  hit <- xml2::xml_find_first(table, "preceding::*[contains(., 'As of')][1]")
  cat("as of:", if (inherits(hit, "xml_missing")) "none" else gsub("\\s+", " ", xml2::xml_text(hit)), "\n")
  invisible(NULL)
}

fetch("GET no date", base)
fetch("GET ?date=2026-10-29", paste0(base, "?date=2026-10-29"))
fetch("GET ?date=2026-10-30", paste0(base, "?date=2026-10-30"))
fetch("GET /2026-10-29", paste0(base, "/2026-10-29"))
fetch("POST plain", base, post = TRUE, fields = list(date = "2026-10-29", sector = ""))
fetch("POST ajax", base, post = TRUE, fields = list(date = "2026-10-29", sector = ""), ajax = TRUE)

cat("\nDIAGNOSE_OK\n")
