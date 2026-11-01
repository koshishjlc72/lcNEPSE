# Diagnostic helper: find how ShareSansar's date filter reaches the server.
# Run by diagnose-page.yml only.

get_text <- function(url) {
  handle <- curl::new_handle()
  curl::handle_setopt(
    handle,
    useragent = "lcNEPSE diagnostic (+https://github.com/koshishjlc72/lcNEPSE)",
    followlocation = TRUE, timeout = 60
  )
  res <- curl::curl_fetch_memory(url, handle = handle)
  txt <- rawToChar(res$content)
  Encoding(txt) <- "UTF-8"
  txt
}

urls <- c(
  "https://content.sharesansar.com/site/js/script_1.4.min.js",
  "https://content.sharesansar.com/site/js/main_2.0.min.js"
)
needles <- c("btn_todayshareprice_submit", "todayshareprice", "fromdate")

for (url in urls) {
  cat("\n###", url, "\n")
  txt <- try(get_text(url), silent = TRUE)
  if (inherits(txt, "try-error")) {
    cat("error\n")
    next
  }
  cat("chars:", nchar(txt), "\n")
  for (needle in needles) {
    hits <- gregexpr(needle, txt, fixed = TRUE)[[1L]]
    if (hits[[1L]] == -1L) next
    cat("\n---", needle, "hits:", length(hits), "---\n")
    for (i in utils::head(hits, 2L)) {
      cat(substr(txt, max(1L, i - 400L), i + 600L), "\n~~~\n", sep = "")
    }
  }
}
cat("\nDIAGNOSE_OK\n")
