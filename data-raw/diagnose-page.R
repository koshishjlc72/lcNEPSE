# Diagnostic helper: inspect the ShareSansar ajax date-filter response.
# Run by diagnose-page.yml only.

base <- "https://www.sharesansar.com/today-share-price"
ajax <- "https://www.sharesansar.com/ajaxtodayshareprice"
jar <- tempfile(fileext = ".cookies")

handle <- curl::new_handle()
curl::handle_setopt(
  handle,
  useragent = "lcNEPSE diagnostic (+https://github.com/koshishjlc72/lcNEPSE)",
  followlocation = TRUE, timeout = 60,
  cookiefile = jar, cookiejar = jar
)

page <- rawToChar(curl::curl_fetch_memory(base, handle = handle)$content)
Encoding(page) <- "UTF-8"
doc <- xml2::read_html(page)
form <- xml2::xml_find_first(doc, "//form[@id='frm_todayshareprice']")
token <- xml2::xml_attr(
  xml2::xml_find_first(form, ".//input[@name='_token']"), "value"
)
cat("token length:", nchar(token), "\n")

fields <- list(
  `_token` = token,
  sector = "all_sec",
  date = "2026-10-29"
)
body <- paste(
  paste0(curl::curl_escape(names(fields)), "=", curl::curl_escape(unlist(fields))),
  collapse = "&"
)

for (label in c("with headers", "without headers")) {
  h <- handle
  if (identical(label, "with headers")) {
    curl::handle_setopt(
      h, httpheader = c("X-Requested-With: XMLHttpRequest", "Accept: text/html, */*")
    )
  }
  curl::handle_setopt(h, post = TRUE, postfields = body, referer = base)
  res <- curl::curl_fetch_memory(ajax, handle = h)
  txt <- rawToChar(res$content)
  Encoding(txt) <- "UTF-8"
  cat("\n###", label, "status:", res$status_code, "bytes:", nchar(txt), "\n")
  cat(substr(txt, 1, 1500), "\n")
  doc2 <- try(xml2::read_html(res$content), silent = TRUE)
  if (!inherits(doc2, "try-error")) {
    cat("tables:", length(xml2::xml_find_all(doc2, "//table")), "\n")
    cat("mentions Symbol:", grepl("Symbol", txt, fixed = TRUE), "\n")
    cat("mentions As of:", grepl("As of", txt, fixed = TRUE), "\n")
  }
  curl::handle_setopt(h, post = FALSE, postfields = NULL, httpheader = NULL)
}
cat("\nDIAGNOSE_OK\n")
