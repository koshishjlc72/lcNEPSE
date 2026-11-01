# Diagnostic helper: captures the live ShareSansar page from a GitHub runner and
# prints the structures the scraper depends on. Run by diagnose-page.yml only.

dir.create("diagnose", showWarnings = FALSE)
url <- "https://www.sharesansar.com/today-share-price"
handle <- curl::new_handle()
curl::handle_setopt(
  handle, useragent = "lcNEPSE diagnostic (+https://github.com/koshishjlc72/lcNEPSE)",
  followlocation = TRUE, timeout = 60
)
res <- curl::curl_fetch_memory(url, handle = handle)
cat("HTTP", res$status_code, "bytes", length(res$content), "\n")
writeBin(res$content, "diagnose/today.html")

doc <- xml2::read_html(res$content)
text <- xml2::xml_text(doc)

cat("\n=== 'As of' occurrences ===\n")
hits <- gregexpr("As of", text, ignore.case = TRUE)[[1]]
if (hits[[1L]] != -1L) {
  for (i in utils::head(hits, 12L)) {
    cat("[", i, "]", gsub("\\s+", " ", substr(text, max(1L, i - 140L), i + 140L)), "\n---\n", sep = "")
  }
} else {
  cat("none found in document text\n")
}

tables <- xml2::xml_find_all(doc, "//table")
cat("\n=== tables ===\n")
cat("count:", length(tables), "\n")
target <- NULL
for (i in seq_along(tables)) {
  th <- xml2::xml_text(xml2::xml_find_all(tables[[i]], ".//th"))
  cat(i, ":", paste(utils::head(th, 6), collapse = " | "), "\n")
  if (is.null(target) && "Symbol" %in% th && "Open" %in% th) target <- tables[[i]]
}
if (is.null(target)) {
  cat("price table not found\n")
  quit(status = 1)
}

cat("\n=== preceding elements of the price table ===\n")
nodes <- xml2::xml_find_all(target, "preceding::*")
for (node in utils::tail(nodes, 10L)) {
  cat(
    "name:", xml2::xml_name(node),
    "| class:", xml2::xml_attr(node, "class"),
    "| text:", gsub("\\s+", " ", substr(xml2::xml_text(node), 1L, 120L)), "\n"
  )
}

cat("\n=== parent structure ===\n")
parent <- xml2::xml_parent(target)
cat(as.character(parent)[1L], "\n")

cat("\n=== ancestors ===\n")
anc <- xml2::xml_find_all(target, "ancestor::*")
for (node in utils::tail(anc, 6L)) {
  cat(
    "name:", xml2::xml_name(node),
    "| class:", xml2::xml_attr(node, "class"),
    "| id:", xml2::xml_attr(node, "id"), "\n"
  )
}

cat("\n=== sibling text before table ===\n")
sib <- xml2::xml_find_all(target, "preceding-sibling::*")
for (node in utils::tail(sib, 5L)) {
  cat("name:", xml2::xml_name(node), "| text:",
      gsub("\\s+", " ", substr(xml2::xml_text(node), 1L, 200L)), "\n")
}
cat("\nDIAGNOSE_OK\n")
