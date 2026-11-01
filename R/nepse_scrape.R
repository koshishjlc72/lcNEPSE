# ShareSansar scraping -----------------------------------------------------

nepse_prices_url <- function() {
  "https://www.sharesansar.com/today-share-price"
}

# The date filter is JavaScript-driven: the page posts the filter form here and
# replaces #todayshareprice_data with the returned HTML fragment.
nepse_ajax_prices_url <- function() {
  "https://www.sharesansar.com/ajaxtodayshareprice"
}

nepse_indices_url <- function() {
  "https://www.sharesansar.com/"
}

nepse_normalize_label <- function(x) {
  x <- tolower(x)
  x <- gsub("%", " percent ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", " ", x)
  stringr::str_squish(x)
}

nepse_find_table <- function(doc, labels) {
  expected <- vapply(labels, nepse_normalize_label, character(1), USE.NAMES = FALSE)
  tables <- xml2::xml_find_all(doc, "//table")
  for (table in tables) {
    header <- nepse_header_cells(table)
    if (!length(header)) next
    if (all(expected %in% header)) return(table)
  }
  NULL
}

nepse_header_cells <- function(table) {
  rows <- xml2::xml_find_all(table, ".//tr")
  for (row in rows) {
    cells <- xml2::xml_find_all(row, "./th|./td")
    if (length(cells) >= 5L) {
      return(nepse_normalize_label(xml2::xml_text(cells)))
    }
  }
  character()
}

nepse_as_of_date <- function(node) {
  # The page nests the date, e.g. <h5>As of : <span>2026-10-30</span></h5>, so
  # read the nearest preceding element's full text rather than one text node.
  hit <- xml2::xml_find_first(node, "preceding::*[contains(., 'As of')][1]")
  if (inherits(hit, "xml_missing")) {
    hit <- xml2::xml_find_first(node, "preceding::text()[contains(., 'As of')][1]")
  }
  if (inherits(hit, "xml_missing")) return(as.Date(NA))
  text <- xml2::xml_text(hit)
  m <- regmatches(text, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", text))
  if (!length(m) || !nzchar(m)) return(as.Date(NA))
  as.Date(m)
}

#' Scrape a ShareSansar share-price snapshot
#'
#' Fetches the current ShareSansar price table, or one historical trading day
#' through the page's date filter, verifies the published columns, and returns
#' the standardized rows for that single day. The scheduled archive workflow
#' uses this function; most users should call [nepse_prices()] instead.
#'
#' @param date Optional `Date` or ISO date. `NULL` reads the current page.
#' @return A tibble of one trading day's prices with `source_url` and
#'   `retrieved_at`.
#' @export
nepse_scrape_prices <- function(date = NULL) {
  if (!is.null(date)) date <- nepse_parse_date_arg(date, "date")
  url <- nepse_prices_url()
  jar <- tempfile(fileext = ".cookies")
  on.exit(unlink(jar), add = TRUE)
  handle <- nepse_new_handle(cookiejar = jar)
  page <- nepse_http_get_text(url, handle = handle)
  doc <- xml2::read_html(page)

  if (!is.null(date)) {
    posted <- nepse_post_date_filter(doc, url, date, handle)
    page <- posted$text
    doc <- xml2::read_html(page)
  }

  table <- nepse_find_table(doc, nepse_prices_source_labels())
  if (is.null(table)) {
    snippet <- substr(gsub("\\s+", " ", page), 1L, 400L)
    nepse_stop(
      "ShareSansar did not return the expected share-price table. Response begins: %s",
      snippet
    )
  }
  nepse_assert_single_page(table)
  as_of <- nepse_as_of_date(table)
  if (is.na(as_of)) {
    if (is.null(date)) {
      nepse_stop(
        "Could not read the 'As of' trading date from the ShareSansar page; refusing to label undated rows."
      )
    }
    if (!grepl(format(date, "%Y-%m-%d"), page, fixed = TRUE)) {
      nepse_stop(
        "ShareSansar did not confirm the requested date %s; refusing to store an undated snapshot.",
        format(date, "%Y-%m-%d")
      )
    }
    as_of <- date
  }
  if (!is.null(date) && !identical(as_of, date)) {
    nepse_stop(
      "ShareSansar returned data for %s when %s was requested.",
      format(as_of, "%Y-%m-%d"), format(date, "%Y-%m-%d")
    )
  }

  out <- nepse_parse_price_table(table)
  out$trading_date <- as_of
  out$source_url <- url
  out$retrieved_at <- nepse_now_utc()
  out <- out[, c(nepse_prices_columns(), "source_url", "retrieved_at"), drop = FALSE]
  if (nrow(out) < 20L) {
    nepse_stop(
      "ShareSansar returned only %d symbol row(s) for %s; refusing a partial snapshot.",
      nrow(out), format(as_of, "%Y-%m-%d")
    )
  }
  out
}

nepse_date_filter_fields <- function(doc, date, url) {
  form <- xml2::xml_find_first(doc, "//form[@id='frm_todayshareprice']")
  if (inherits(form, "xml_missing")) {
    form <- xml2::xml_find_first(doc, "//form[.//input[@id='fromdate']]")
  }
  if (inherits(form, "xml_missing")) {
    nepse_stop(
      "ShareSansar has no date filter form; historical backfill is unavailable. The page layout may have changed."
    )
  }
  inputs <- xml2::xml_find_all(form, ".//input[@name]")
  fields <- list()
  for (input in inputs) {
    type <- tolower(xml2::xml_attr(input, "type"))
    if (is.na(type)) type <- "text"
    if (type %in% c("submit", "button", "reset", "image", "file")) next
    name <- xml2::xml_attr(input, "name")
    value <- xml2::xml_attr(input, "value")
    fields[[name]] <- if (is.na(value)) "" else value
  }
  selects <- xml2::xml_find_all(form, ".//select[@name]")
  for (select in selects) {
    name <- xml2::xml_attr(select, "name")
    option <- xml2::xml_find_first(select, ".//option[@selected]")
    if (inherits(option, "xml_missing")) option <- xml2::xml_find_first(select, ".//option[1]")
    value <- if (inherits(option, "xml_missing")) "" else xml2::xml_attr(option, "value")
    fields[[name]] <- if (is.na(value)) "" else value
  }
  date_input <- xml2::xml_find_first(form, ".//input[@id='fromdate']")
  if (inherits(date_input, "xml_missing")) {
    nepse_stop("ShareSansar's date filter has no date input; cannot request a historical date.")
  }
  date_name <- xml2::xml_attr(date_input, "name")
  if (is.na(date_name) || !nzchar(date_name)) {
    nepse_stop("ShareSansar's date filter has no input name; cannot request a historical date.")
  }
  format_hint <- xml2::xml_attr(date_input, "data-date-format")
  pattern <- if (!is.na(format_hint) && grepl("MM/DD/YYYY", format_hint)) {
    "%m/%d/%Y"
  } else {
    "%Y-%m-%d"
  }
  fields[[date_name]] <- format(date, pattern)
  list(action = nepse_ajax_prices_url(), fields = fields)
}

nepse_post_date_filter <- function(doc, url, date, handle) {
  spec <- nepse_date_filter_fields(doc, date, url)
  res <- nepse_http_post_form(
    spec$action, spec$fields, handle = handle, referer = url,
    headers = c("X-Requested-With: XMLHttpRequest", "Accept: text/html, */*")
  )
  txt <- rawToChar(res$content)
  Encoding(txt) <- "UTF-8"
  list(text = txt, status = res$status_code)
}

nepse_assert_single_page <- function(table) {
  wrapper <- xml2::xml_find_first(table, "ancestor::div[1]")
  scope <- if (inherits(wrapper, "xml_missing")) table else wrapper
  next_link <- xml2::xml_find_first(scope, ".//a[normalize-space(.)='Next']")
  if (!inherits(next_link, "xml_missing")) {
    nepse_stop(
      "ShareSansar paginated the price table. Refusing to archive a partial page; update the scraper to follow pagination."
    )
  }
  invisible(TRUE)
}

nepse_prices_source_labels <- function() {
  c(
    symbol = "Symbol", conf = "Conf.", open = "Open", high = "High",
    low = "Low", close = "Close", ltp = "LTP",
    close_ltp_diff = "Close - LTP", close_ltp_pct = "Close - LTP %",
    vwap = "VWAP", volume = "Vol", prev_close = "Prev. Close",
    turnover = "Turnover", transactions = "Trans.", diff = "Diff",
    range = "Range", diff_pct = "Diff %", range_pct = "Range %",
    vwap_pct = "VWAP %", avg_120d = "120 Days", avg_180d = "180 Days",
    high_52w = "52 Weeks High", low_52w = "52 Weeks Low"
  )
}

nepse_indices_source_labels <- function() {
  c(
    index_name = "Sub-Indices", open = "Open", high = "High", low = "Low",
    close = "Close", turnover = "Turnover", point_change = "Point",
    pct_change = "% Change", high_52w = "52 Week High", low_52w = "52 Week Low"
  )
}

nepse_column_positions <- function(header, labels) {
  normalized <- vapply(labels, nepse_normalize_label, character(1), USE.NAMES = FALSE)
  pos <- match(normalized, header)
  if (anyNA(pos)) {
    nepse_stop(
      "ShareSansar table is missing expected column(s): %s. Found: %s",
      paste(names(labels)[is.na(pos)], collapse = ", "),
      paste(header, collapse = " | ")
    )
  }
  stats::setNames(pos, names(labels))
}

nepse_row_cells <- function(row, ncol, index) {
  cells <- xml2::xml_find_all(row, "./td")
  if (length(cells) != ncol) {
    nepse_stop(
      "ShareSansar row %d has %d cells but the header has %d; column alignment is unsafe.",
      index, length(cells), ncol
    )
  }
  cells
}

nepse_parse_price_table <- function(table) {
  header <- nepse_header_cells(table)
  pos <- nepse_column_positions(header, nepse_prices_source_labels())
  rows <- xml2::xml_find_all(table, ".//tbody/tr")
  if (!length(rows)) {
    rows <- xml2::xml_find_all(table, ".//tr")
    keep <- vapply(
      rows,
      function(r) length(xml2::xml_find_all(r, "./td")) == length(header),
      logical(1)
    )
    rows <- rows[keep]
  }
  if (!length(rows)) nepse_stop("ShareSansar price table has no data rows.")

  cells <- lapply(seq_along(rows), function(i) {
    nepse_row_cells(rows[[i]], length(header), i)
  })
  cell_text <- function(field) {
    vapply(cells, function(cs) nepse_clean_text(xml2::xml_text(cs[[pos[[field]]]])), character(1))
  }

  symbol_cells <- lapply(cells, function(cs) cs[[pos[["symbol"]]]])
  symbol <- vapply(symbol_cells, function(cell) {
    link <- xml2::xml_find_first(cell, ".//a")
    if (inherits(link, "xml_missing")) nepse_clean_text(xml2::xml_text(cell)) else nepse_clean_text(xml2::xml_text(link))
  }, character(1))
  company <- vapply(symbol_cells, function(cell) {
    link <- xml2::xml_find_first(cell, ".//a")
    if (inherits(link, "xml_missing")) NA_character_ else nepse_clean_text(xml2::xml_attr(link, "title"))
  }, character(1))
  company[is.na(company)] <- NA_character_

  out <- tibble::tibble(
    symbol = symbol,
    company_name = company,
    conf = nepse_parse_number(cell_text("conf"), "conf"),
    open = nepse_parse_number(cell_text("open"), "open"),
    high = nepse_parse_number(cell_text("high"), "high"),
    low = nepse_parse_number(cell_text("low"), "low"),
    close = nepse_parse_number(cell_text("close"), "close"),
    ltp = nepse_parse_number(cell_text("ltp"), "ltp"),
    close_ltp_diff = nepse_parse_number(cell_text("close_ltp_diff"), "close_ltp_diff"),
    close_ltp_pct = nepse_parse_percent(cell_text("close_ltp_pct"), "close_ltp_pct"),
    vwap = nepse_parse_number(cell_text("vwap"), "vwap"),
    volume = nepse_parse_number(cell_text("volume"), "volume"),
    prev_close = nepse_parse_number(cell_text("prev_close"), "prev_close"),
    turnover = nepse_parse_number(cell_text("turnover"), "turnover"),
    transactions = nepse_parse_number(cell_text("transactions"), "transactions"),
    diff = nepse_parse_number(cell_text("diff"), "diff"),
    range = nepse_parse_number(cell_text("range"), "range"),
    diff_pct = nepse_parse_percent(cell_text("diff_pct"), "diff_pct"),
    range_pct = nepse_parse_percent(cell_text("range_pct"), "range_pct"),
    vwap_pct = nepse_parse_percent(cell_text("vwap_pct"), "vwap_pct"),
    avg_120d = nepse_parse_number(cell_text("avg_120d"), "avg_120d"),
    avg_180d = nepse_parse_number(cell_text("avg_180d"), "avg_180d"),
    high_52w = nepse_parse_number(cell_text("high_52w"), "high_52w"),
    low_52w = nepse_parse_number(cell_text("low_52w"), "low_52w")
  )
  if (any(!nzchar(out$symbol))) nepse_stop("ShareSansar price table contains an empty symbol.")
  if (anyDuplicated(out$symbol)) {
    nepse_stop("ShareSansar price table contains duplicate symbols.")
  }
  out
}

#' Scrape the ShareSansar sector sub-index snapshot
#'
#' Reads the published sub-index table from the ShareSansar front page. The
#' scheduled archive workflow uses this function; most users should call
#' [nepse_indices()] instead.
#'
#' @return A tibble of one trading day's sub-index levels with `source_url` and
#'   `retrieved_at`.
#' @export
nepse_scrape_indices <- function() {
  url <- nepse_indices_url()
  page <- nepse_http_get_text(url)
  doc <- xml2::read_html(page)
  table <- nepse_find_table(doc, nepse_indices_source_labels())
  if (is.null(table)) {
    nepse_stop(
      "ShareSansar did not return the expected sub-index table. The page layout may have changed."
    )
  }
  as_of <- nepse_as_of_date(table)
  if (is.na(as_of)) {
    nepse_stop(
      "Could not read the 'As of' trading date for the ShareSansar sub-indices; refusing to label undated rows."
    )
  }
  out <- nepse_parse_index_table(table)
  out$trading_date <- as_of
  out$source_url <- url
  out$retrieved_at <- nepse_now_utc()
  out <- out[, c(nepse_indices_columns(), "source_url", "retrieved_at"), drop = FALSE]
  if (nrow(out) < 5L) {
    nepse_stop(
      "ShareSansar returned only %d sub-index row(s) for %s; refusing a partial snapshot.",
      nrow(out), format(as_of, "%Y-%m-%d")
    )
  }
  out
}

nepse_parse_index_table <- function(table) {
  header <- nepse_header_cells(table)
  pos <- nepse_column_positions(header, nepse_indices_source_labels())
  rows <- xml2::xml_find_all(table, ".//tbody/tr")
  if (!length(rows)) {
    rows <- xml2::xml_find_all(table, ".//tr")
    rows <- rows[vapply(rows, function(r) length(xml2::xml_find_all(r, "./td")) == length(header), logical(1))]
  }
  if (!length(rows)) nepse_stop("ShareSansar sub-index table has no data rows.")
  cells <- lapply(seq_along(rows), function(i) nepse_row_cells(rows[[i]], length(header), i))
  cell_text <- function(field) {
    vapply(cells, function(cs) nepse_clean_text(xml2::xml_text(cs[[pos[[field]]]])), character(1))
  }
  out <- tibble::tibble(
    index_name = cell_text("index_name"),
    open = nepse_parse_number(cell_text("open"), "open"),
    high = nepse_parse_number(cell_text("high"), "high"),
    low = nepse_parse_number(cell_text("low"), "low"),
    close = nepse_parse_number(cell_text("close"), "close"),
    turnover = nepse_parse_number(cell_text("turnover"), "turnover"),
    point_change = nepse_parse_number(cell_text("point_change"), "point_change"),
    pct_change = nepse_parse_percent(cell_text("pct_change"), "pct_change"),
    high_52w = nepse_parse_number(cell_text("high_52w"), "high_52w"),
    low_52w = nepse_parse_number(cell_text("low_52w"), "low_52w")
  )
  if (any(!nzchar(out$index_name))) nepse_stop("ShareSansar sub-index table has an empty name.")
  if (anyDuplicated(out$index_name)) nepse_stop("ShareSansar sub-index table has duplicate names.")
  out
}
