test_that("the published price table is parsed without column drift", {
  doc <- xml2::read_html(fixture_path("prices-page.html"))
  table <- nepse_find_table(doc, nepse_prices_source_labels())
  expect_false(is.null(table))

  out <- nepse_parse_price_table(table)
  expect_equal(nrow(out), 7L)
  expect_equal(
    out$symbol,
    c("ACLBSL", "ADBL", "GBILD84/85", "TESTD91", "UNL", "CSY", "PLAIN")
  )
  expect_equal(out$close[out$symbol == "ACLBSL"], 854)
  expect_equal(out$open[out$symbol == "ACLBSL"], 863.5)
  expect_equal(out$high[out$symbol == "ACLBSL"], 863.9)
  expect_equal(out$low[out$symbol == "ACLBSL"], 851)
  expect_equal(out$volume[out$symbol == "ADBL"], 366409)
  expect_equal(out$turnover[out$symbol == "ADBL"], 127696988.5)
  expect_equal(out$transactions[out$symbol == "ADBL"], 605)
  expect_equal(out$diff_pct[out$symbol == "ACLBSL"], -0.56)
  expect_equal(out$high_52w[out$symbol == "ACLBSL"], 1095)
  expect_equal(out$company_name[out$symbol == "ACLBSL"],
               "Aarambha Chautari Laghubitta Bittiya Sanstha Limited")
  expect_true(is.na(out$company_name[out$symbol == "PLAIN"]))
})

test_that("dashes and blanks become missing values, not zeros", {
  doc <- xml2::read_html(fixture_path("prices-page.html"))
  out <- nepse_parse_price_table(nepse_find_table(doc, nepse_prices_source_labels()))
  row <- out[out$symbol == "TESTD91", , drop = FALSE]
  expect_true(is.na(row$open))
  expect_true(is.na(row$volume))
  expect_true(is.na(row$turnover))
  expect_equal(row$close, 1000)
})

test_that("the table date is read from the preceding 'As of' label", {
  doc <- xml2::read_html(fixture_path("prices-page.html"))
  table <- nepse_find_table(doc, nepse_prices_source_labels())
  expect_equal(nepse_as_of_date(table), as.Date("2026-10-30"))
})

test_that("the date is read when ShareSansar nests it in a span", {
  html <- paste(readLines(fixture_path("prices-page.html"), warn = FALSE), collapse = "\n")
  nested <- sub(
    "<h4>As of : 2026-10-30</h4>",
    '<h5>As of : <span class="text-org">2026-10-30</span></h5>',
    html, fixed = TRUE
  )
  expect_false(identical(nested, html))
  doc <- xml2::read_html(nested)
  table <- nepse_find_table(doc, nepse_prices_source_labels())
  expect_equal(nepse_as_of_date(table), as.Date("2026-10-30"))
})

test_that("an undated table reports a missing date", {
  html <- paste(readLines(fixture_path("prices-page.html"), warn = FALSE), collapse = "\n")
  undated <- sub("<h4>As of : 2026-10-30</h4>", "", html, fixed = TRUE)
  doc <- xml2::read_html(undated)
  table <- nepse_find_table(doc, nepse_prices_source_labels())
  expect_true(is.na(nepse_as_of_date(table)))
})

test_that("the historical fragment without 120/180-day columns parses", {
  doc <- xml2::read_html(fixture_path("prices-page.html"))
  table <- nepse_find_table(doc, nepse_prices_source_labels())
  header <- nepse_header_cells(table)
  drop <- which(header %in% c("120 days", "180 days"))
  expect_equal(length(drop), 2L)

  headers <- xml2::xml_find_all(table, ".//thead//th")
  for (i in sort(drop, decreasing = TRUE)) xml2::xml_remove(headers[[i]])
  for (row in xml2::xml_find_all(table, ".//tbody/tr")) {
    cells <- xml2::xml_find_all(row, "./td")
    for (i in sort(drop, decreasing = TRUE)) xml2::xml_remove(cells[[i]])
  }

  required <- nepse_prices_source_labels()
  optional <- nepse_prices_optional_labels()
  required <- required[!names(required) %in% names(optional)]
  doc2 <- xml2::read_html(as.character(doc))
  table2 <- nepse_find_table(doc2, required, optional = optional)
  expect_false(is.null(table2))

  out <- nepse_parse_price_table(table2, optional = optional)
  expect_equal(nrow(out), 7L)
  expect_true(all(is.na(out$avg_120d)))
  expect_true(all(is.na(out$avg_180d)))
  expect_equal(out$close[out$symbol == "ACLBSL"], 854)
})

test_that("a renamed column is a hard failure", {
  doc <- xml2::read_html(fixture_path("prices-page.html"))
  table <- nepse_find_table(doc, nepse_prices_source_labels())
  header <- nepse_header_cells(table)
  header[header == "open"] <- "opening"
  expect_error(
    nepse_column_positions(header, nepse_prices_source_labels()),
    "missing expected column"
  )
})

test_that("a missing required column still fails", {
  html <- paste(readLines(fixture_path("prices-page.html"), warn = FALSE), collapse = "\n")
  broken <- sub("<th>Open</th>", "<th>Opening</th>", html)
  doc <- xml2::read_html(broken)
  expect_null(nepse_find_table(doc, nepse_prices_source_labels()))
})

test_that("a ragged row is a hard failure", {
  html <- paste(readLines(fixture_path("prices-page.html"), warn = FALSE), collapse = "\n")
  broken <- sub(
    '<td>40.92</td><td>863.50</td>',
    '<td>40.92</td>',
    html, fixed = TRUE
  )
  doc <- xml2::read_html(broken)
  expect_error(
    nepse_parse_price_table(nepse_find_table(doc, nepse_prices_source_labels())),
    "column alignment is unsafe"
  )
})

test_that("the date filter form is read for historical requests", {
  doc <- xml2::read_html(fixture_path("prices-page.html"))
  spec <- nepse_date_filter_fields(doc, as.Date("2026-10-29"), nepse_prices_url())
  expect_equal(spec$action, nepse_ajax_prices_url())
  expect_equal(spec$fields[["_token"]], "token-abc123")
  expect_equal(spec$fields[["sector"]], "")
  expect_equal(spec$fields[["fromdate"]], "2026-10-29")
  expect_false("submit" %in% names(spec$fields))
})

test_that("the live date field name and date format are honoured", {
  html <- paste(
    '<html><body><form id="frm_todayshareprice" method="POST" action="">',
    '<input type="hidden" name="_token" value="tok">',
    '<select name="sector"><option value="all_sec">All Sector</option></select>',
    '<input type="text" name="date" id="fromdate" value="2026-10-30"',
    ' data-date-format="YYYY-MM-DD"></form></body></html>'
  )
  spec <- nepse_date_filter_fields(
    xml2::read_html(html), as.Date("2026-10-29"), nepse_prices_url()
  )
  expect_equal(spec$fields[["date"]], "2026-10-29")
  expect_equal(spec$fields[["sector"]], "all_sec")
  expect_equal(spec$fields[["_token"]], "tok")
  expect_equal(spec$action, nepse_ajax_prices_url())
})

test_that("the sub-index table is parsed", {
  doc <- xml2::read_html(fixture_path("indices-page.html"))
  table <- nepse_find_table(doc, nepse_indices_source_labels())
  expect_false(is.null(table))
  expect_equal(nepse_as_of_date(table), as.Date("2026-10-30"))

  header <- nepse_header_cells(table)
  pos <- nepse_column_positions(header, nepse_indices_source_labels())
  expect_equal(names(pos), names(nepse_indices_source_labels()))
})

test_that("the historical index JSON is parsed without invented fields", {
  text <- paste(readLines(fixture_path("index-history.json"), warn = FALSE), collapse = "\n")
  out <- nepse_parse_index_history_response(
    text = text,
    index_id = "12",
    index_name = "NEPSE Index",
    source_url = paste0(nepse_index_history_url(), "?index_id=12"),
    retrieved_at = "2026-11-04T07:00:00Z"
  )
  expect_equal(nrow(out), 2L)
  expect_equal(out$trading_date, as.Date(c("2026-11-03", "2026-11-02")))
  expect_equal(out$close, c(2664.13, 2657.51))
  expect_equal(out$point_change, c(6.62, -2.49))
  expect_equal(out$pct_change, c(0.24, -0.10))
  expect_true(all(is.na(out$high_52w)))
  expect_true(all(is.na(out$low_52w)))
  expect_true(all(out$ohlc_consistent))
  expect_true(all(out$source_note == ""))
  expect_silent(nepse_validate_indices(out))
})

test_that("published OHLC anomalies are preserved and explicitly flagged", {
  text <- paste(readLines(fixture_path("index-history.json"), warn = FALSE), collapse = "\n")
  anomalous <- sub('"high": "2668.40"', '"high": "2600.00"', text, fixed = TRUE)
  out <- nepse_parse_index_history_response(
    anomalous, "12", "NEPSE Index", nepse_index_history_url(),
    "2026-11-04T07:00:00Z"
  )
  expect_equal(out$high[[1L]], 2600)
  expect_equal(out$low[[1L]], 2635.2)
  expect_false(out$ohlc_consistent[[1L]])
  expect_match(out$source_note[[1L]], "original values preserved")
  expect_silent(nepse_validate_indices(out))
})

test_that("a partial historical index response is rejected", {
  text <- paste(readLines(fixture_path("index-history.json"), warn = FALSE), collapse = "\n")
  partial <- sub('"recordsFiltered": 2', '"recordsFiltered": 3', text, fixed = TRUE)
  expect_error(
    nepse_parse_index_history_response(
      partial, "12", "NEPSE Index", nepse_index_history_url(),
      "2026-11-04T07:00:00Z"
    ),
    "refusing a partial response"
  )
})

test_that("the index-history catalog requires the core series", {
  html <- paste0(
    '<select id="index">',
    '<option value="12">NEPSE Index</option>',
    '<option value="16">Sensitive Index</option>',
    '<option value="4">Float Index</option>',
    '<option value="1">Banking SubIndex</option>',
    '</select>'
  )
  out <- nepse_index_history_catalog(xml2::read_html(html))
  expect_equal(out$index_id, c("12", "16", "4", "1"))
  broken <- sub("Banking SubIndex", "Banking", html, fixed = TRUE)
  expect_error(
    nepse_index_history_catalog(xml2::read_html(broken)),
    "missing required series"
  )
})

test_that("the history request asks for every row in the selected range", {
  query <- nepse_index_history_query(
    "12", as.Date("2010-01-01"), as.Date("2026-11-04")
  )
  expect_match(query, "index_id=12", fixed = TRUE)
  expect_match(query, "from=2010-01-01", fixed = TRUE)
  expect_match(query, "to=2026-11-04", fixed = TRUE)
  expect_match(query, "length=-1", fixed = TRUE)
})

test_that("a missing table is reported clearly", {
  doc <- xml2::read_html("<html><body><table><tr><th>Date</th><th>News</th></tr></table></body></html>")
  expect_null(nepse_find_table(doc, nepse_prices_source_labels()))
})
