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
  expect_equal(spec$action, "https://www.sharesansar.com/today-share-price")
  expect_equal(spec$fields[["_token"]], "token-abc123")
  expect_equal(spec$fields[["sector"]], "")
  expect_equal(spec$fields[["fromdate"]], "10/29/2026")
  expect_false("submit" %in% names(spec$fields))
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

test_that("a missing table is reported clearly", {
  doc <- xml2::read_html("<html><body><table><tr><th>Date</th><th>News</th></tr></table></body></html>")
  expect_null(nepse_find_table(doc, nepse_prices_source_labels()))
})
