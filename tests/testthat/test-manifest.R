test_that("the archive manifest is validated", {
  arc <- local_archive()
  expect_silent(nepse_validate_manifest(arc$manifest))
  expect_equal(nrow(nepse_index(force = TRUE)$manifest), nrow(arc$manifest))
})

test_that("structural manifest problems are rejected", {
  arc <- local_archive()

  bad_hash <- arc$manifest
  bad_hash$sha256[1L] <- "not-a-hash"
  expect_error(nepse_validate_manifest(bad_hash), "SHA-256")

  duplicate <- arc$manifest
  duplicate$path[2L] <- duplicate$path[1L]
  expect_error(nepse_validate_manifest(duplicate), "duplicate paths")

  unsafe <- arc$manifest
  unsafe$path[1L] <- "../evil.csv"
  expect_error(nepse_validate_manifest(unsafe), "unsafe asset path")

  wrong_parser <- arc$manifest
  wrong_parser$parser_version[1L] <- "prices-0.0.1"
  expect_error(nepse_validate_manifest(wrong_parser), "parser version")

  foreign <- arc$manifest
  foreign$source_url[1L] <- "https://example.com/page"
  expect_error(nepse_validate_manifest(foreign), "outside sharesansar")

  missing_column <- arc$manifest
  missing_column$rows <- NULL
  expect_error(nepse_validate_manifest(missing_column), "missing column")

  dated_without_dates <- arc$manifest
  dated_without_dates$first_date[1L] <- ""
  expect_error(nepse_validate_manifest(dated_without_dates), "first and last dates")

  two_current <- arc$manifest
  idx <- which(two_current$dataset_id == "prices" & two_current$current == "true")[1:2]
  two_current$partition[idx[2L]] <- two_current$partition[idx[1L]]
  expect_error(nepse_validate_manifest(two_current), "more than one current file")
})

test_that("a missing current symbol master is rejected", {
  arc <- local_archive()
  broken <- arc$manifest
  broken$current[broken$dataset_id == "symbols"] <- "false"
  expect_error(nepse_validate_manifest(broken), "current symbol master")
})

test_that("an empty or unreadable manifest fails clearly", {
  expect_error(nepse_validate_manifest(data.frame()), "missing column")
  expect_error(nepse_validate_manifest(iris), "missing column")
})

test_that("an archive with no rows for a range reports it", {
  local_archive()
  expect_error(nepse_prices(start = "2030-01-01"), "no `prices` data")
})
