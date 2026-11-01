test_that("standardized prices validate", {
  data <- fixture_scraped_prices()
  expect_silent(nepse_validate(data))
  expect_true(is.data.frame(nepse_validate(data, error = FALSE)))
})

test_that("duplicate symbol-date rows are rejected", {
  data <- fixture_scraped_prices()
  data <- rbind(data, data[1L, , drop = FALSE])
  expect_error(nepse_validate(data), "Duplicate symbol-date")
})

test_that("negative values and inverted ranges are rejected", {
  data <- fixture_scraped_prices()

  bad_volume <- data
  bad_volume$volume[1L] <- -1
  expect_error(nepse_validate(bad_volume), "negative")

  bad_range <- data
  bad_range$high[1L] <- 1
  bad_range$low[1L] <- 2
  expect_error(nepse_validate(bad_range), "high.*below.*low")
})

test_that("missing close values are rejected", {
  data <- fixture_scraped_prices()
  data$close[1L] <- NA_real_
  expect_error(nepse_validate(data), "close")
})

test_that("a missing column is reported", {
  data <- fixture_scraped_prices()
  data$vwap <- NULL
  result <- nepse_validate(data, error = FALSE)
  expect_false(result$valid)
  expect_match(result$problems[[1L]], "Missing columns")
})

test_that("index tables validate", {
  local_archive()
  data <- nepse_indices(start = "2026-10-01", end = "2026-10-05")
  expect_silent(nepse_validate(data))
  data$close[1L] <- NA_real_
  expect_error(nepse_validate(data), "close")
})

test_that("unknown input types are rejected", {
  expect_error(nepse_validate(data.frame(a = 1)), "neither")
  expect_error(nepse_validate(1:3), "data frame")
})
