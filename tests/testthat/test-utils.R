test_that("numbers are cleaned strictly", {
  expect_equal(nepse_parse_number(c("1,234.50", "-", " 7 "), "x"), c(1234.5, NA, 7))
  expect_equal(nepse_parse_percent(c("-0.56", "1.52%", ""), "x"), c(-0.56, 1.52, NA))
  expect_error(nepse_parse_number(c("12", "oops"), "close"), "non-numeric")
})

test_that("date arguments are parsed and rejected", {
  expect_equal(nepse_parse_date_arg("2026-10-30", "start"), as.Date("2026-10-30"))
  expect_equal(nepse_parse_date_arg(as.Date("2026-10-30"), "start"), as.Date("2026-10-30"))
  expect_null(nepse_parse_date_arg(NULL, "start"))
  expect_error(nepse_parse_date_arg("30/10/2026", "start"), "ISO date")
  expect_error(nepse_parse_date_arg(c("2026-10-30", "2026-10-31"), "start"), "one non-empty")
})

test_that("month keys and range filters behave", {
  expect_equal(nepse_month_key(as.Date("2026-02-03")), "2026-02")
  x <- as.Date(c("2026-01-01", "2026-02-01", "2026-03-01"))
  expect_equal(nepse_in_range(x, as.Date("2026-01-15"), as.Date("2026-02-15")),
               c(FALSE, TRUE, FALSE))
  expect_true(all(nepse_in_range(x)))
})

test_that("only the expected archive hosts are trusted", {
  expect_silent(nepse_assert_trusted_url(
    "https://raw.githubusercontent.com/koshishjlc72/lcNEPSE/data-archive"
  ))
  expect_error(nepse_assert_trusted_url("http://raw.githubusercontent.com/x"), "HTTPS")
  expect_error(nepse_assert_trusted_url("https://example.com/data.csv"), "raw.githubusercontent.com")
})

test_that("scalar assertions fail loudly", {
  expect_error(nepse_assert_scalar_string(c("a", "b"), "x"), "one non-empty")
  expect_error(nepse_assert_scalar_string("", "x"), "one non-empty")
  expect_silent(nepse_assert_scalar_string("a", "x"))
  expect_error(nepse_assert_flag(NA, "x"), "TRUE")
})
