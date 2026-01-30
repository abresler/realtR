# Mortgage rates function tests

library(testthat)
library(realtR)

context("Mortgage rates from FRED")

test_that("mortgage_rates_fred returns valid tibble", {
  skip_if_offline()

 result <- mortgage_rates_fred(rate_types = "30yr", start_date = "2024-01-01")

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c("dateData", "typeRate", "value") %in% names(result)))
  expect_true(all(result$value > 0 & result$value < 1))  # Rates as decimals
})

test_that("mortgage_rates_fred handles multiple rate types", {
  skip_if_offline()

  result <- mortgage_rates_fred(
    rate_types = c("30yr", "15yr"),
    start_date = "2024-01-01"
  )

  expect_s3_class(result, "tbl_df")
  expect_true("pct30YearFixed" %in% result$typeRate)
  expect_true("pct15YearFixed" %in% result$typeRate)
})

test_that("mortgage_rates_fred wide format works", {
  skip_if_offline()

  result <- mortgage_rates_fred(
    rate_types = "30yr",
    start_date = "2024-01-01",
    return_wide = TRUE
  )

  expect_s3_class(result, "tbl_df")
  expect_true("pct30YearFixed" %in% names(result))
  expect_true("dateData" %in% names(result))
})

test_that("deprecated mortgage_rates shows warning", {
  expect_warning(
    mortgage_rates(),
    "deprecated"
  )
})
