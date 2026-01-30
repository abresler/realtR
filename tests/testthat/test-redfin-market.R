# Redfin market data function tests

library(testthat)
library(realtR)

context("Redfin market data")

test_that("redfin_market_data returns valid tibble", {
  skip_if_offline()

  result <- redfin_market_data(start_date = "2024-10-01")

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c("periodEnd", "zipCode", "medianSalePrice") %in% names(result)))
})

test_that("redfin_market_data filters by ZIP code", {
  skip_if_offline()

  result <- redfin_market_data(
    zip_codes = c("90210", "10001"),
    start_date = "2024-01-01"
  )

  expect_s3_class(result, "tbl_df")
  if (nrow(result) > 0) {
    expect_true(all(result$zipCode %in% c("90210", "10001")))
  }
})

test_that("redfin_market_data filters by date range", {
  skip_if_offline()

  result <- redfin_market_data(
    start_date = "2024-06-01",
    end_date = "2024-12-31"
  )

  expect_s3_class(result, "tbl_df")
  if (nrow(result) > 0) {
    expect_true(all(result$periodEnd >= as.Date("2024-06-01")))
    expect_true(all(result$periodBegin <= as.Date("2024-12-31")))
  }
})

test_that("redfin_market_data ZIP codes are clean (no prefix)", {
  skip_if_offline()

  result <- redfin_market_data(start_date = "2024-10-01")

  if (nrow(result) > 0) {
    # ZIP codes should be numeric strings, not have "Zip Code:" prefix
    expect_false(any(grepl("Zip Code:", result$zipCode)))
    expect_true(all(grepl("^\\d{5}$", result$zipCode)))
  }
})
