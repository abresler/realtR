# Dictionary function tests
# These tests verify core dictionary functions work correctly

library(testthat)
library(realtR)

context("Dictionary functions")

test_that("dictionary_property_types returns valid tibble", {
  result <- dictionary_property_types()

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true("nameType" %in% names(result))
  expect_true("slugType" %in% names(result))

  # Check expected property types exist
  types <- result$nameType
  expect_true("House" %in% types)
  expect_true("Condo" %in% types)
})

test_that("dictionary_listing_features returns valid tibble", {
  result <- dictionary_listing_features()

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true("nameFeature" %in% names(result))
  expect_true("slugFeature" %in% names(result))

  # Check expected features exist
  features <- result$nameFeature
  expect_true("Basement" %in% features)
  expect_true("Garage" %in% features)
})

test_that("dictionary functions are idempotent",
 {
  # Same call should return same result
  result1 <- dictionary_property_types()
  result2 <- dictionary_property_types()
  expect_equal(result1, result2)

  result3 <- dictionary_listing_features()
  result4 <- dictionary_listing_features()
  expect_equal(result3, result4)
})
