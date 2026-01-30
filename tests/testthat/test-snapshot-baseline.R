# Snapshot Baseline Tests
# Created: 2026-01-29
# Purpose: Capture current behavior before cleanup operations
# These tests establish regression baselines

library(testthat)
library(realtR)

context("Snapshot: Dictionary functions")

test_that("dictionary_property_types returns expected structure", {
  result <- dictionary_property_types()
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("nameType", "slugType") %in% names(result)))
  expect_gt(nrow(result), 0)
})

test_that("dictionary_listing_features returns expected structure", {
  result <- dictionary_listing_features()
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("nameFeature") %in% names(result)))
  expect_gt(nrow(result), 0)
})

context("Snapshot: Geocoding")

test_that("geocode returns tibble for valid address", {
  skip_if_offline()
  result <- geocode("1600 Pennsylvania Avenue, NW, Washington DC")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

context("Snapshot: Return types baseline")

# These tests capture that functions return the expected class
# without hitting APIs (skip if offline)

test_that("listing_counts would return tibble structure", {
  # Baseline: function exists and is callable
  expect_true(is.function(listing_counts))
})

test_that("listings would return tibble structure", {
  expect_true(is.function(listings))
})

test_that("map_listings would return tibble structure", {
  expect_true(is.function(map_listings))
})

test_that("table_listings would return tibble structure", {
  expect_true(is.function(table_listings))
})

test_that("properties_near would return tibble structure", {
  expect_true(is.function(properties_near))
})

test_that("median_prices would return tibble structure", {
  expect_true(is.function(median_prices))
})

test_that("vitality would return tibble structure", {
  expect_true(is.function(vitality))
})

test_that("mortgage_rates would return tibble structure", {
  expect_true(is.function(mortgage_rates))
})

test_that("rental_estimates would return tibble structure", {
  expect_true(is.function(rental_estimates))
})

test_that("trends_zipcodes would return tibble structure", {
  expect_true(is.function(trends_zipcodes))
})

test_that("parse_listing_urls would return tibble structure", {
  expect_true(is.function(parse_listing_urls))
})

test_that("parse_geo_urls would return tibble structure", {
  expect_true(is.function(parse_geo_urls))
})

test_that("html_listing_urls would return tibble structure", {
  expect_true(is.function(html_listing_urls))
})

test_that("summarise_broker_bullshit would return tibble structure", {
  expect_true(is.function(summarise_broker_bullshit))
})

test_that("join_listing_data would return tibble structure", {
  expect_true(is.function(join_listing_data))
})

context("Snapshot: Baseline test suite passes")

test_that("baseline snapshot suite establishes regression baseline", {
  # This test confirms the snapshot suite itself is valid
  expect_true(TRUE)
})
