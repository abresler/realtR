# NLP function tests
# Tests for summarise_broker_bullshit

library(testthat)
library(realtR)

context("NLP functions")

test_that("summarise_broker_bullshit handles valid input", {
  skip_if_not_installed("udpipe")
  skip_if_not_installed("textrank")
  skip("Requires udpipe model download - skip for CI")

  # Test with a simple property description
  description <- "Beautiful home with stunning views. The kitchen features granite countertops and stainless steel appliances. The living room has hardwood floors and a cozy fireplace. Large backyard perfect for entertaining."

  result <- summarise_broker_bullshit(description = description)

  expect_s3_class(result, "tbl_df")
  expect_true("textOriginal" %in% names(result))
})

test_that("summarise_broker_bullshit function exists", {
  expect_true(is.function(summarise_broker_bullshit))
})
