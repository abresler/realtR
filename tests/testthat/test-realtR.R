context("basic functionality")
test_that("we can do something", {
  expect_that(geocode("1600 Pennsylvania Avenue, NW, Washington DC"), is_a("tbl_df"))
  
})