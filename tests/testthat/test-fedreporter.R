test_that("Expected results from fedreporter", {
  suppressMessages(vcr::use_cassette("fedreporter", {
    fed <- .fedreporter_standardize("interviews", "2012-01-01", "2020-01-01")
  }))
  expect_equal(nrow(fed), 42)
})
