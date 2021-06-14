test_that("Expected results from fedreporter", {
  suppressMessages(vcr::use_cassette("fedreporter", {
    fed <- .standardize_fedreporter("interviews", "2012-01-01", "2020-01-01",
                                    FALSE)
  }))
  expect_equal(nrow(fed), 42)
})
