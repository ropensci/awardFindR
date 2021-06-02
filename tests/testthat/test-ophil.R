test_that("Expected results", {
  suppressMessages(vcr::use_cassette("ophil", {
    ophil <- .ophil_standardize("qualitative", "2012-01-01", "2020-01-01",
                                FALSE)
  }))
  expect_equal(nrow(ophil), 4)
})
