test_that("Expected results from Open Society", {
  suppressMessages(vcr::use_cassette("osociety", {
    osociety <- .osociety_standardize("qualitative", "2012-01-01", "2020-01-01",
                                      FALSE)
  }))
  expect_equal(osociety$id[1], "OR2019-63941")
})
