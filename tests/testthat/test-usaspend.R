test_that("Expected USAspending results", {
  suppressMessages(vcr::use_cassette("usaspend", {
    usa <- .standardize_usaspend("test", "2012-01-01", "2014-01-01", FALSE)
  }))
  expect_equal(usa$id[1], "S330B130017")
})
