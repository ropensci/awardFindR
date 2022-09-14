test_that("Expected USAspending results", {
  skip_on_cran()
  suppressMessages(vcr::use_cassette("usaspend", serialize_with="json", {
    usa <- .standardize_usaspend("test", "2012-01-01", "2014-01-01", FALSE)
  }))
  expect_true("S330B130017" %in% usa$id)
})
