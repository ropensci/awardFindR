#test_that("Sloan returns expected results", {
#  suppressMessages(vcr::use_cassette("full", {
#    results <- sloan_standardize("qualitative", "2014-01-01", "2020-01-01")
#  }))
#  expect_equal(nrow(results), 2)
#})
