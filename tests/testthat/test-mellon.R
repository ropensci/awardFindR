test_that("Mellon returns expected results", {
  skip_on_cran()
  suppressMessages(vcr::use_cassette("mellon", serialize_with="json", record="new_episodes", {
    mellon <- .standardize_mellon("database", "2018-01-01", "2018-02-01", FALSE)
    no_results <- .standardize_mellon("hippopotamus", "2018-01-01", "2019-01-01", FALSE)
  }))
  expect_gt(nrow(mellon), 2)
  expect_null(no_results)
})
