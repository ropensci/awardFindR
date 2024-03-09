test_that("NIH returns expected results", {
  skip_on_cran()
  suppressMessages(vcr::use_cassette("nih", serialize_with="json", record="new_episodes", {
    too_many_results <- .standardize_nih("data", "2010-01-01", "2020-01-01",
                                  FALSE)
  }))
  expect_null(too_many_results)
})
