test_that("Mellon returns expected results", {
  skip_on_cran()
  suppressMessages(vcr::use_cassette("mellon", serialize_with="json", record="new_episodes", {
    mellon <- .standardize_mellon("case studies", "2018-01-01", "2021-01-01",
                                  FALSE)
    no_results <- .standardize_mellon("hippopotamus", "2018-01-01", "2019-01-01",
                                  FALSE)
  }))
  expect_gt(nrow(mellon), 10)
  expect_equal(nrow(no_results), 0)
})
