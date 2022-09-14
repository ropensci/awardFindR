test_that("Expected results with looping", {
  skip_on_cran()
  suppressMessages(vcr::use_cassette("ssrc", serialize_with="json", record="new_episodes", {
    ssrc <- .standardize_ssrc("case studies", "2018-01-01", "2021-01-01",
                              FALSE)
  }))
  expect_gt(nrow(ssrc), 10)
})
