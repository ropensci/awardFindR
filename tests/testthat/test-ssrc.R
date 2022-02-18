test_that("Expected results with looping", {
  suppressMessages(vcr::use_cassette("ssrc", {
    ssrc <- .standardize_ssrc("case studies", "2018-01-01", "2021-01-01",
                              FALSE)
  }))
  expect_equal(nrow(ssrc), 54)
})
