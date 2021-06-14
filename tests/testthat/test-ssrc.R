test_that("Expected results", {
  suppressMessages(vcr::use_cassette("ssrc", {
    ssrc <- .standardize_ssrc("qualitative", "2012-01-01", "2020-01-01",
                              FALSE)
  }))
  expect_equal(nrow(ssrc), 48)
})
