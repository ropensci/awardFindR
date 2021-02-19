test_that("Expected results", {
  suppressMessages(vcr::use_cassette("ssrc", {
    ssrc <- ssrc_standardize("qualitative", "2012-01-01", "2020-01-01")
  }))
  expect_equal(nrow(ssrc), 48)
})
