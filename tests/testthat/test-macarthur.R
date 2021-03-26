test_that("MacArthur returns expected result", {
  suppressMessages(vcr::use_cassette("macarthur", {
    macarthur <-
      .macarthur_standardize("qualitative", "2018-01-01", "2019-01-01")
  }))
  expect_equal(nrow(macarthur), 1)
})
