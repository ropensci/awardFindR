test_that("RSF search", {
  suppressMessages(vcr::use_cassette("rsf", {
    rsf <- .rsf_standardize("semi-structured interviews",
                            "2018-01-01", "2020-01-01")
  }))
  expect_equal(nrow(rsf), 1)
})
