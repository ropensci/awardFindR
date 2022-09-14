test_that("RSF search", {
  suppressMessages(vcr::use_cassette("rsf", serialize_with="json", record="new_episodes", {
    rsf <- .standardize_rsf("semi-structured interviews",
                            "2018-01-01", "2020-01-01", FALSE)
  }))
  expect_equal(nrow(rsf), 1)
})
