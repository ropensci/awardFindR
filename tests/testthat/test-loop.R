test_that("NIH and NSF multiple page query", {
  suppressMessages(vcr::use_cassette("nih", {
    nih <- awardFindR::get_nih("qualitative", "2019-01-01", "2019-02-25")
    nsf <- awardFindR::get_nsf("qualitative", "2019-01-01", "2019-02-25")
  }))
  expect_gt(nrow(nih), 20)
  expect_gt(nrow(nsf), 25)
})
