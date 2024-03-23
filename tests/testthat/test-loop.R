test_that("NIH and NSF multiple page query", {
  skip_on_cran()
  suppressMessages(vcr::use_cassette("loop", serialize_with="json", record="new_episodes", {
    nih <- awardFindR::get_nih("dataset", "2019-01-01", "2020-01-01")
    nsf <- awardFindR::get_nsf("qualitative", "2019-01-01", "2019-02-25")
    carnegie <- awardFindR::get_carnegie("analysis", "2000", "2024")
  }))
  expect_gt(nrow(nih), 500)
  expect_gt(nrow(nsf), 25)
  expect_gt(nrow(carnegie), 100)
})
