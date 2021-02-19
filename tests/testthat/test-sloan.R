test_that("Generate sloan data.frame successfully", {
  suppressMessages(vcr::use_cassette("sloan", {
    df <- sloan_df()
  }))
  expect_gt(nrow(df), 2000) # Should only be getting bigger over time
})
