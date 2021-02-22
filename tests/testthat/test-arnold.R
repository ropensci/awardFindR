test_that("Arnold returns expected results", {
  suppressMessages(vcr::use_cassette("arnold", {
    one_result <- suppressMessages(arnold_standardize("qualitative research", "2020-01-02", "2020-01-03"))
    expect_equal("136154", one_result$id)
  }))
})
