test_that("Arnold returns expected results", {
  suppressMessages(vcr::use_cassette("arnold", serialize_with="json", {
    one_result <- suppressMessages(
      .standardize_arnold("qualitative research", "2020-01-02", "2020-01-03",
                          FALSE))
    expect_equal(1, nrow(one_result))
  }))
})
