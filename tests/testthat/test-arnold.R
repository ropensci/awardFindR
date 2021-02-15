test_that("Arnold returns expected results", {
  one_result <- suppressMessages(arnold_standardize("qualitative research", "2020-01-02", "2020-01-03"))
  expect_equal("136154", one_result$id)

  # Fail gracefully
  expect_null(suppressMessages(arnold_standardize("foobar", "2019-01-01", "2019-01-02")))
})
