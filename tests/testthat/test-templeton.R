test_that("Templeton returns expected results", {
  one_result <- suppressMessages(templeton_standardize("ethnography", "2015-01-01", "2016-01-01"))
  expect_equal("57690", one_result$id)
  expect_null(suppressMessages(templeton_standardize("foobar", "2010-01-01", "2020-01-01")))
})
