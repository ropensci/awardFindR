test_that("Rockefeller returns expected results", {
  one_result <- suppressMessages(rockefeller_standardize("testing", "2011-01-01", "2021-01-31"))
  expect_equal(1, nrow(one_result))
  # Hash generated ID number
  expect_equal("R18132", one_result$id)

  # Fail gracefully
  expect_null(suppressMessages(rockefeller_standardize("foobar goonar", "2019-01-01", "2019-01-02")))
})
