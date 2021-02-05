test_that("USASpending returns null with zero results", {
  expect_null(
    suppressMessages(usaspend_get("foobar", "2012-01-01", "2021-01-01")))
})
