test_that("NIH returns expected results", {
  nih <- suppressMessages(nih_get("ethnography", "2018-01-01", "2018-02-01"))
  expect_equal(nrow(nih), 7)
  expect_match(nih$project_num[1], "5F31HL131441-03")
})

test_that("NIH returns null with zero results", {
  expect_null(
    suppressMessages(nih_get("foobar", "2012-01-01", "2021-01-01")))
})
