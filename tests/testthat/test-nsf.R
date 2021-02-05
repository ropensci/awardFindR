test_that("NSF returns expected results", {
  nsf <- suppressMessages(nsf_get("qualitative data repository",
                                  "2016-01-01", "2017-01-01"))
  expect_equal(nrow(nsf), 2)
  expect_match(as.character(nsf$id[1]), "1628636")
})

test_that("NSF returns null with zero results", {
  expect_null(
    suppressMessages(nsf_get("foobar", "2012-01-01", "2021-01-01")))
})
