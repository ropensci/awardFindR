test_that("Templeton returns expected results", {
  suppressMessages(vcr::use_cassette("templeton", {
    one_result <- .templeton_standardize("ethnography",
                                         "2015-01-01", "2016-01-01")
  }))

  expect_equal("57690", one_result$id)
})
