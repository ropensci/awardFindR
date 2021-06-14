test_that("Templeton returns expected results", {
  suppressMessages(vcr::use_cassette("templeton", {
    one_result <- .standardize_templeton("ethnography",
                                         "2015-01-01", "2016-01-01", FALSE)
  }))

  expect_equal("57690", one_result$id)
})
