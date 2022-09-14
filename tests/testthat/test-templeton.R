test_that("Templeton returns expected results", {
  suppressMessages(vcr::use_cassette("templeton", serialize_with="json", {
    one_result <- .standardize_templeton("ethnography",
                                         "2015-01-01", "2022-01-01", FALSE)
  }))

  expect_equal("62101", one_result$id)
})
