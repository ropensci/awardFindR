test_that("Rockefeller returns expected results", {
  suppressMessages(vcr::use_cassette("rockefeller", {
    one_result <- .rockefeller_standardize("testing",
                                           "2011-01-01", "2021-01-31",
                                           FALSE)
  }))

  expect_equal(1, nrow(one_result))
  # Hash generated ID number
  expect_equal("R18132", one_result$id)
})
