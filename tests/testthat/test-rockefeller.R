test_that("Rockefeller returns expected results", {
  suppressMessages(vcr::use_cassette("rockefeller", serialize_with="json", {
    one_result <- .standardize_rockefeller("testing",
                                           "2011-01-01", "2021-01-31",
                                           FALSE)
  }))

  expect_gt(nrow(one_result), 40)
  # Hash generated ID number
  #expect_equal("R18132", one_result$id)
})
