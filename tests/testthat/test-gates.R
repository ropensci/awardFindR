test_that("Gates foundation search", {
  suppressMessages(vcr::use_cassette("gates", {
    gates <- .standardize_gates("qualitative", "2018-01-01", "2019-01-01",
                                FALSE)
  }))
  expect_equal(nrow(gates), 3)
})
