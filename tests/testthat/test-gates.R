test_that("Gates foundation search", {
  suppressMessages(vcr::use_cassette("gates", {
    gates <- .gates_standardize("qualitative", "2018-01-01", "2019-01-01")
  }))
  expect_equal(nrow(gates), 3)
})
