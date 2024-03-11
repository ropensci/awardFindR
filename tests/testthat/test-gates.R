test_that("Gates foundation search", {
  skip_on_cran()
  suppressMessages(vcr::use_cassette("gates", serialize_with="json", record="new_episodes", {
    gates <- .standardize_gates("qualitative", "2018-01-01", "2019-01-01",
                                FALSE)
  }))
  expect_equal(nrow(gates), 3)
})