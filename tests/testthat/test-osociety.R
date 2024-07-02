test_that("Expected results from Open Society", {
  skip_on_cran()
  suppressMessages(vcr::use_cassette("osociety", serialize_with="json", record="new_episodes", {
    osociety <- .standardize_osociety("qualitative", "2012-01-01", "2020-01-01",
                                      FALSE)
  }))
  expect_equal(osociety$id[1], "OR2019-63941")
})
