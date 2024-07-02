test_that("MacArthur returns expected result", {
  skip_on_cran()
  suppressMessages(vcr::use_cassette("macarthur", serialize_with="json", record="new_episodes", {
    macarthur <-
      .standardize_macarthur("qualitative", "2015-01-01", "2019-01-01",
                             FALSE)
  }))
  expect_equal(nrow(macarthur), 3)
})
