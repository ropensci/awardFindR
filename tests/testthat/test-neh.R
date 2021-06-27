# need to find problems on windows

test_that("NEH returns expected results", {
 suppressMessages(vcr::use_cassette("neh", {
   neh <- .standardize_neh("qualitative", "2019-01-01", "2020-01-01",
                           FALSE)
  }))
  expect_equal("AE-264000-19", neh$id[1])
})
