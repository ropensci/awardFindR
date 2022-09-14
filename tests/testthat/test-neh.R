# need to find problems on windows

test_that("NEH returns expected results", {
 suppressMessages(vcr::use_cassette("neh", serialize_with="json", {
   neh <- .standardize_neh("qualitative", "2019-01-01", "2020-01-01",
                           FALSE)
  }))
  expect_equal("MN-268921-20", neh$id[1])
})
