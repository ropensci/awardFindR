test_that("Incorrect date", {
  expect_error(search_awards("test", "nsf", "foobar"), "date invalid")
  expect_error(search_awards("test", "nsf", "2018-01-01", "foobar"),
               "date invalid")
  expect_error(search_awards("test", "nsf", "2018-01-01", "2016-01-01"),
               "must be after")

})

test_that("Search that should return all empty", {
  foobar <- search_awards("foobar", to_date="2021-01-01")
  expect_equal(data.frame(), foobar)
})

test_that("merging multiple sources and multiple keywords with spaces", {
  keywords <- c("ethnography", "qualitative data")
  csv <- tempfile()
  writeLines(keywords, csv)

  # missing ssrc, rsf &
  suppressMessages(vcr::use_cassette("full", record="new_episodes", {
    results <- search_awards(csv,
                             sources=c("arnold", "carnegie",
                                      "gates", "macarthur", "mellon",
                                      "nih", "nsf", "rockefeller", "ophil",
                                      "osociety", "rwjf",
                                      "sloan", "ssrc", "templeton", "usaspend"),
                             from_date = "2018-01-01", to_date="2018-02-01",
                          verbose=TRUE)
  }))
  expect_gt(nrow(results), 1)
  # If the date columns are numeric or whatever, they've been corrupted
  expect_type(results$start, "character")
  expect_type(results$end, "character")
})
