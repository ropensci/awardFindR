test_that("Incorrect date", {
  skip_on_cran()
  expect_error(search_awards("test", "nsf", "foobar"), "date invalid")
  expect_error(search_awards("test", "nsf", "2018-01-01", "foobar"),
               "date invalid")
  expect_error(search_awards("test", "nsf", "2018-01-01", "2016-01-01"),
               "must be after")

})

test_that("Search that should return all empty", {
  skip_on_cran()
  foobar <- search_awards("foobar", to_date="2021-01-01")
  expect_equal(data.frame(), foobar)
})

test_that("merging multiple sources and multiple keywords with spaces", {
  skip_on_cran()
  keywords <- c("ethnography", "qualitative data")
  csv <- tempfile()
  writeLines(keywords, csv)

  # missing ssrc, rsf & mellon
  suppressMessages(vcr::use_cassette("full", serialize_with="json", record="new_episodes", {
    results <- search_awards(csv,
                             sources=c("arnold", "carnegie",
                                      "gates", "macarthur", 
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
