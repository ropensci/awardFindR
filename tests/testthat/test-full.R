test_that("Combined search of nonsense term", {
  suppressMessages(vcr::use_cassette("full_empty", {
    foobar <- awardFindR("foobar")
  }))
  expect_equal(data.frame(), foobar)
})

test_that("merging multiple sources and multiple keywords with spaces", {
  suppressMessages(vcr::use_cassette("full", {
    results <- awardFindR(c("ethnography", "qualitative analysis"),
                          from_date = "2018-01-01", to_date="2018-02-01")
  }))
  expect_gt(nrow(results), 1)
  # If the date columns are numeric or whatever, they've been corrupted
  expect_type(results$start, "character")
  expect_type(results$end, "character")

})
