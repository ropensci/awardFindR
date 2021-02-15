# Which sources should be included in this bucket?
sources <- c("nsf", "nih", "ssrc", "fedreporter")

test_that("Fails gracefully with empty data.frame", {
  empty <- suppressMessages(awardFindR("foobar", sources))
  expect_equal(data.frame(), empty)
})

test_that("Complex keyword query", {
  return <- suppressMessages(
    awardFindR(c("ethnography", "qualitative analysis"), sources,
                       "2019-01-01", "2019-06-01"))
  expect_gt(length(unique(return$id)), length(sources)) # More than 1 per source
  expect_type(return$id, "character") # Any corruption?
})

