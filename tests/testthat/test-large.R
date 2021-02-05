# Which sources should be included in this bucket?
sources <- c("nsf", "nih")

test_that("Zero results warns and returns null", {
  expect_warning(return <- suppressMessages(awardFindR("foobar", sources)),
                 "No results from any sources")
  expect_null(return)
})

test_that("Small but complex keyword query", {
  return <- suppressMessages(
    awardFindR(c("ethnography", "qualitative analysis"), sources,
                       "2019-01-01", "2019-06-01"))
  expect_gt(length(unique(return$id)), length(sources)) # More than 1 per source
  expect_type(return$id, "character") # Any corruption?
})

