# Which sources should be included in this bucket?
sources <- c("macarthur", "gates", "mellon",
             "carnegie", "ophil", "osociety")

test_that("Zero results warns and returns null", {
  expect_warning(return <- suppressMessages(awardFindR("foobar", sources)),
                 "No results from any sources")
  expect_null(return)
})

test_that("Small but complex keyword query", {
  return <- suppressMessages(
    awardFindR(c("qualitative", "interviews with"), sources,
               "2015-01-01", "2021-01-01"))
  # More than 1 result per source?
  expect_gt(length(unique(return$id)), length(sources))
  # If the date columns are numeric or whatever, they've been corrupted
  expect_type(return$start, "character")
  expect_type(return$end, "character")
})

test_that("Simple keyword NSF mix for type compatibility", {
  return <- suppressMessages(awardFindR("qualitative", c("nsf", "ssrc"),
                     "2015-01-01", "2015-02-01"))
  # If the date columns are numeric or whatever, they've been corrupted
  expect_type(return$start, "character")
  expect_type(return$end, "character")
  expect_type(return$year, "character")
})
