test_that("HTTP GET works to fetch html", {
  skip_on_cran()
  expect_match(attributes(suppressMessages(
    request("http://r-project.org", "get")))$class[1],
               "xml_document")
})

