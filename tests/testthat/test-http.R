test_that("HTTP GET works to fetch html", {
  expect_match(attributes(suppressMessages(request("r-project.org", "get")))$class[1],
               "xml_document")
})
