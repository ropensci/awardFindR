#' Query the USA Spending API
#'
#' @param query Keyword to query
#' @param from Beginning time period
#' @param to Ending time period
#'
#' @return a data.frame
usaspend_get <- function(queries, from, to) {
  url <- "https://api.usaspending.gov/api/v2/spending/"

  payload <- list(filter=list(
      filters=list(
        keywords=queries,
        timePeriodStart=from,
        timePeriodEnd=end)))

  new <- list(type="award",
              filters=list(fy="2019"))

  response <- httr::POST(url,
      body = new,
      encode = "json"
    )
}
