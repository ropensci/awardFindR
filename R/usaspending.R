#' Send a request to the USAspending API
#'
#' The USAspending API requires that you send a request to generate a csv file
#' before you are able to download it. This sends out the request,
#' and returns the information necessary to retrieve it later.
#'
#' @param keywords Vector of keywords to search
#' @param from Beginning date to search
#' @param to Ending date to search
#'
#' @return a list object converted from json as returned by USAspending
#' @export
#'
#' @examples
#' \dontrun{usaspend_request(c("qualitative", "case studies"), "2019-01-01", "2019-02-01")}
usaspend_request <- function(keywords, from, to) {
  url <- "https://api.usaspending.gov/api/v2/download/awards/"

  payload <- list(filters=list(
    keyword=keywords,
    date_type="action_date",
    date_range=list(
      start_date=from,
      end_date=to),
    award_type_codes=c("02", "03", "04", "05"), # Only grants
    recipient_type_names=list("higher_education")),
    file_format="csv")

  response <- httr::POST(url, body=payload, encode="json")
  response <- httr::content(response)
  return(response)
}

#' Download USAspending request
#'
#' @param request a USAspending request object from usaspend_request()
#'
#' @return A data.frame
#' @export
usaspend_retrieve <- function(request) {
  status <- httr::GET(request$status_url)
  status <- httr::content(request)
  if (status$status=="finished") {
    temp <- tempfile()
    utils::download.file(status$file_url, temp, mode="wb")
    #unzip(temp)
    awards <- utils::read.csv(temp)
  } else {
    message("Request still not finished. Try again later")
  }
  return(awards)
}
