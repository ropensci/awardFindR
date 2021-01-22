#' Query the USAspending database
#'
#' @param queries Vector of keywords to search
#' @param from Beginning date object to search
#' @param to Ending date object to search
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' # Single keyword
#' results <- usaspend_get("interview", "2018-01-01", "2020-01-01")
#'
#' # Multiple keywords
#' results <- usaspend_get(c("qualitative", "interview"), "2019-01-01", "2020-01-01")
usaspend_get <- function(queries, from, to) {
  url <- "https://api.usaspending.gov/api/v2/search/spending_by_award/"

  payload <- list(
    fields=c("Award ID", "Recipient Name", "Description",
             "Start Date", "End Date", "Award Amount",
             "Awarding Agency", "Awarding Sub Agency",
             "Funding Agency", "Funding Sub Agency"),
    filters=list(
      agencies=list(
        list(name="Institute of Museum and Library Services", tier="toptier", type="awarding"),
        list(name="Smithsonian Institution", tier="toptier", type="awarding"),
        list(name="Department of Commerce", tier="toptier", type="awarding"),
        list(name="Department of Education", tier="toptier", type="awarding"),
        list(name="Department of the Interior", tier="toptier", type="awarding")),
      award_type_codes=c("02", "03", "04", "05"), # Only grants
      keywords=as.list(queries),
      recipient_type_names=list("higher_education"),
      # An array syntax quirk in the API demands this double list(list())
      time_period=list(list(start_date=from, end_date=to))),
    limit=50, page=1, order="desc", subawards="false"
  )

  message("Querying USAspending API...")
  response <- httr::POST(url, body=payload, encode="json")
  response <- httr::content(response)

  awards <- response$results
  if (length(awards)==0) {
    return(NULL)
  }
  # Replace NULL with NA
  awards <- lapply(awards, lapply, function(x)ifelse(is.null(x), NA, x))
  awards <- do.call(rbind.data.frame, awards)

  # Need to loop queries?
  while (response$page_metadata$hasNext == TRUE) {
    payload$page <- response$page_metadata$page + 1
    message(paste0("Querying USAspending API... (page ", payload$page, ")"))
    response <- httr::POST(url, body=payload, encode="json")
    response <- httr::content(response)

    temp <- response$results
    temp <- lapply(temp, lapply, function(x)ifelse(is.null(x), NA, x))
    temp <- do.call(rbind.data.frame, temp)

    awards <- rbind.data.frame(awards, temp)
  }

  return(awards)
}
