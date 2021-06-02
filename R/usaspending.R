#' Search the USAspending database
#' @param keywords Vector of keywords to search
#' @param from_date Beginning date object to search
#' @param to_date Ending date object to search
#' @param verbose enable verbose HTTP messages. TRUE/FALSE, default: false
#' @return a data.frame
#' @export
#' @examples
#' \dontrun{
#' results <- usaspend_get(c("qualitative", "interview"),
#'  "2019-01-01", "2020-01-01")
#' }
usaspend_get <- function(keywords, from_date, to_date, verbose) {
  url <- "https://api.usaspending.gov/api/v2/search/spending_by_award/"

  payload <- list(
    fields=c("Award ID", "Recipient Name", "Description",
             "Start Date", "End Date", "Award Amount",
             "Awarding Agency", "Awarding Sub Agency",
             "Funding Agency", "Funding Sub Agency"),
    filters=list(
      agencies=list(
        # These should be agencies that are not covered by:
        # Federal Reporter, NIH RePORTER or NSF
        # because the data is limited from USAspending compared to other sources
        list(name="Institute of Museum and Library Services", tier="toptier",
             type="awarding"),
        list(name="Smithsonian Institution", tier="toptier", type="awarding"),
        list(name="Department of Commerce", tier="toptier", type="awarding"),
        list(name="Department of Education", tier="toptier", type="awarding"),
        list(name="Department of the Interior", tier="toptier",
             type="awarding")),
      award_type_codes=c("02", "03", "04", "05"), # Only grants
      keywords=as.list(keywords),
      recipient_type_names=list("higher_education"),
      # An array syntax quirk in the API demands this double list(list())
      time_period=list(list(start_date=from_date, end_date=to_date))),
    limit=50, page=1, order="desc", subawards="false"
  )

  # query API
  response <- request(url, "post", verbose, payload)

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
    response <- request(url, "post", verbose, payload)

    temp <- response$results
    temp <- lapply(temp, lapply, function(x)ifelse(is.null(x), NA, x))
    temp <- do.call(rbind.data.frame, temp)

    awards <- rbind.data.frame(awards, temp)
  }

  awards[] <- lapply(awards,
                     function(x) ifelse(is.factor(x),
                                        as.character(x), x)) # No factors

  awards
}

.usaspend_standardize <- function(keywords, from_date, to_date, verbose) {
  raw <- usaspend_get(keywords, from_date, to_date, verbose)
  if (is.null(raw)) {
    return(NULL)
  }

  with(raw, data.frame(
    institution=Recipient.Name, pi=NA, year=substr(Start.Date, 1, 4),
    start=Start.Date, end=End.Date, program=Awarding.Agency,
    amount=as.integer(Award.Amount), id=Award.ID, title=Description,
    abstract=NA, keyword=NA, source="USASpending", stringsAsFactors = FALSE
  ))
}
