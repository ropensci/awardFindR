#' Search NIH RePORTER
#' @param keyword Keyword to query
#' @param from_date Date object to begin search
#' @param to_date Date object to end search
#' @param verbose enable verbose HTTP messages. TRUE/FALSE, default: false
#' @param payload A custom NIH search query object. Only for advanced purposes. default: null
#' @return a data.frame
#' @export
#' @examples nih <- get_nih("ethnography", "2019-01-01", "2019-05-01")
get_nih <- function(keyword, from_date, to_date, verbose=FALSE, payload=NULL) {
  url <- "https://api.reporter.nih.gov/v2/projects/Search"

  if (is.null(payload)) {
    # httr encodes all this into json for a POST request
    payload <- list(criteria=list(
      award_notice_date=list(
        from_date=from_date,
        to_date=to_date),
      exclude_subprojects="true",
      advanced_text_search=list(
        search_text=paste0("\"", keyword, "\""),
        operator="advanced",
        search_field="abstracttext")
    ),
    include_fields=c("Organization", "ProjectNum", "ProjectSerialNum",
                     "FiscalYear", "ProjectStartDate", "ProjectEndDate",
                     "ProjectTitle", "AgencyCode", "AbstractText",
                     "ContactPiName", "AwardAmount", "AwardNoticeDate"),
    #, "principle_investigators"))
    limit=500, offset=0)
  }

  response <- request(url, "post", verbose, payload) # Query API
  if (response$meta$total == 0) {
    return(NULL) # No results?
  } else if (response$meta$total > 9999) {
    warning(paste0("NIH query for \"", keyword, "\" too large, returning empty"))
    return(NULL)
  }

  # Extract OrgName, which is currently nested inside Organization
  response$results <- lapply(response$results, function(x) {
    x$org_name <- x$organization$org_name
    x$organization <- NULL
    x
  })

  # change NULL values to NA
  df <- lapply(response$results, lapply, function(x)ifelse(is.null(x), NA, x))
  df <- Reduce(function(x, y) merge(x, y, all=TRUE), df)
  df <- as.data.frame(df)

  # What is our current max?
  place <- response$meta$offset + response$meta$limit
  # Do we need to loop with different offsets?
  while (place < response$meta$total) {
    payload$offset <- place

    Sys.sleep(3) # Be NICE to the API!

    response <- request(url, "post", verbose, payload) # Query again
    place <- response$meta$offset + response$meta$limit # New position

    # Extract OrgName from Organization
    response$results <- lapply(response$results, function(x) {
      x$org_name <- x$organization$org_name
      x$organization <- NULL
      x
    })

    # Bind everything together
    temp <- lapply(response$results, lapply,
                   function(x)ifelse(is.null(x), NA, x))

    temp <- Reduce(function(x, y) merge(x, y, all=TRUE), temp)
    df <- rbind.data.frame(df, temp)
  }

  # Remove factors
  df[] <- lapply(df, as.character)
  df$keyword <- keyword

  df
}

.standardize_nih <- function(keywords, from_date, to_date, verbose,
                             payload=NULL) {
  raw <- lapply(keywords, get_nih, from_date, to_date, verbose, payload)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    message("No results from NIH")
    return(NULL)
  }

  with(raw, data.frame(
    institution=org_name, pi=contact_pi_name, year=fiscal_year,
    start=project_start_date, end=project_end_date,
    program=agency_code, amount=as.integer(award_amount), id=project_num,
    title=project_title, abstract=abstract_text,
    keyword, source="NIH", stringsAsFactors = FALSE
  ))
}
