#' Search NIH RePORTER
#'
#' @param keyword Keyword to query
#' @param from_date Date object to begin search
#' @param to_date Date object to end search
#' @return a data.frame
#' @export
#' @examples nih <- nih_get("ethnography", "2019-01-01", "2019-05-01")
nih_get <- function(keyword, from_date, to_date) {
  url <- "https://api.reporter.nih.gov/v1/projects/Search"

  # httr encodes all this into json for a POST request
  payload <- list(criteria=list(
    award=list(award_notice_date=list(
        from_date=from_date,
        to_date=to_date)),
    exclude_subprojects="true",
    advanced_text_search=list(
      search_text=keyword,
      operator="AND",
      search_field="terms")
  ),
  include_fields=c("org_name", "project_num", "project_serial_num",
                   "fiscal_year", "project_start_date", "project_end_date",
                   "project_title", "agency_code",
                   "contact_pi_name", "award_amount"), #, "principle_investigators"))
  offset=0)

  # Query API
  response <- request(url, "post", payload)
  # No results?
  if (response$meta$total == 0) {
    return(NULL)
  }
  # change NULL values to NA
  df <- lapply(response$results, lapply, function(x)ifelse(is.null(x), NA, x))
  df <- do.call(rbind.data.frame, df)

  # What is our current max?
  place <- response$meta$offset + response$meta$limit
  # Do we need to loop with different offsets?
  while (place < response$meta$total) {
    payload$offset <- place
    # Query again
    response <- request(url, "post", payload)
    # Record our new position
    place <- response$meta$offset + response$meta$limit

    # Bind everything together
    temp <- lapply(response$results, lapply, function(x)ifelse(is.null(x), NA, x))
    temp <- do.call(rbind.data.frame, temp)
    df <- rbind.data.frame(df, temp)
  }

  # Remove duplicates
  df <- df[!duplicated(df$project_serial_num), ]
  # Remove factors
  df[] <- lapply(df, function(x) ifelse(is.factor(x), as.character(x), x))

  return(df)
}
