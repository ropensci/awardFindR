#' Search NIH RePORTER
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
                   "project_title", "agency_code", "abstract_text",
                   "contact_pi_name", "award_amount"),
  #, "principle_investigators"))
  offset=0)

  response <- request(url, "post", payload) # Query API
  if (response$meta$total == 0) {
    return(NULL) # No results?
  }

  # change NULL values to NA
  df <- lapply(response$results, lapply, function(x)ifelse(is.null(x), NA, x))
  df <- do.call(rbind.data.frame, df)

  # What is our current max?
  place <- response$meta$offset + response$meta$limit
  # Do we need to loop with different offsets?
  while (place < response$meta$total) {
    payload$offset <- place
    response <- request(url, "post", payload) # Query again
    place <- response$meta$offset + response$meta$limit # New position

    # Bind everything together
    temp <- lapply(response$results, lapply,
                   function(x)ifelse(is.null(x), NA, x))

    temp <- do.call(rbind.data.frame, temp)
    df <- rbind.data.frame(df, temp)
  }

  # Remove duplicates
  df <- df[!duplicated(df$project_serial_num), ]
  # Remove factors
  df[] <- lapply(df, as.character)
  df$keyword <- keyword

  df
}

.nih_standardize <- function(keywords, from_date, to_date) {
  raw <- lapply(keywords, nih_get, from_date, to_date)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    return(NULL)
  }

  with(raw, data.frame(
    institution=org_name, pi=contact_pi_name, year=fiscal_year,
    start=project_start_date, end=project_end_date,
    program=agency_code, amount=as.integer(award_amount), id=project_num,
    title=project_title, keyword, source="NIH", stringsAsFactors = FALSE
  ))
}
