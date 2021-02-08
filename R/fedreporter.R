#' Search awards from the Federal Reporter
#' @param keyword Keyword, single string
#' @param from_year Beginning fiscal year to search, integer
#' @param to_year Ending fiscal year to search, integer
#' @param agency Agency keyword for search criteria. Single comma-separated string. Defaults to "usda,dod,nasa,epa".
#' @export
#' @return A data.frame
#' @examples
#' federal <- fedreporter_get(keyword="ethnography", from_year=2020, to_year=2021, agency="nih")
fedreporter_get <- function (keyword, from_year, to_year,
                             agency="usda,dod,nasa,epa") {
  base_url <- 'https://api.federalreporter.nih.gov/v1/Projects/search'

  query_url <- paste0(base_url,
                      "?query=agency:", toupper(agency),
                      "$text:", xml2::url_escape(keyword), "$textFields:terms",
                      # Only date paramater available in the fedreporter query is by fiscal year :(
                      # Also, you can't use ranges, so this collates each consecutive year with commas
                      "$fy:", paste0(as.integer(from_year):as.integer(to_year), collapse=","))

  # Actually query the API
  api <- request(query_url, "get")

  if (api$totalCount==0) return(NULL) # No results?
  df <- api$items
  df <- lapply(df, lapply, function(x)ifelse(is.null(x), NA, x)) # NULL to NA
  df <- do.call(rbind.data.frame, df)

  # Do we need to loop until we hit the limit?
  while (api$offset+api$limit < api$totalCount) {
    offset <- api$offset + api$limit # New offset
    new_url <- paste0(query_url, "&offset=", offset)
    api <- request(new_url, "get")

    # Process list
    temp <- api$items
    temp <- lapply(temp, lapply, function(x)ifelse(is.null(x), NA, x)) # NULL to NA
    temp <- do.call(rbind.data.frame, temp)

    df <- rbind.data.frame(df, temp) # Merge
  }

  # Find duplicates (renewals?)
  # Results seem to be returned from most to least recent, so we can just drop the duplicates
  if (nchar(as.character(df$projectNumber[1])) > 14) {
    # This is for NIH
    df$id_main <- substr(df$projectNumber, 2, 12)
  } else {
    # This works for DoD
    df$id_main <- substr(df$projectNumber, 1, 8)
  }
  df <- df[!duplicated(df$id_main), ]
  df$id_main <- NULL

  df[] <- lapply(df, function(x) ifelse(is.factor(x), as.character(x), x)) # Remove factors

  return(df)
}

#' Standardize award results from the Federal Reporter
#' @param keyword Keyword, single string
#' @param from_year Beginning fiscal year to search, integer
#' @param to_year Ending fiscal year to search, integer
#' @return A standardized data.frame
fedreporter_stardardize <- function(keyword, from_year, to_year) {
  federal <- fedreporter_get(keyword, from_year, to_year)
  if (is.null(federal)) return(NULL)
  with(federal, data.frame(
    institution=orgName, pi=contactPi, year=fy,
    start=substr(projectStartDate, 1, 10), end=substr(projectEndDate, 1, 10),
    program=agency, amount=totalCostAmount, id=projectNumber, title,
    source="Federal Reporter",
    stringsAsFactors = FALSE
  ))
}
