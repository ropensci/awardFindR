#' Scrape data from the Federal Reporter
#'
#' Query the Federal Reporter API for grants, get the XML response(s) and process it into a data.frame
#'
#' @param query Keyword, single string
#' @param from Beginning fiscal year to search, integer
#' @param to Ending fiscal year to search, integer
#' @param agency Agency keyword for search criteria. Single comma-separated string. Defaults to "usda,dod,nasa,epa".
#' @export
#' @return A data.frame from the API, or NULL if no results
#'
#' @examples
#' nih <- fedreporter_get(query="ethnography", from=2020, to=2021, agency="nih")
fedreporter_get <- function (query, from, to,
                             agency="usda,dod,nasa,epa") {
  base_url <- 'https://api.federalreporter.nih.gov/v1/Projects/search'

  query_url <- paste0(base_url,
                      "?query=agency:", toupper(agency),
                      "$text:", xml2::url_escape(query), "$textFields:terms",
                      # Only date paramater available in the fedreporter query is by fiscal year :(
                      # Also, you can't use ranges, so this collates each consecutive year with commas
                      "$fy:", paste0(as.integer(from):as.integer(to), collapse=","))

  # Actually query the API
  api <- request_xml(query_url)

  # No results?
  if (xml2::xml_integer(xml2::xml_find_first(api, "/SearchResultOfApiProject/TotalCount")) == 0) {
    return(NULL)
  }

  # Max result count 50. Do we need to loop with different offsets?
  extra <- api
  while (xml2::xml_integer(xml2::xml_find_first(extra, '/SearchResultOfApiProject/TotalCount'))==50) {
    offset <- xml2::xml_integer(xml2::xml_find_first(extra, "/SearchResultOfApiProject/Offset")) +
      xml2::xml_integer(xml2::xml_find_first(extra, "/SearchResultOfApiProject/TotalCount"))

    new_url <- paste0(query_url, "&offset=", offset)
    # Fetch next page
    extra <- request_xml(new_url)
    # Add the items tag to existing internal XML tree
    xml2::xml_add_child(api, xml2::xml_find_first(extra, "/SearchResultOfApiProject/Items"))

  }
  rm(extra)

  # Convert to data.frame
  awards <- xml2::xml_find_all(api, "//ApiProject")
  df <- xml2list_to_df(xml2::as_list(awards))

  # Find duplicates (renewals?)
  # Results seem to be returned from most to least recent, so we can just drop the duplicates
  if (nchar(as.character(df$ProjectNumber[1])) > 14) {
    # This is for NIH
    df$id_main <- substr(df$ProjectNumber, 2, 12)
  } else {
    # This works for DoD
    df$id_main <- substr(df$ProjectNumber, 1, 8)
  }
  df <- df[!duplicated(df$id_main), ]
  df$id_main <- NULL

  return(df)

}
