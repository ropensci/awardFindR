#' Search awards from the Federal Reporter
#' @param agency Agencies, comma separated. Defaults to "usda,dod,nasa,epa".
#' @param keyword Keyword to query
#' @inheritParams neh_get
#' @return A data.frame
#' @export
#' @examples
#' \dontrun{
#' federal <- fedreporter_get(keyword="ethnography",
#' from_year=2020, to_year=2021, agency="nih")
#' }
fedreporter_get <- function (keyword, from_year, to_year, verbose=FALSE,
                             agency="usda,dod,nasa,epa,ed") {
  base_url <- 'https://api.federalreporter.nih.gov/v1/Projects/search'

  query_url <- paste0(base_url,
                      "?query=agency:", toupper(agency),
                      "$text:", xml2::url_escape(keyword), "$textFields:terms",
                      # Only date paramater available is fiscal year :(
                      # Also, you can't use ranges
                      "$fy:", paste0(as.integer(from_year):as.integer(to_year),
                                     collapse=","))

  # Actually query the API
  api <- request(query_url, "get", verbose)

  if (api$totalCount==0) {
    return(NULL) # No results?
  }

  df <- api$items
  df <- lapply(df, lapply, function(x)ifelse(is.null(x), NA, x)) # NULL to NA
  df <- do.call(rbind.data.frame, df)

  # Do we need to loop until we hit the limit?
  while (api$offset+api$limit < api$totalCount) {
    offset <- api$offset + api$limit # New offset
    new_url <- paste0(query_url, "&offset=", offset)
    api <- request(new_url, "get", verbose)

    # Process list
    temp <- api$items
    # NULL to NA
    temp <- lapply(temp, lapply, function(x)ifelse(is.null(x), NA, x))
    temp <- do.call(rbind.data.frame, temp)

    df <- rbind.data.frame(df, temp) # Merge
  }

  # Find duplicates (renewals?)
  # Results returned from most to least recent, so we drop the duplicates
  df$id_main <- substr(df$projectNumber, 1, 8)
  df <- df[!duplicated(df$id_main), ]
  df$id_main <- NULL

  df$keyword <- keyword

  df
}

.fedreporter_standardize <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, fedreporter_get,
                    format.Date(from_date, "%Y"), format.Date(to_date, "%Y"),
                verbose)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    return(NULL)
  }

  with(raw, data.frame(
    institution=orgName, pi=contactPi, year=fy,
    start=substr(projectStartDate, 1, 10), end=substr(projectEndDate, 1, 10),
    program=agency, amount=totalCostAmount, id=projectNumber, title,
    abstract, keyword, source="Federal Reporter", stringsAsFactors = FALSE
  ))
}
