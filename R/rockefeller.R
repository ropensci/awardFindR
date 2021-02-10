#' Search Rockefeller Foundation grants
#' @param keyword Single keyword to query
#' @param from_date Date object to begin search
#' @param to_date Date object to end search
#' @return a data.frame
#' @export
#' @examples
#' rockefeller <- rockefeller_get("test", "2012-01-01", "2021-01-01")
rockefeller_get <- function(keyword, from_date, to_date) {
  url <- paste0("https://www.rockefellerfoundation.org/?post_type=grant&",
  "keyword=", xml2::url_escape(keyword),
  "&from_month=", format.Date(from_date, "%m"), "&from_year=", format.Date(from_date, "%Y"),
  "&to_month=", format.Date(to_date, "%m"), "&to_year=", format.Date(to_date, "%Y"), "&download=filter")

  response <- as.data.frame(request(url, "get"))
  if (nrow(response)==0) return(NULL) # No results?
  return(response)
}

#' Standardize Rockefeller Foundation grants search
#' @param keyword Single keyword to query
#' @param from_date Date object to begin search
#' @param to_date Date object to end search
#' @return a standardized data.frame
rockefeller_standardize <- function(keyword, from_date, to_date) {
  rockefeller <- rockefeller_get(keyword, from_date, to_date)
  if (is.null(rockefeller)) return(NULL)
  with(rockefeller, data.frame(
    institution=Title, pi=NA, year=format.Date(`Grant Term Start`, "%Y"),
    start=as.character(`Grant Term Start`), end=as.character(`Grant Term End`),
    program=Initiative, amount=as.integer(`Grant Amount`),
    id=paste0("R", 1:nrow(rockefeller)), title=Description, keyword,
    source="Rockefeller", stringsAsFactors = FALSE
  ))
}
