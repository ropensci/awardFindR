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
  if (nrow(response)==0) {
    return(NULL) # No results?
  }

  response$keyword <- keyword
  response$id <- sapply(response$Description, .text_hash)

  response
}

.rockefeller_standardize <- function(keywords, from_date, to_date) {
  raw <- lapply(keywords, rockefeller_get, from_date, to_date)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    return(NULL)
  }

  with(raw, data.frame(
    institution=Title, pi=NA, year=format.Date(`Grant Term Start`, "%Y"),
    start=as.character(`Grant Term Start`), end=as.character(`Grant Term End`),
    program=Initiative, amount=as.integer(`Grant Amount`),
    id=paste0("R", id), title=Description, keyword,
    source="Rockefeller", stringsAsFactors = FALSE
  ))
}
