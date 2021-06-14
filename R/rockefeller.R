#' Search Rockefeller Foundation grants
#' @inheritParams get_nih
#' @return a data.frame
#' @export
#' @examples
#' \dontrun{
#' rockefeller <- get_rockefeller("test", "2012-01-01", "2021-01-01")
#' }
get_rockefeller <- function(keyword, from_date, to_date, verbose) {
  url <- paste0("https://www.rockefellerfoundation.org/?post_type=grant&",
  "keyword=", xml2::url_escape(keyword),
  "&from_month=", format.Date(from_date, "%m"),
  "&from_year=", format.Date(from_date, "%Y"),
  "&to_month=", format.Date(to_date, "%m"),
  "&to_year=", format.Date(to_date, "%Y"),
  "&download=filter")

  response <- as.data.frame(request(url, "get", verbose))
  if (nrow(response)==0) {
    return(NULL) # No results?
  }

  response$keyword <- keyword
  response$id <- vapply(response$Description, .text_hash, 1)

  response
}

.standardize_rockefeller <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, get_rockefeller, from_date, to_date, verbose)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    message("No results from Rockefeller")
    return(NULL)
  }

  with(raw, data.frame(
    institution=Title, pi=NA, year=format.Date(`Grant Term Start`, "%Y"),
    start=as.character(`Grant Term Start`), end=as.character(`Grant Term End`),
    program=Initiative, amount=as.integer(`Grant Amount`),
    id=paste0("R", id), title=Description, abstract=NA, keyword,
    source="Rockefeller", stringsAsFactors = FALSE
  ))
}
