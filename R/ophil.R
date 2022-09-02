#' Grab the Open Philanthropy grants data search for keyword-date combos
#' @inheritParams get_neh
#' @return A data.frame
#' @export
#' @examples ophil <- get_ophil("qualitative", 2019, 2020)
get_ophil <- function(keyword, from_year, to_year, verbose=FALSE) {
  base_url <- "https://www.openphilanthropy.org/grants?"
  query_url <- paste0(base_url,
                      "q=\"", xml2::url_escape(keyword), "\"&yr=",
                      paste(from_year:to_year, collapse="&yr="))

  response <- request(query_url, "get", verbose)

  title <- response %>% rvest::html_elements(".block-feed-post__title > a") %>%
    rvest::html_text()

  if (length(title)==0) {
    return(NULL)
  }

  dates <- response %>% rvest::html_elements(".block-feed-post__date") %>%
    rvest::html_text2()

  year <- unlist(lapply(strsplit(dates, " "), function(x) { x[2] }))

  organization <- response %>% rvest::html_elements(".block-feed-post__organization-name > a") %>%
    rvest::html_text()

  program <- response %>% rvest::html_elements(".block-feed-post__focus-area > a") %>%
    rvest::html_text()

  amount <- response %>% rvest::html_elements(".block-feed-post__grant-amount") %>%
    rvest::html_text()
  amount <- gsub('\\$', '', amount)
  amount <- gsub(',', '', amount)

  link <- response %>% rvest::html_elements(".block-feed-post__link > a") %>%
    rvest::html_attr("href")

  data.frame(organization, title, program, amount, year, link, keyword)
}

.standardize_ophil <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, get_ophil,
                format.Date(from_date, "%Y"), format.Date(to_date, "%Y"),
                verbose)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    message("No results from Open Philanthropy")
    return(NULL)
  }

  with(raw, data.frame(
    institution=organization, pi=NA,
    year, start=NA, end=NA, program,
    amount=as.integer(amount), id=link,
    title, abstract=NA, keyword, source="Open Philanthropy",
    stringsAsFactors = FALSE
  ))
}
