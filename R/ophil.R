#' Grab the Open Philanthropy grants data search for keyword-date combos
#' @inheritParams get_ssrc
#' @return A data.frame
#' @export
#' @examples ophil <- get_ophil("qualitative", 2019, 2020)
get_ophil <- function(keyword, from_year, to_year, verbose=FALSE) {
  base_url <- "https://www.openphilanthropy.org/grants?"
  query_url <- paste0(base_url,
                      "q=", xml2::url_escape(keyword), "&yr=",
                      paste(from_year:to_year, collapse="&yr="))

  response <- request(query_url, "get", verbose)

  # each one of these divs is a result
  blocks <- response %>% rvest::html_elements("div.block-feed-post")

  # check for no results
  if (length(blocks)==0) {
    return(NULL)
  }
  # loop through each award and extract relevant info
  awards <- lapply(blocks, function(x) {
    title <- x %>% rvest::html_element("div.block-feed-post__body > .block-feed-post__title > a") %>% rvest::html_text()
    date <- x %>% rvest::html_element("div.block-feed-post__body > .block-feed-post__date") %>% rvest::html_text()
    year <- unlist(lapply(strsplit(date, " "), function(x) { x[2] }))
    prog <- x %>% rvest::html_element("div.block-feed-post__body > .block-feed-post__focus-area > a") %>% rvest::html_text()
    org <- x %>% rvest::html_element("div.block-feed-post__body > .block-feed-post__organization-name > a") %>% rvest::html_text()
    amount <- x %>% rvest::html_element("div.block-feed-post__body > .block-feed-post__grant-amount") %>% rvest::html_text()
    link <- x %>% rvest::html_element("div.block-feed-post__head > a") %>% rvest::html_attr("href")


    data.frame(organization=org, title, program=prog, amount, year, link)
  }); awards <- do.call(rbind.data.frame, awards)
  awards$keyword <- keyword

  awards$amount <- gsub('\\$', '', awards$amount)
  awards$amount <- gsub(',', '', awards$amount)

  awards
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
