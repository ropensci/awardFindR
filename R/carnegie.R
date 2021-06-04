#' Search Carnegie awards
#' @inheritParams fedreporter_get
#' @return a data.frame
#' @export
#' @examples
#' \dontrun{
#' carnegie <- carnegie_get("qualitative data", 2016, 2017)
#' }
carnegie_get <- function(keyword, from_year, to_year, verbose=FALSE) {
  base_url <- "https://www.carnegie.org/grants/grants-database/"
  query <- paste0("?q=", xml2::url_escape(keyword), "&per_page=104")
  url <- paste0(base_url, query)
  for (n in from_year:to_year) url <- paste0(url, "&y=", n)

  if (verbose==TRUE) message(paste("GET", url, "... "), appendLF=FALSE)
  response <- httr::GET(url)
  if (verbose==TRUE) {
    httr::message_for_status(response)
    message()
  }

  cookie <- httr::cookies(response)$value # Need this cookie for CSRF validation
  response <- httr::content(response)
  awards <- rvest::html_nodes(response, "tbody > tr")
  if (length(awards)==0)  {
    return(NULL) # No awards in the table?
  }

  awards <- lapply(awards, function(x) {
    id <- gsub("^grant-", "", rvest::html_attr(x, "id"))
    info <- rvest::html_nodes(x, "td") %>% rvest::html_text()
    # Remove $ and , in amounts (i.e. $1,000,000)
    amount <- as.integer(gsub("^\\$|,", "", info[3]))

    # Extra details require another HTTP request (per individual award, ugh)
    # Also, Carnegie now uses CSRF tokens, so we have to include those headers
    url <- paste0(base_url, "grant/", id, "/")
    if(verbose==TRUE) message(paste("GET", url, "... "), appendLF=FALSE)
    response <- httr::GET(url, config=httr::add_headers(
                      Referer=paste0(base_url,
                                     "/grantee/knowledgeworks-foundation/"),
                      `X-CSRFToken`=cookie,
                      `X-Requested-With`="XMLHttpRequest"),
                    httr::set_cookies(csfrtoken=cookie), httr::accept_json())
    if (verbose==TRUE) {
      httr::message_for_status(response)
      message()
    }

    details <- httr::content(response)
    details <- xml2::read_html(details$result[1])

    # Turn the div table into a real table
    details <- data.frame(
      name=xml2::xml_text(
        xml2::xml_find_all(details, ".//strong[@class='grant-detail--label']")),

      value=xml2::xml_text(
        xml2::xml_find_all(details, ".//div[@class='grant-detail--text']")),

      stringsAsFactors = FALSE)

    title <- details$value[details$name=="Project Title"]
    date <- details$value[details$name=="Date"]
    description <- details$value[details$name=="Description"]
    data.frame(grantee=info[2], date, amount, program=info[4], id, title,
               year=info[1], abstract=description,
               keyword, stringsAsFactors = FALSE)
  })

  do.call(rbind.data.frame, awards)
}

.carnegie_standardize <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, carnegie_get,
                     format.Date(from_date, "%Y"), format.Date(to_date, "%Y"),
                verbose)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    message("No results from Carnegie")
    return(NULL)
  }

  with(raw, data.frame(
    institution=grantee, pi=NA, year, start=NA, end=NA,
    program, amount, id, title, abstract, keyword, source="Carnegie",
    stringsAsFactors = FALSE
  ))
}
