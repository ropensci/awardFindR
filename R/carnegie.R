#' Search Carnegie awards
#' @inheritParams get_ssrc
#' @return a data.frame
#' @export
#' @examples
#' \dontrun{
#' carnegie <- get_carnegie("qualitative data", 2016, 2017)
#' }
get_carnegie <- function(keyword, from_year, to_year, verbose=FALSE) {
  base_url <- "https://www.carnegie.org/grants/grants-database/"
  query <- paste0("?q=", xml2::url_escape(keyword), "&per_page=100")
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
  num_awards <- length(rvest::html_nodes(response, "tbody > tr"))
  if (num_awards == 0)  {
    return(NULL) # No awards in the table?
  }


  more_to_scrape <- TRUE
  page <- 1

  # Create vectore to store awards in, which we'll iteratively add each page's results to
  all_awards <- c()

  while (more_to_scrape) {
    # Create url for this page of results
    page_url <- paste0(url, "&page=", page)

    # Get new page
    response <- httr::GET(page_url)

    # Raises statuses, if verbose
    if (verbose==TRUE) {
      httr::message_for_status(response)
      message()
    }

    # Get response
    response <- httr::content(response)

    # Get awards for new page
    awards <- rvest::html_nodes(response, "tbody > tr")

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
        name=rvest::html_text(
          rvest::html_elements(details, "label.grant-detail--label")), #  ".//label[@class='grant-detail--label']")),

        value=rvest::html_text(
          rvest::html_elements(details, "div.grant-detail--text")), # .//div[@class='grant-detail--text']")),

        stringsAsFactors = FALSE)

      # Pull the data we're looking for
      title <- details$value[details$name=="Project Title"]
      date <- details$value[details$name=="Date"]

      description <- details$value[details$name=="Description"]
      if (length(description)==0) description <- NA

      # Merge the data from the search page and the specific result page
      data.frame(grantee=info[2], date, amount, program=info[4], id, title,
                 year=info[1], abstract=description,
                 keyword, stringsAsFactors = FALSE)
    })

    all_awards <- append(all_awards, awards)

    # Check if there is a "next" button on the page, indicating that there's an
    # an additional page of results to visit
    more_to_scrape <- length(rvest::html_nodes(response, xpath = "//text()[contains(., 'Next')]/..")) > 0
    page <- page + 1

  }

  do.call(rbind.data.frame, all_awards)
}

.standardize_carnegie <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, get_carnegie,
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
