#' @importFrom rvest %>%
NULL

#' Search for a set of keywords in the Sloan grants database.
#' @inheritParams get_neh
#' @param grantee Use the web-based search function instead of an internal grep(), ideal for searching names
#' @return A data.frame
#' @export
#' @examples
#'
#' \dontrun{
#' sloan <- get_sloan("case studies", 2018, 2020)
#' }
get_sloan <- function(keyword, from_year, to_year,
                      verbose=FALSE, grantee=FALSE) {

  if (grantee == TRUE) {
    url <- paste0("https://sloan.org/grants-database?keywords=", xml2::url_escape(keyword),
                  "&dynamic=1&order_by=approved_at&order_by_direction=desc&limit=3000")
  } else {
    url <- paste0("https://sloan.org/grants-database",
                  "?dynamic=1&order_by=approved_at&order_by_direction=desc&limit=3000")

  }

  response <- request(url, "get", verbose)

  descriptions <- rvest::html_text(
    rvest::html_nodes(response, "div.brief-description"), trim=TRUE)

  # this is the search function, unless we're using the web search
  if (grantee==FALSE) {
    hits <- grepl(keyword, descriptions, ignore.case=TRUE)
    if (!any(hits)) {
      return(NULL) # No results
    }
  } else {
    hits <- rep(TRUE, length(descriptions))
  }

  description <- descriptions[hits]

  results <- rvest::html_node(response, "ul.data-list")
  results <- rvest::html_children(results)[hits]

  if (length(results)==0) {
    return(NULL)
  }

  grantee <- rvest::html_nodes(results, "div.grantee") %>%
    rvest::html_nodes(xpath="./text()[normalize-space()]") %>%
    rvest::html_text(trim=TRUE)

  amount <- rvest::html_nodes(results, "div.amount") %>%
    rvest::html_nodes(xpath="./text()[normalize-space()]") %>%
    rvest::html_text(trim=TRUE)
  amount <- gsub("^\\$|,", "", amount) # Get rid of all the $ and commas

  year <- rvest::html_nodes(results, "div.year") %>%
    rvest::html_text(trim=TRUE) %>% .substr_right(4)
  year <- as.integer(year)

  id <- rvest::html_nodes(results, "footer > a.permalink") %>%
    rvest::html_attr("href")
  id <- gsub("/grant-detail/" , "", id)

  df <- data.frame(grantee, amount, year, id, description, keyword,
                   stringsAsFactors = FALSE) # begin assemble

  # These extra tags are a hassle to extract because they're not labeled
  # Gotta go one by one
  extra <- lapply(results, function(entry) {
    extra <- rvest::html_nodes(entry, "ul.col > li") %>%
      rvest::html_nodes(xpath="./text()[normalize-space()]") %>%
      rvest::html_text(trim=TRUE)
    if (length(extra)==0) {
      data.frame(pi=NA, program=NA)
    }
    data.frame(pi=extra[length(extra)], program=extra[1],
               stringsAsFactors = FALSE)
  })
  extra <- do.call(rbind.data.frame, extra)

  df <- cbind(df, extra) # End assemble
  year <- NULL

  df <- subset(df, year >= from_year, year <= to_year)

  df

}

.standardize_sloan <- function(keywords, from_date, to_date,
                               verbose, grantee=FALSE) {
  raw <- lapply(keywords, get_sloan,
                as.integer(format.Date(from_date, "%Y")),
                as.integer(format.Date(to_date, "%Y")),
                verbose, grantee)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    message("No results from Sloan")
    return(NULL)
  }

  with(raw, data.frame(
    institution=grantee, pi, year, start=NA, end=NA, program, amount, id,
    title=description, abstract=NA,
    keyword, source="Sloan", stringsAsFactors = FALSE
  ))
}
