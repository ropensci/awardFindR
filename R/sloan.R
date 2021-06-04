#' @importFrom rvest %>%
NULL

#' Search for a set of keywords in the Sloan grants database.
#' @inheritParams fedreporter_get
#' @return A data.frame
#' @export
#' @examples
#'
#' \dontrun{
#' sloan <- sloan_get(c("qualitative data", "case studies"), 2018, 2020)
#' }
sloan_get <- function(keyword, from_year, to_year, verbose=FALSE) {
  url <- paste0("https://sloan.org/grants-database",
  "?dynamic=1&order_by=approved_at&order_by_direction=desc&limit=3000")

  response <- request(url, "get", verbose)

  descriptions <- rvest::html_text(
    rvest::html_nodes(response, "div.brief-description"), trim=TRUE)

  # this is the search function
  hits <- grepl(keyword, descriptions, ignore.case=TRUE)
  if (!any(hits)) {
    return(NULL) # No results
  }

  description <- descriptions[hits]

  results <- rvest::html_node(response, "ul.data-list")
  results <- rvest::html_children(results)[hits]

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
    data.frame(pi=extra[length(extra)], program=extra[1],
               stringsAsFactors = FALSE)
  })
  extra <- do.call(rbind.data.frame, extra)

  df <- cbind(df, extra) # End assemble
  year <- NULL

  subset(df, year >= from_year, year <= to_year)
}

.sloan_standardize <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, sloan_get,
                as.integer(format.Date(from_date, "%Y")),
                as.integer(format.Date(to_date, "%Y")),
                verbose)
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
