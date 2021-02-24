#' @importFrom rvest %>%
NULL

#' Search for a set of keywords in the Sloan grants database.
#' @param keyword vector of keywords to query
#' @param from_year Beginning year to search
#' @param to_year Ending year to search
#' @return A data.frame
#' @export
#' @examples
#'
#' \dontrun{sloan <- sloan_get(c("qualitative data", "case studies"), 2018, 2020)}
sloan_get <- function(keyword, from_year, to_year) {
  url <- "https://sloan.org/grants-database?dynamic=1&order_by=approved_at&order_by_direction=desc&limit=3000"
  response <- request(url, "get")

  descriptions <- rvest::html_text(rvest::html_nodes(response, "div.brief-description"),
                                   trim=TRUE)
  hits <- grepl(keyword, descriptions, ignore.case=TRUE)  # this is the search function
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
    rvest::html_text(trim=TRUE) %>% substr_right(4)
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

#' Standardize Sloan search
#' @param keywords Vector of keywords to search
#' @param from_date Beginning date object to search
#' @param to_date Ending date object to search
#' @return a standardized data.frame
sloan_standardize <- function(keywords, from_date, to_date) {
  raw <- lapply(keywords, sloan_get,
                as.integer(format.Date(from_date, "%Y")),
                as.integer(format.Date(to_date, "%Y")))
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    return(NULL)
  }

  with(raw, data.frame(
    institution=grantee, pi, year, start=NA, end=NA, program, amount, id,
    title=description, keyword, source="Sloan", stringsAsFactors = FALSE
  ))
}
