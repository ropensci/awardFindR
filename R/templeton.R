#' Scrape the Templeton grants database from html to a data.frame
#' @param verbose enable verbose HTTP messages. TRUE/FALSE, default: false
#' @return A data.frame
#' @export
#' @examples
#' \dontrun{templeton <- create_templeton_df()}
create_templeton_df <- function(verbose=FALSE) {
  url <- "https://www.templeton.org/grants/grant-database"
  html <- request(url, "get", verbose)
  table <- xml2::xml_find_first(html, "//table[@id='grants-table']/tbody")
  # Capture the table headers
  vars <- xml2::xml_text(xml2::xml_find_all(table, "//thead//th/text()"))
  entries <- xml2::xml_children(table)

  full <- lapply(entries, function(x) {
    info <- xml2::xml_text(xml2::xml_find_all(x, ".//td"))
    data.frame(t(info), stringsAsFactors = FALSE)
  })
  full <- do.call(rbind.data.frame, full)
  names(full) <- vars # Use the captured table headers
  full$link <- xml2::xml_text(xml2::xml_find_all(entries, ".//td/a/@href"))

  full$Featured <- NULL
  # Get rid of all the $ and commas
  full$`Grant Amount` <- gsub("^\\$|,", "", full$`Grant Amount`)

  full
}

#' Search for a set of keywords in the Templeton grants database.
#' @inheritParams get_neh
#' @return A data.frame
#' @export
#' @examples
#' \dontrun{
#' templeton <- get_templeton(c("qualitative data", "case studies"), 2018, 2020)
#' }
get_templeton <- function(keywords, from_year, to_year, verbose=FALSE) {
  url <- "https://www.templeton.org/?limit=500&s="
  links <- lapply(keywords, function(keyword) {
    response <- request(paste0(url, xml2::url_escape(keyword)), "get", verbose)

    link <- xml2::xml_text(
      xml2::xml_find_all(response, "//h2/a[@rel='bookmark']/@href"))

    if (length(link)==0) {
      return(NULL)
    }
    data.frame(keyword, link, stringsAsFactors = FALSE)
  })
  links <- do.call(rbind.data.frame, links)
  if (nrow(links)==0) {
    return(NULL)
  }

  all <- create_templeton_df(verbose)
  vars <- c("year", "id", "title", "pi", "grantee",
            "amount", "area", "region", "link")
  names(all) <- vars

  selected <- merge(links, all, by="link")
  year <- NULL # For R CMD check
  subset(selected, from_year <= year & to_year >= year)
}

.standardize_templeton <- function(keywords, from_date, to_date, verbose) {
  raw <- get_templeton(keywords,
                       format.Date(from_date, "%Y"), format.Date(to_date, "%Y"),
                       verbose)

  if (is.null(raw) || nrow(raw)==0) {
    message("No results from Templeton")
    return(NULL)
  }

  raw <- raw[!duplicated(raw$id), ] # Remove duplicates (only a few in full set)

  with(raw, data.frame(
    institution=grantee, pi, year, start=NA, end=NA, program=area, amount,
    id, title, abstract=NA, keyword, source="Templeton",
    stringsAsFactors = FALSE
  ))

}
