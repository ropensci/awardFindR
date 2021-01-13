#' Extract details from a list in the ophil grants database
#'
#' Passed internally by ophil_get in a loop. Not intended to be run directly.
#'
#' @param entry an xml2 node from a nodelist.
#' @return a data.frame with a single entry
ophil_details <- function(entry) {
  title <- xml2::xml_text(xml2::xml_find_all(xml2::xml_children(entry)[1], ".//a/text()"))
  grantee <- xml2::xml_text(xml2::xml_find_all(xml2::xml_children(entry)[2], ".//a/text()"))
  program <- xml2::xml_text(xml2::xml_find_all(xml2::xml_children(entry)[3], ".//a/text()"))
  amount <- xml2::xml_text(xml2::xml_children(entry)[4])
  date <- xml2::xml_text(xml2::xml_find_all(xml2::xml_children(entry)[5], ".//span/text()"))

  # Assemble it into a data.frame
  row <- data.frame(grantee=grantee,
                    month=date,
                    focus=program,
                    amount=amount,
                    title=title,
                    stringsAsFactors = FALSE)
  return(row)
}

#' Grab the Open Philanthropy grants data search for keyword-date combos
#'
#' @param keyword Keyword to search for in the project description, single string
#' @param from Beginning year to search
#' @param to Ending year to search
#'
#' @return A data.frame
#' @export
#'
#' @example
#' ophil <- ophil_get("qualitative", 2019, 2020)
ophil_get <- function(keyword, from, to) {
  base_url <- "https://www.openphilanthropy.org/giving/grants?"
  query_url <- paste0(base_url,
                      "keys=\"", xml2::url_escape(keyword), "\"")

  message(paste("Grabbing url:", query_url))
  response <- xml2::read_html(query_url)

  results <- xml2::xml_children(xml2::xml_find_first(response,
                                                     "//div[@class='view-content']/table/tbody"))
  # No results?
  if (length(results)==0) {
    return(NULL)
  }

  df <- lapply(results, ophil_details)
  df <- do.call(rbind.data.frame, df)

  df$year <- as.integer(substr_right(df$month, 4))
  df <- subset(df, year >= from & year <= to)
  # No results in the date range?
  if (nrow(df)==0) {
    return(NULL)
  }

  return(df)
}

#ophil_csv <- function(keywords, from, to) {
#  url <- "https://www.openphilanthropy.org/giving/grants/spreadsheet"
#  message(paste("Grabbing url:", url))
#  ophil <- read.csv(url,
#                  na.strings = c("NA", "NULL", "Unknown"),
#                  stringsAsFactors = FALSE,
#                  fileEncoding = "UTF-8-BOM")

#}
