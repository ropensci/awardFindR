#' Extract details from a list in the ophil grants database
#'
#' Passed internally by ophil_get in a loop. Not intended to be run directly.
#'
#' @param entry an xml2 node from a nodelist.
#' @return a data.frame with a single entry
ophil_details <- function(entry) {
  fields <- xml2::xml_children(entry)
  title <- xml2::xml_text(xml2::xml_find_all(fields[1], ".//a/text()"))
  id <- xml2::xml_text(xml2::xml_find_all(fields[1], ".//a/@href"))

  grantee <- xml2::xml_text(xml2::xml_find_all(fields[2], ".//a/text()"))
  program <- xml2::xml_text(xml2::xml_find_all(fields[3], ".//a/text()"))

  amount <- xml2::xml_text(fields[4])
  amount <- gsub("^\\s+|\\s+$", "", amount)   # Remove trailing and leading whitespace
  amount <- gsub("^\\$|,", "", amount) # Remove $ and , in amounts (i.e. $1,000,000)

  date <- xml2::xml_text(xml2::xml_find_all(fields[5], ".//span/text()"))

  # Assemble it into a data.frame
  row <- data.frame(grantee=grantee,
                    month=date,
                    focus=program,
                    amount=as.integer(amount),
                    title=title,
                    id=id,
                    stringsAsFactors = FALSE)
  return(row)
}

#' Grab the Open Philanthropy grants data search for keyword-date combos
#'
#' @param query Keyword to search for in the project description, single string
#' @param from Beginning year to search
#' @param to Ending year to search
#'
#' @return A data.frame
#' @export
#'
#' @example
#' ophil <- ophil_get("qualitative", 2019, 2020)
ophil_get <- function(query, from, to) {
  base_url <- "https://www.openphilanthropy.org/giving/grants?"
  query_url <- paste0(base_url,
                      "keys=\"", xml2::url_escape(query), "\"")

  message(paste("GET", query_url))
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
