#' Parse the fields of an entry in the Mellon grant database
#'
#' Internal use only
#'
#' @param entry XML element
#'
#' @return a single row data.frame
mellon_get_details <- function(entry) {
  id <- xml2::xml_text(xml2::xml_find_all(xml2::xml_children(entry)[2], ".//a/@href"))
  text <- lapply(xml2::xml_children(entry), xml2::xml_text)
  fields <- c("institution", "description",
              "date", "amount",
              "location", "program")
  df <- as.data.frame(text)
  names(df) <- fields
  df$id <- id

  return(df)
}

#' Query the Andrew W. Mellon Foundation grant database
#'
#' @param keyword Keyword to query
#' @param from Year to begin search
#' @param to Year to end search
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' mellon <- mellon_get("qualitative", 2013, 2021)
mellon_get <- function(keyword, from, to) {
  base_url <- "https://mellon.org/grants/grants-database/advanced-search/?"
  query_url <- paste0(base_url,
                      "&year-start=", from, "&year-end=", to,
                      "&q=", xml2::url_escape(keyword),
                      # Total grants are 17437 as of writing,
                      # and this figure below seems to be arbitrarily flexible
                      "&per_page=5000")

  message(paste("GET", query_url))
  response <- xml2::read_html(query_url)

  results <- xml2::xml_children(xml2::xml_find_first(response,
                                                     "//table[@class='grant-list']/tbody"))

  df <- lapply(results, mellon_get_details)
  df <- do.call(rbind.data.frame, df)
  if (nrow(df)==0) return(NULL) # No results?

  df$amount <- gsub("^\\$|,", "", df$amount) # Remove $ and , in amounts (i.e. $1,000,000)

  return(df)
}
