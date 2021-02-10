#' Search the Andrew W. Mellon Foundation grant database
#' @param keyword Keyword to query
#' @param from_year Year to begin search
#' @param to_year Year to end search
#' @return a data.frame
#' @export
#' @examples
#' mellon <- mellon_get("qualitative", 2013, 2021)
mellon_get <- function(keyword, from_year, to_year) {
  base_url <- "https://mellon.org/grants/grants-database/advanced-search/?"
  query_url <- paste0(base_url,
                      "&year-start=", from_year, "&year-end=", to_year,
                      "&q=", xml2::url_escape(keyword),
                      # Total grants are 17437 as of writing,
                      # and this figure below seems to be arbitrarily flexible
                      "&per_page=5000")

  response <- request(query_url, "get")

  results <- xml2::xml_children(xml2::xml_find_first(response,
                                                     "//table[@class='grant-list']/tbody"))
  # Loop through each entry
  df <- lapply(results, function(entry) {
    id <- xml2::xml_text(xml2::xml_find_all(xml2::xml_children(entry)[2],
                                            ".//a/@href"))
    text <- lapply(xml2::xml_children(entry), xml2::xml_text)
    fields <- c("institution", "description",
                "date", "amount",
                "location", "program")
    df <- as.data.frame(text, stringsAsFactors=F)
    names(df) <- fields
    df$id <- id

    return(df)
  })
  df <- do.call(rbind.data.frame, df)
  if (nrow(df)==0) return(NULL) # No results?

  df$amount <- gsub("^\\$|,", "", df$amount) # Remove $ and , in amounts (i.e. $1,000,000)
  df$amount <- as.integer(df$amount)

  df$date <- as.Date(df$date, format="%m/%d/%y")

  return(df)
}

#' Standardize award results from Mellon
#' @param keyword Keyword to query
#' @param from_year Year to begin search
#' @param to_year Year to end search
#' @return a standardized data.frame
mellon_standardize <- function(keyword, from_year, to_year) {
  mellon <- mellon_get(keyword, from_year, to_year)
  if (is.null(mellon)) return(NULL)
  with(mellon, data.frame(
    institution, pi=NA, year=format.Date(date, "%Y"), start=NA, end=NA,
    program, amount, id, title=description, keyword, source="Mellon",
    stringsAsFactors = FALSE
  ))
}
