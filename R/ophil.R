#' Grab the Open Philanthropy grants data search for keyword-date combos
#' @param keyword Keyword to search for in description, single string
#' @param from_year Beginning year to search
#' @param to_year Ending year to search
#' @return A data.frame
#' @export
#' @examples ophil <- ophil_get("qualitative", 2019, 2020)
ophil_get <- function(keyword, from_year, to_year) {
  base_url <- "https://www.openphilanthropy.org/giving/grants?"
  query_url <- paste0(base_url,
                      "keys=\"", xml2::url_escape(keyword), "\"")

  response <- request(query_url, "get")

  # Everything important is a child of this table's tbody
  results <- xml2::xml_children(
    xml2::xml_find_first(response, "//div[@class='view-content']/table/tbody"))

  if (length(results)==0) {
    return(NULL)  # No results?
  }

  # Loop through each entry and extract details
  df <- lapply(results, function(x) {
    fields <- xml2::xml_children(x)
    title <- xml2::xml_text(xml2::xml_find_all(fields[1], ".//a/text()"))
    id <- xml2::xml_text(xml2::xml_find_all(fields[1], ".//a/@href"))

    grantee <- xml2::xml_text(xml2::xml_find_all(fields[2], ".//a/text()"))
    program <- xml2::xml_text(xml2::xml_find_all(fields[3], ".//a/text()"))

    amount <- xml2::xml_text(fields[4])
    # Remove trailing and leading whitespace
    amount <- gsub("^\\s+|\\s+$", "", amount)
    # Remove $ and , in amounts (i.e. $1,000,000)
    amount <- gsub("^\\$|,", "", amount)

    date <- xml2::xml_text(xml2::xml_find_all(fields[5], ".//span/text()"))

    data.frame(grantee, month=date, program,
               amount=as.integer(amount), title, id,
               stringsAsFactors = FALSE)
  })
  df <- do.call(rbind.data.frame, df)

  # Subset to the years passed in the arguments
  df$year <- as.integer(.substr_right(df$month, 4))
  year <- NULL # For R CMD check
  df <- subset(df, year >= from_year & year <= to_year)
  if (nrow(df)==0) {
    return(NULL) # No results in the date range?
  }

  df$keyword <- keyword

  df
}

.ophil_standardize <- function(keywords, from_date, to_date) {
  raw <- lapply(keywords, ophil_get,
                format.Date(from_date, "%Y"), format.Date(to_date, "%Y"))
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    return(NULL)
  }

  with(raw, data.frame(
    institution=grantee, pi=NA, year, start=NA, end=NA, program, amount, id,
    title, abstract=NA, keyword, source="Open Philanthropy",
    stringsAsFactors = FALSE
  ))
}
