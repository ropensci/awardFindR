#' Scrape the Sloan grants database from html to a data.frame
#'
#' @return A data.frame
#' @export
#' @examples
#' # Make a new version of the sloan data.frame
#' \dontrun{sloan <- sloan_df()}
sloan_df <- function() {
  # This URL will give us all the grants Sloan ever made in a single html return
  # up to 3000 results, there are 2131 as of writing but that parameter can be changed at the end here
  url <- "https://sloan.org/grants-database?dynamic=1&order_by=approved_at&order_by_direction=desc&limit=3000"

  response <- request(url, "get")

  # Everything important is a child of this ul tag
  awards <- xml2::xml_children(xml2::xml_find_first(response, "//ul[@class='data-list']"))

  # The sloan_df_details function does all the magic here
  message("Processing Sloan HTML to data.frame...")
  awards <- lapply(awards, function(entry) {
    regexp <- "(\\n|\\t)"  # These html text nodes are full of ugly newlines and tabs
    grantee <- xml2::xml_text(xml2::xml_find_all(entry, ".//div[@class='grantee']/text()"))
    grantee <- gsub(regexp, "", grantee)
    grantee <- grantee[grantee!=""] # Some text nodes are just all "\n\t\n\n" for example

    amount <- xml2::xml_text(xml2::xml_find_all(entry, ".//div[@class='amount']/text()"))
    amount <- gsub(regexp, "", amount)
    amount <- amount[amount!=""]
    amount <- gsub("^\\$|,", "", amount) # Get rid of all the $ and commas

    year <- xml2::xml_text(xml2::xml_find_all(entry, ".//div[@class='year']/text()"))
    year <- gsub(regexp, "", year)
    year <- year[year!=""]
    # One recent case with no year entry was causing problems
    if (length(year)==0) year <- NA else year <- as.integer(year)

    # Sloan program, subprogram and PI name are in this unstructured list section
    categories <- xml2::xml_text(xml2::xml_find_all(entry, ".//ul[@class='col']/li/text()"))
    categories <- gsub(regexp, "", categories)
    categories <- categories[categories!=""]
    # No point keeping an entry with no info here? Was messing everything up and only excludes 3 cases
    if (length(categories)==0) return(NULL)
    # I expect the first element in "categories" to be the program and the last to be the PI name.
    # Trying to get more specific than this is bound to cause problems since the numbers of nodes differ
    # This is mainly a problem when trying to specifically pick out the sub-program, so I omitted

    description <- xml2::xml_text(xml2::xml_find_first(entry, ".//div[@class='brief-description']"))
    description <- gsub(regexp, "", description)

    # These ID numbers are derived from the permalink. They work in the Sloan grants search
    id <- xml2::xml_find_first(entry, ".//footer/a[@class='permalink']")
    id <- xml2::xml_attr(id, "href")
    id <- gsub("/grant-detail/" , "", id)
    id <- as.integer(id)

    # Assemble it into a data.frame
    data.frame(grantee, pi=categories[length(categories)],
               year, program=categories[1], amount=as.integer(amount),
               id, description,
               stringsAsFactors = FALSE)
  })
  do.call(rbind.data.frame, awards)
}

#' Search for a set of keywords in the Sloan grants database.
#' @param keywords vector of keywords to query
#' @param from_year Beginning year to search
#' @param to_year Ending year to search
#' @return A data.frame
#' @export
#' @examples
#' \dontrun{sloan <- sloan_get(c("qualitative data", "case studies"), 2018, 2020)}
sloan_get <- function(keywords, from_year, to_year) {
  results <- lapply(keywords, function(keyword, df, from, to) {
    year <- NULL # For R CMD check
    df <- subset(df, year >= from & year <= to)

    # grep the keyword in the description, subset to the hits
    hits <- grepl(keyword, df$description, ignore.case=TRUE)
    hits <- df[hits, ]

    # Empty results?
    if (nrow(hits)==0) {
      message(paste("No Sloan results for:", keyword))
      return(NULL)
    }

    hits$keyword <- keyword
    return(hits)
  },
  sloan, from_year, to_year) # the lapply function arguments here
  results <- do.call(rbind.data.frame, results)
  if (nrow(results)==0) return(NULL)
  return(results)
}

#' Standardize Sloan search
#' @param keywords Vector of keywords to search
#' @param from_date Beginning date object to search
#' @param to_date Ending date object to search
#' @return a standardized data.frame
sloan_standardize <- function(keywords, from_date, to_date) {
  raw <- sloan_get(keywords,
                     format.Date(from_date, "%Y"), format.Date(to_date, "%Y"))
  if (is.null(raw)) return(NULL)

  with(raw, data.frame(
    institution=grantee, pi, year, start=NA, end=NA, program, amount, id,
    title=description, keyword, source="Sloan", stringsAsFactors = FALSE
  ))
}
