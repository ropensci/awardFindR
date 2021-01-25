#' Extract details from a list in the Sloan grants database
#'
#' Internal function to extract text details in the Sloan routine.
#' Passed internally by sloan_df in a loop. Not intended to be run directly.
#'
#' @param entry an xml2 node from a nodelist.
#' @return a data.frame with a single entry
sloan_df_details <- function(entry) {
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
  if (length(year)==0) { # One recent case with no year entry was causing problems
    year <- NA
  } else {
    year <- as.integer(year)
  }

  # Sloan program, subprogram and PI name are in this unstructured list section
  categories <- xml2::xml_text(xml2::xml_find_all(entry, ".//ul[@class='col']/li/text()"))
  categories <- gsub(regexp, "", categories)
  categories <- categories[categories!=""]
  # No point keeping an entry with no info here? Was messing everything up and only excludes 3 cases
  if (length(categories)==0) {
    return(NULL)
  }
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
  row <- data.frame(grantee=grantee,
                    pi=categories[length(categories)],
                    year=year,
                    program=categories[1],
                    amount=as.integer(amount),
                    id=id,
                    description=description,
                    stringsAsFactors = FALSE)
  return(row)
}

#' Scrape the Sloan grants database from html to a data.frame
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' \dontrun{sloan <- sloan_df()}
sloan_df <- function() {
  # This URL will give us all the grants Sloan ever made in a single html return
  # up to 3000 results, there are 2131 as of writing but that parameter can be changed at the end here
  url <- "https://sloan.org/grants-database?dynamic=1&order_by=approved_at&order_by_direction=desc&limit=3000"

  message(paste("GET", url))
  response <- xml2::read_html(url)

  # Everything important is a child of this ul tag
  awards <- xml2::xml_children(xml2::xml_find_first(response, "//ul[@class='data-list']"))

  # The sloan_df_details function does all the magic here
  message("Processing Sloan HTML to data.frame...")
  awards <- lapply(awards, sloan_df_details)
  df <- do.call(rbind.data.frame, awards)
  return(df)
}

#' Query the Sloan grant data.frame for a keyword
#'
#' For internal use only
#'
#' @param query A single string keyword
#' @param df A data.frame passed by sloan_df
#' @param from Beginning year to search (integer)
#' @param to Ending year to search (integer)
#' @return A data.frame with the relevant results matching the keyword
sloan_get_query <- function(query, df, from, to) {
  df <- subset(df, year >= from & year <= to)

  # grep the query in the description, subset to the hits
  hits <- grepl(query, df$description,
                ignore.case=TRUE)
  hits <- df[hits, ]

  # Empty results?
  if (nrow(hits)==0) {
    message(paste("NOTICE (non-fatal) no Sloan results for:", query))
    return(NULL)
  }

  hits$query <- query
  return(hits)
}

#' Search for a set of keywords in the Sloan grants database.
#'
#' Scrape, format and search the Sloan grants database for a series of keywords
#'
#' @param queries vector of keywords to query
#' @param from Beginning year to search
#' @param to Ending year to search
#' @return A data.frame
#' @export
#' @examples
#' \dontrun{sloan <- sloan_get(c("qualitative data", "case studies"), 2018, 2020)}
sloan_get <- function(queries, from, to) {
  results <- lapply(queries, sloan_get_query, sloan_df(), from, to)
  results <- do.call(rbind.data.frame, results)
  if (length(results)==0) {
    return(NULL)
  }
  return(results)
}
