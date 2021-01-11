#' Capitalize string to Title Case
#'
#' @param string a character string of ANY CaSe
#' @return A Title Case Character String
#' @examples
#' # Should output "Foo Bar"
#' \dontrun{title_case("foo BAR")}
title_case <- function(string) {
  c <- strsplit(tolower(string), " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

#' xml2 list to data.frame
#'
#' Turns the result of an xml2::as_list() function into a data.frame.
#' Employs maximalist merging of columns, adding NAs to any missing values.
#' One way of replacing the XML::xmlToDataFrame function that no longer exists in xml2
#'
#' @param xml2list List object, typically the result of xml2::as_list() function
#' @return A data.frame
#' @examples
#' \dontrun{df <- xml2list_to_df(xml2list=xml2::as_list(xml))}
xml2list_to_df <- function(xml2list) {
  xml2list <- lapply(xml2list, unlist, recursive=F)
  xml2list <- lapply(xml2list, as.data.frame)

  # Find all unique column names
  fields <- unique(unlist(lapply(xml2list, names)))

  # Bind the lists into one data.frame and handle missing data
  df <- do.call(rbind, lapply(xml2list, function(x) {
    data.frame(c(x, sapply(setdiff(fields, names(x)),
                           function(y) NA)),
               stringsAsFactors = F)
  }))

  return(df)
}

#' Request XML specifically
#'
#' Sets HTTP Accept header to application/xml.
#' This prevents some APIs from returning json by default
#'
#' @param url URL to query
#' @return The HTTP request response, hopefully in XML
#' @examples
#' \dontrun{request_xml("http://someapi.org/api.xml?parameters=foo")}
request_xml <- function(url) {
  message(paste("Grabbing url:", url))
  httr::with_config(httr::add_headers(Accept = "application/xml"), {
    response <- httr::GET(url)
  })

  # HTML entities (like "&#x19;") cause problems for the xml parser because of the ;
  # So we're just removing them if they exist (extremely rare)
  html_regexp <- "&#[a-z]+[0-9]+;"
  if (grepl(html_regexp, httr::content(response, type="text", encoding="UTF-8"))) {
    response <- xml2::read_xml(gsub(html_regexp, "", content(response, type="text")),
                               encoding="UTF-8")
  } else {
    response <- xml2::read_xml(response$content, encoding="UTF-8")
  }
  rm(html_regexp)

  return(response)
}
