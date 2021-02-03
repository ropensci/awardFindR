#' Get Carnegie awards
#'
#' @param keyword Keyword to query, single string
#' @param from_year Year to begin search, integer
#' @param to_year Year to end search, integer
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' carnegie <- carnegie_get("qualitative data", 2016, 2017)
carnegie_get <- function(keyword, from_year, to_year) {
  base_url <- "https://www.carnegie.org/grants/grants-database/"
  query <- paste0("?q=", xml2::url_escape(keyword), "&per_page=104")
  url <- paste0(base_url, query)
  for (n in from_year:to_year) url <- paste0(url, "&y=", n)

  response <- request(url, "get")
  awards <- xml2::xml_find_all(response, "//tbody/tr[@data-url]")
  if (length(awards)==0) return(NULL) # No awards in the table?

  awards <- lapply(awards, function(x) {
    id <- gsub("^grant-", "",
               xml2::xml_text(xml2::xml_find_first(x, ".//@id")))
    info <- xml2::xml_find_all(x, ".//td")
    info <- sapply(info, xml2::xml_text)
    # Remove $ and , in amounts (i.e. $1,000,000)
    amount <- as.integer(gsub("^\\$|,", "", info[3]))

    # Extra details require another HTTP request (per individual award, ugh)
    details <- request(paste0(base_url, "grant/", id, "/"), "get")
    details <- xml2::read_html(details$result)

    # Turn the div table into a real table
    details <- data.frame(name=xml2::xml_text(xml2::xml_find_all(details, ".//strong[@class='grant-detail--label']")),
                       value=xml2::xml_text(xml2::xml_find_all(details, ".//div[@class='grant-detail--text']")),
                       stringsAsFactors = F)

    title <- details$value[details$name=="Project Title"]
    date <- details$value[details$name=="Date"]
    data.frame(grantee=info[2], date, amount, program=info[4], id, title,
               year=info[1], stringsAsFactors = FALSE)
  })

  do.call(rbind.data.frame, awards)
}
