.get_details_ssrc <- function(entry) {
  pi_name <- xml2::xml_text(
    xml2::xml_find_first(entry, ".//h5/a/text()"))

  title <- xml2::xml_text(
    xml2::xml_find_first(entry, ".//p[@class='l-project-title']/em"))

  program <- xml2::xml_text(
    xml2::xml_find_first(entry, ".//dl[@class='l-inline-summary']/dd/a"))

  data <- xml2::xml_find_all(entry, ".//dl[@class='l-inline-summary']")

  data <- data.frame(label=xml2::xml_text(
    xml2::xml_find_all(data, ".//dt/text()")),
                     data=xml2::xml_text(
                       xml2::xml_find_all(data, ".//dd")))

  year <- as.character(data$data[as.character(data$label)=="Year "])

  institution <-
    data$data[as.character(data$label)==
                "University/Institution (at time of award)"]

  if (length(institution)==0) {
    institution <- xml2::xml_text(
      xml2::xml_find_first(entry, ".//p[@class='l-institution']"))
  }

  id <- xml2::xml_text(xml2::xml_find_first(entry, ".//h5/a/@href"))
  id <- regmatches(id, regexpr("/([-A-Z0-9])+/$", id))

  data.frame(pi_name, institution, year, title, program, id,
             stringsAsFactors = FALSE)
}

#' Search SSRC fellowships and grants by keyword and date
#' @inheritParams get_fedreporter
#' @return a data.frame
#' @export
#' @examples ssrc <- get_ssrc("qualitative", 2015, 2016)
get_ssrc <- function(keyword, from_year, to_year, verbose=FALSE) {

  url <- "https://12786hbsdl-dsn.algolia.net/1/indexes/*/queries?x-algolia-agent=Algolia%20for%20JavaScript%20(4.10.3)%3B%20Browser%20(lite)%3B%20instantsearch.js%20(4.25.2)%3B%20JS%20Helper%20(3.5.4)&x-algolia-api-key=cdd85ab4fc628277674bb5d9e375af2b&x-algolia-application-id=12786HBSDL"
  payload <- list(requests=list(list(
    indexName="wp_searchable_posts",
    params=paste0('query="', keyword, '"')
#    hitsPerPage=list("20", "1"),
#    maxValuesPerFacet=list("10", "10"),
#    page=list("0", "0"),
#    facets=list('["post_type"]', '["post_type"]"}]}'),
#    tagFilters=list("", ""),
#    facetFilters='[["post_type:fellow"]]"},{"indexName":"wp_searchable_posts","params":"query"'
)))

  response <- request(url, "post", verbose, payload) # Query API

  #base_url <- "https://www.ssrc.org/search/?"
  #query <- "t=fellows&sort=relevance"
  #query <- paste0(query, "&q=", xml2::url_escape(keyword))
  # Have to collate years to search by date
  if (length(from_year:to_year) > 1)
    query <- paste0(query, "&year=", paste0(from_year:to_year, collapse=","))
  else query <- paste0(query, "&year[]=", from_year)

  url <- paste0(base_url, query)
  page <- request(url, "get", verbose)

  entries <- xml2::xml_find_all(page, "//li[@class='hit l-fellows-hit']")
  if (length(entries)==0) {
    return(NULL)   # No results?
  }
  df <- lapply(entries, .get_details_ssrc)
  df <- do.call(rbind.data.frame, df)

  if (length(entries)==25) { # Need to loop?
    n <- 2
    repeat {
      url <- paste0(base_url, query, "&p=", n)
      page <- request(url, "get", verbose)
      entries <- xml2::xml_find_all(page, "//li[@class='hit l-fellows-hit']")
      if (length(entries)==0) break

      temp <- lapply(entries, .get_details_ssrc)
      temp <- do.call(rbind.data.frame, temp)
      df <- rbind.data.frame(df, temp)
      n <- n + 1
    }
  }

  df$keyword <- keyword

  df
}

.standardize_ssrc <- function(keywords, from_date, to_date, verbose) {
  # temporarily disable, not working
  message("SSRC currently under maintainance")
  return(NULL)

  raw <- lapply(keywords, get_ssrc,
                format.Date(from_date, "%Y"), format.Date(to_date, "%Y"),
                verbose)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    message("No results from SSRC")
    return(NULL)
  }

  with(raw, data.frame(
    institution, pi=pi_name, year, start=NA, end=NA, program, amount=NA,
    id, title, abstract=NA, keyword, source="SSRC", stringsAsFactors = FALSE
  ))
}
