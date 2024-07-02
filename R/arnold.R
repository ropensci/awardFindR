#' Get Arnold Foundations/Arnold Venture awards
#' @inheritParams get_ssrc
#' @return a data.frame
#' @export
#' @examples
#' arnold <- get_arnold("qualitative", 2016, 2017)
get_arnold <- function(keyword, from_year, to_year, verbose=FALSE) {
  url <- paste0("https://pyj9b8sltv-dsn.algolia.net",
  "/1/indexes/*/queries?",
  "x-algolia-agent=Algolia%20for%20JavaScript%20(4.5.1)%3B%20Browser%20(lite)",
  "%3B%20instantsearch.js%20(4.8.3)%3B%20JS%20Helper%20(3.2.2)",
  "&x-algolia-api-key=bec9ead5977b11ae383f4df272c2f106",
  "&x-algolia-application-id=PYJ9B8SLTV")

  years <- paste0('"years:', from_year, '"')
  for (n in (as.integer(from_year)+1):as.integer(to_year))
    years <- paste0(years, ',"years:', n, '"')
  years <- xml2::url_escape(years)

  # Not including all the terms at the end gives a HTTP 400 error
  page <- 0
  query <- paste0("query=", keyword,
                  "&maxValuesPerFacet=100&highlightPreTag=__ais-highlight__",
                  "&highlightPostTag=__%2Fais-highlight__&",
                  "facets=%5B%22topics%22%2C%22years%22%2C%22",
                  "fundingSource%22%5D&tagFilters=",
                  "&facetFilters=%5B%5B", years, "%5D%5D")

  payload <- list(requests = list(list(indexName ="prod_grants",
                                         params = paste0(query, "&page=0"))))

  response <- request(url, "post", verbose, payload)
  response <- unlist(response, recursive=FALSE)$results
  num_hits <- response$nbHits
  if (num_hits==0) {
    return(NULL) # No results?
  }

  full <- list()
  while (response$page <= response$nbPages) {
    awards <- response$hits
    row <- lapply(awards, function(x) {
      entry <- unlist(x, recursive=FALSE)
      with(entry, data.frame(
        title, grantDescription, grantTerm,
        fundingSource, grantAmount,
        url, objectID, keyword, stringsAsFactors = FALSE))
    })
    full[[response$page+1]] <- do.call(rbind.data.frame, row)

    # Load next page
    payload$requests[[1]]$params <- paste0(query, "&page=", response$page+1)
    response <- request(url, "post", verbose, payload)
    response <- unlist(response, recursive=FALSE)$results
  }


  results <- do.call(rbind.data.frame, full)

  if (nrow(results) < num_hits) {
    message("Arnold can only return 1,000 results: not all results were loaded")
  }

  results

}

.standardize_arnold <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, get_arnold,
                   format.Date(from_date, "%Y"), format.Date(to_date, "%Y"),
                verbose)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    message("No results from Arnold")
    return(NULL)
  }

  with(raw, data.frame(
    institution=title, pi=NA,
    year=substr(grantTerm, 1, 4), start=substr(grantTerm, 1, 4),
    end=.substr_right(as.character(grantTerm), 4),
    program=fundingSource, amount=grantAmount, id=as.character(objectID),
    title=grantDescription, abstract=NA, keyword, source="Arnold",
    stringsAsFactors = FALSE
  ))
}
