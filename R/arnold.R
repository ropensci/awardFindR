
library(jsonlite)
#' Get Arnold Foundations/Arnold Venture awards
#'
#' @param keyword Keyword to query, single string
#' @param from_year Year to begin search, integer
#' @param to_year Year to end search, integer
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' arnold <- arnold_get("qualitative data", 2016, 2017)
arnold_get <- function(keyword, from_year, to_year) {

  years <- paste0('"years:', from_year, '"')
  for (n in (from_year+1):to_year) years <- paste0(years, ',"years:', n, '"')
  years <- xml2::url_escape(years)

# not all of the query might be required, but keeping to be safe
  query <- paste0("query=", keyword, "&maxValuesPerFacet=100&highlightPreTag=__ais-highlight__&highlightPostTag=__%2Fais-highlight__&page=0&facets=%5B%22topics%22%2C%22years%22%2C%22fundingSource%22%5D&tagFilters=&facetFilters=%5B%5B%22", years, "%5D%5D"  )

  body_list <- list(requests = list(list(indexName = jsonlite::unbox("prod_grants"), params = jsonlite::unbox(query))))

  r <- httr::POST(url, body = body, encode = "json")
  api_response <- jsonlite::fromJSON(httr::content(r, "text"))
  awards <- api_response$results['hits'][[1]][[1]]

  if (length(awards)==0) return(NULL) # No awards in the table?

  else {
    awards <- subset(awards, select = c("title",  "grantTerm", "fundingSource", "grantAmount", "url", "grantDescription"))
  }
}
