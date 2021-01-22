#' Get details from Gates foundation entry
#'
#' Internal use only
#'
#' @param entry Object passed by gates_get
#'
#' @return single row data.frame
gates_get_details <- function(entry) {
  entry <- unlist(entry, recursive=FALSE)
  df <- with(entry, data.frame(amount, date, description, grantee, url, year, stringsAsFactors = FALSE))
  return(df)
}

#' Query awards from the Bill & Melinda Gates Foundation
#'
#' @param query Single keyword to query
#' @param from Year to begin search
#' @param to Year to end search
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' gates <- gates_get("qualitative", 2018, 2020)
gates_get <- function(query, from, to) {
 url <- "https://www.gatesfoundation.org/services/gfo/search.ashx"

 payload <- list(fieldQueries="(@gfomediatype==\"Grant\")",
                 freeTextQuery=query,
                 page="1",
                 resultsPerPage="100")

 response <- httr::POST(url, body=payload, encode="json")
 response <- httr::content(response)

 df <- lapply(response$results, gates_get_details)
 df <- do.call(rbind.data.frame, df)

 df$grantee[df$grantee==""] <- NA

 return(df)
}
