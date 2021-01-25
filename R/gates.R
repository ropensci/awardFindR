#' Query awards from the Bill & Melinda Gates Foundation
#'
#' @param keyword Single keyword to query
#' @param from Date object to begin search
#' @param to Date object to end search
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' gates <- gates_get("qualitative", "2018-01-01", "2020-01-01")
gates_get <- function(keyword, from, to) {
 url <- "https://www.gatesfoundation.org/services/gfo/search.ashx"

 payload <- list(fieldQueries="(@gfomediatype==\"Grant\")",
                 freeTextQuery=keyword,
                 page="1",
                 resultsPerPage="100")

 response <- post(url, payload)

 df <- lapply(response$results, function(x) {
   x <- unlist(x, recursive=FALSE)
   with(x, data.frame(amount, date, description, grantee, url, year, stringsAsFactors = FALSE))
 })
 df <- do.call(rbind.data.frame, df)

 # Extract ID from the ending of the url
 df$id <- regmatches(df$url, regexpr("([-a-zA-Z0-9])+$", df$url))

 # Subset by date
 df$date <- as.Date(df$date)
 df <- subset(df, date >= from & date <= to)

 df$grantee[df$grantee==""] <- NA

 return(df)
}
