#' Query awards from the Bill & Melinda Gates Foundation
#' @param keyword Single keyword to query
#' @param from_date Date object to begin search
#' @param to_date Date object to end search
#' @return a data.frame
#' @export
#' @examples
#' gates <- gates_get("qualitative", "2018-01-01", "2020-01-01")
gates_get <- function(keyword, from_date, to_date) {
 url <- "https://www.gatesfoundation.org/services/gfo/search.ashx"

 payload <- list(fieldQueries="(@gfomediatype==\"Grant\")",
                 freeTextQuery=keyword,
                 page="1",
                 resultsPerPage="100")

 response <- request(url, "post", payload)

 df <- lapply(response$results, function(x) {
   x <- unlist(x, recursive=FALSE)
   with(x, data.frame(
      amount, date, description, grantee, url, year,
      stringsAsFactors = FALSE))
 })
 df <- do.call(rbind.data.frame, df)
 if (nrow(df)==0) return(NULL) # No results at first?

 # Extract ID from the ending of the url
 df$id <- regmatches(df$url, regexpr("([-a-zA-Z0-9])+$", df$url))

 # Subset by date
 df$date <- as.Date(df$date)
 df <- subset(df, date >= from_date & date <= to_date)
 if (nrow(df)==0) return(NULL) # No results now?

 df$grantee[df$grantee==""] <- NA

 return(df)
}

#' Standardize award results from the Bill & Melinda Gates Foundation
#' @param keyword Single keyword to query
#' @param from_date Date object to begin search
#' @param to_date Date object to end search
#' @return a standardized data.frame
gates_standardize <- function(keyword, from_date, to_date) {
   gates <- gates_get(keyword, from_date, to_date)
   if (is.null(gates)) return(NULL)
   with(gates, data.frame(
      institution=grantee, pi=NA, year, start=NA, end=NA, program=NA,
      amount, id, title=description, keyword, source="Gates",
      stringsAsFactors = FALSE
   ))
}
