#' Query awards from the Bill & Melinda Gates Foundation
#' @param keyword Single keyword to query
#' @param from_year Year integer to begin search
#' @param to_year Year integer to end search
#' @return a data.frame
#' @export
#' @examples
#' gates <- gates_get("qualitative", "2018-01-01", "2020-01-01")
gates_get <- function(keyword, from_year, to_year) {
 url <- "https://www.gatesfoundation.org/api/grantssearch"

 params <- paste0("?date&displayedTaxonomy&",
 "listingId=d2a41504-f557-4f1e-88d6-ea109d344feb",
 "&loadAllPages=true&page=1",
 "&pageId=31242fca-dcf8-466a-a296-d6411f85b0a5&perPage=999")

 params <- paste0(params, "&q=", xml2::url_escape(keyword),
                  "&sc_site=gfo&showContentTypes=false&showDates=false",
                  "&showImages&showSummaries=false&sortBy=date-desc",
                  "&sortOrder=desc")

 params <- paste0(params, "&yearAwardedEnd=", to_year,
                  "&yearAwardedStart=", from_year)

 url <- paste0(url, params)

 response <- request(url, "get")

 if (response$totalResults==0) {
    return(NULL)
 }

 df <- lapply(response$results, function(x) {
   x <- unlist(x, recursive=FALSE)
   with(x, data.frame(
      awardedAmount, grantee, url, yearAwarded, id,
      stringsAsFactors = FALSE))
 })
 df <- do.call(rbind.data.frame, df)

 df$grantee[df$grantee==""] <- NA
 # Get rid of all the $ and commas
 df$awardedAmount <- gsub("^\\$|,", "", df$awardedAmount)
 df$keyword <- keyword

 df
}

.gates_standardize <- function(keywords, from_date, to_date) {
   raw <- lapply(keywords, gates_get,
                 format.Date(from_date, "%Y"), format.Date(to_date, "%Y"))
   raw <- do.call(rbind.data.frame, raw)
   if (nrow(raw)==0) {
      return(NULL)
   }

   with(raw, data.frame(
      institution=grantee, pi=NA, year=yearAwarded, start=NA, end=NA,
      program=NA, amount=awardedAmount, id, title=NA, abstract=NA,
      keyword, source="Gates", stringsAsFactors = FALSE
   ))
}
