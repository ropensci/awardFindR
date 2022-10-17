.get_ssrc_details <- function(hit) {
  with(hit, data.frame(id=post_id, name=post_title,
                       date=as.POSIXct(post_date, origin="1970-01-01"),
                       year=competition_year, link=permalink,
                       title=project_title, institution=award_institution,
                       program=related_competitions, description=content))
}
#' Search SSRC fellowships and grants by keyword and date
#' Note: API limitations prevent returning more than 1000 results.
#'
#' @inheritParams get_neh
#' @param totals Only return the total number of results, not a table
#' @return a data.frame
#' @export
#' @examples \dontrun{ssrc <- get_ssrc("qualitative", 2015, 2016)}
get_ssrc <- function(keyword, from_year, to_year, verbose=FALSE, totals=FALSE) {

  url <- "https://12786hbsdl-dsn.algolia.net/1/indexes/*/queries?x-algolia-agent=Algolia%20for%20JavaScript%20(4.10.3)%3B%20Browser%20(lite)%3B%20instantsearch.js%20(4.25.2)%3B%20JS%20Helper%20(3.5.4)&x-algolia-api-key=cdd85ab4fc628277674bb5d9e375af2b&x-algolia-application-id=12786HBSDL"

  year_filter <- paste0('"competition_year:', from_year, '"')
  for (year in I(as.integer(from_year)+1):as.integer(to_year))
    year_filter <- paste0(year_filter, ',"competition_year:', year, '"')

  payload <- list(requests=list(list(
    indexName="wp_searchable_posts",
    params=paste0('query="', keyword, '"'),
#    hitsPerPage=list("20", "1"),
#    maxValuesPerFacet=list("10", "10"),
#    page=list("0", "0"),
#    facets=list('["post_type"]', '["post_type"]"}]}'),
#    tagFilters=list("", ""),
    facetFilters=paste0('[[', year_filter, '],',
                        '["post_type:fellow"]]')
)))

  response <- request(url, "post", verbose, payload) # Query API
  results <- response$results[[1]]

  # If we only want the totals, return that now
  if (totals==TRUE) {
    return(results$nbHits)
  }

  # No results?
  if (results$nbHits==0) {
    return(NULL)
  }

  df <- lapply(results$hits, .get_ssrc_details)
  df <- do.call(rbind.data.frame, df)

  if (results$nbPages>1) { # Need to loop?
    for (n in results$page:I(results$nbPages-1)) {
      payload$requests[[1]]$page <- as.character(n)
      response <- request(url, "post", verbose, payload) # Query API
      results <- response$results[[1]]
      temp <- lapply(results$hits, .get_ssrc_details)
      temp <- do.call(rbind.data.frame, temp)
      df <- rbind.data.frame(df, temp)
    }
  }

  df$keyword <- keyword

  df
}

.standardize_ssrc <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, get_ssrc,
                format.Date(from_date, "%Y"), format.Date(to_date, "%Y"),
                verbose)
  raw <- do.call(rbind.data.frame, raw)

  if (nrow(raw)==0) {
    message("No results from SSRC")
    return(NULL)
  }

  with(raw, data.frame(
    institution, pi=name, year, start=date, end=NA, program, amount=NA,
    id, title, abstract=description, keyword, source="SSRC", stringsAsFactors = FALSE
  ))
}
