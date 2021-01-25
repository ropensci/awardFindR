#' Query a NEH grant data.frame for a keyword
#'
#' Loop for neh_get(). Not intended to be run directly.
#'
#' @param query A single string keyword
#' @param df A data.frame of NEH grant data to search through
#' @return A data.frame with the relevant results matching the keyword
neh_query <- function(query, df) {
  # grep the query in the description, subset to the hits
  hits <- grepl(query, df$ProjectDesc,
                ignore.case=TRUE)
  hits <- df[hits, ]

  # Empty results?
  if (nrow(hits)==0) {
    message(paste("NOTICE (non-fatal) no NEH results for:", query))
    return(NULL)
  }

  hits$query <- query
  return(hits)
}

#' Grab the NEH grants data and loop the keyword queries
#'
#' So that we don't keep redownloading the NEH csv file, this is a wrapper to loop neh_query()
#'
#' @param keywords Vector of strings to search for in the project description
#' @param from Beginning year to search
#' @param to Ending year to search
#'
#' @return A data.frame with the relevant results from NEH
#' @export
#'
#' @examples
#' neh <- neh_get(c("focus groups", "ethnography"), 2018, 2020)
neh_get <- function(keywords, from, to) {
  # This file is updated monthly, should hopefully be valid for the next decade?
  # See https://securegrants.neh.gov/open/data/
  url <- "https://securegrants.neh.gov/Open/data/NEH_Grants2020s.csv"
  message(paste("GET", url))
  neh <- utils::read.csv(url,
                         na.strings = c("NA", "NULL", "Unknown"),
                         stringsAsFactors = FALSE,
                         fileEncoding = "UTF-8-BOM")

  neh <- subset(neh, YearAwarded >= from & YearAwarded <= to)

  results <- lapply(keywords, neh_query, neh)
  results <- do.call(rbind.data.frame, results)
  # Did we get nothing after all these queries?
  if (nrow(results)==0) {
    return(NULL)
  }

  # Some regex magic to make the "participant" field more applicable to our PI field
  results$pi <- sub(" \\[Project Director\\].*", "", results$Participants)

  return(results)
}
