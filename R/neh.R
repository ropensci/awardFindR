#' Query a NEH grant data.frame for a keyword
#'
#' @param query A single string keyword
#' @param df A data.frame of NEH grant data to search through
#' @param from Beginning year to search
#' @param to Ending year to search
#'
#' @return A data.frame with the relevant results matching the keyword
#'
neh_query <- function(query, df, from, to) {
  df <- subset(df, YearAwarded >= from & YearAwarded <= to)

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

#' Grab the NEH grants data and run a series of queries
#'
#' So that we don't keep redownloading the NEH csv file, this is a wrapper to loop neh_query
#'
#' @param keywords Vector of strings to search for in the project description
#' @param from Beginning year to search
#' @param to Ending year to search
#'
#' @return A subsetted data.frame with the relevant results from NEH
#'
#' @examples neh_get(c("focus groups", "ethnography"), 2018, 2020)
neh_get <- function(keywords, from, to) {
  # This file is updated monthly, should hopefully be valid for the next decade?
  # See https://securegrants.neh.gov/open/data/
  url <- "https://securegrants.neh.gov/Open/data/NEH_Grants2020s.csv"
  message(paste("Grabbing url:", url))
  neh <- read.csv(url,
                  na.strings = c("NA", "NULL", "Unknown"),
                  stringsAsFactors = FALSE)

  results <- lapply(keywords, neh_query, neh, from, to)
  results <- do.call(rbind.data.frame, results)

  # Some regex magic to make the "participant" field more applicable to our PI field
  results$pi <- sub(" \\[Project Director\\].*", "", results$Participants)

  return(results)
}
