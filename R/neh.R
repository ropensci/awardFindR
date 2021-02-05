#' Grab the NEH grants data and loop the keyword queries
#'
#' So that we don't keep redownloading the NEH csv file, this is a wrapper to loop neh_query()
#'
#' @param keywords Vector of strings to search for in the project description
#' @param from_year Beginning year to search
#' @param to_year Ending year to search
#'
#' @return A data.frame with the relevant results from NEH
#' @export
#'
#' @examples
#' neh <- neh_get(c("focus groups", "ethnography"), 2018, 2020)
neh_get <- function(keywords, from_year, to_year) {
  # This file is updated monthly, should hopefully be valid for the next decade?
  # See https://securegrants.neh.gov/open/data/
  url <- "https://securegrants.neh.gov/Open/data/NEH_Grants2020s.csv"
  message(paste("GET", url, "... "), appendLF = F)
  response <- httr::GET(url)
  httr::message_for_status(response)
  message()
  httr::stop_for_status(response)

  response <- httr::content(response, as="text", encoding="UTF-8")
  neh <- utils::read.csv(text=response,
                         na.strings = c("NA", "NULL", "Unknown"),
                         stringsAsFactors = FALSE,
                         fileEncoding = "UTF-8-BOM")

  YearAwarded <- NULL # For R CMD check
  neh <- subset(neh, YearAwarded >= from_year & YearAwarded <= to_year)

  results <- lapply(keywords, function(query, df) {
    # grep the query in the description, subset to the hits
    hits <- grepl(query, df$ProjectDesc,
                  ignore.case=TRUE)
    hits <- df[hits, ]

    # Empty results?
    if (nrow(hits)==0) {
      message(paste("No NEH results for:", query))
      return(NULL)
    }

    hits$query <- query
    return(hits)
  },
  neh) # The df input is here at the end

  results <- do.call(rbind.data.frame, results)
  # Did we get nothing after all these queries?
  if (nrow(results)==0) return(NULL)

  # Some regex magic to make the "participant" field more applicable to our PI field
  results$pi <- sub(" \\[Project Director\\].*", "", results$Participants)

  return(results)
}
