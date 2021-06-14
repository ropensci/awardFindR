#' Get relevant awards from NEH
#' @param keywords Vector of strings to search for in the project description
#' @param from_year Beginning year to search
#' @param to_year Ending year to search
#' @param verbose enable verbose HTTP messages. TRUE/FALSE, default: false
#' @return A raw data.frame with the relevant results from NEH
#' @export
#' @examples
#' neh <- get_neh(c("focus groups", "ethnography"), 2018, 2020)
get_neh <- function(keywords, from_year, to_year, verbose=FALSE) {
  # This file is updated monthly, should hopefully be valid for the next decade?
  # See https://securegrants.neh.gov/open/data/
  url <- "https://securegrants.neh.gov/Open/data/NEH_Grants2020s.csv"
  if (verbose==TRUE)  message(paste("GET", url, "... "), appendLF = FALSE)
  response <- httr::GET(url)
  if (verbose==TRUE) {
    httr::message_for_status(response)
    message()
  }
  httr::stop_for_status(response)

  response <- httr::content(response, as="text", encoding="UTF-8")
  neh <- utils::read.csv(text=response,
                         na.strings = c("NA", "NULL", "Unknown"),
                         stringsAsFactors = FALSE,
                         fileEncoding = "UTF-8-BOM")

  YearAwarded <- NULL # For R CMD check
  neh <- subset(neh, YearAwarded >= from_year & YearAwarded <= to_year)

  results <- lapply(keywords, function(keyword, df) {
    # grep the query in the description, subset to the hits
    hits <- grepl(keyword, df$ProjectDesc, ignore.case=TRUE)
    hits <- df[hits, ]

    # Empty results?
    if (nrow(hits)==0) {
      return(NULL)
    }

    hits$keyword <- keyword
    hits
  }, neh) # The df input is here at the end

  results <- do.call(rbind.data.frame, results)
  # Did we get nothing after all these queries?
  if (nrow(results)==0) {
    return(NULL)
  }

  # Some regex magic to make the "participant" field applicable to our PI field
  results$pi <- sub(" \\[Project Director\\].*", "", results$Participants)

  results
}

.standardize_neh <- function(keywords, from_date, to_date, verbose) {
  raw <- get_neh(keywords,
                 format.Date(from_date, "%Y"), format.Date(to_date, "%Y"),
                 verbose)
  if (is.null(raw)) {
    message("No results from NEH")
    return(NULL)
  }

  with(raw, data.frame(
    institution=Institution, pi, year=YearAwarded,
    start=BeginGrant, end=EndGrant,
    program=Program, amount=as.integer(AwardOutright),
    # Encoding problems on windows changes first variable name ("AppNumber")
    id=as.character(raw[1]), title=ProjectTitle, abstract=ProjectDesc,
    keyword, source="NEH", stringsAsFactors = FALSE
  ))
}
