#' awardFindR grant search
#'
#' Query a collection of online grant databases for awards.
#' These queries can be limited by keyword, source and date terms.
#'
#' @param keywords Path to keywords csv file (1 term per line) or vector of keywords.
#' @param sources A vector of sources to pull from. Supported: nsf, nih, fedreporter, neh, sloan, ophil. Default: all
#' @param from A date object to limit the search, defaults to Jan 1 2019
#' @param to A date object to limit the search, defaults to today
#' @return a data.frame
#' @export
#'
#' @examples
#' # Results for "ethnography" from NSF between 1/1 and 2/1 2020
#' awards <- awardFindR("ethnography", "nsf", "2020-01-01", "2020-02-01")
#'
#' # More intensive queries
#' \dontrun{
#' # Full pull of all keywords and sources
#' full <- awardFindR(keywords="data/keywords.csv")
#'
#' # Specific keywords, all sources:
#' specific <- awardFindR(keywords=c("ethnography", "case studies"))
#'
#' # All keywords, specific sources:
#' nsf <- awardFindR(keywords="data/keywords.csv", sources="nsf")
#' nsf_and_neh <- awardFindR(keywords="data/keywords.csv",
#' sources=c("nsf", "neh"))
#'
#' # Specific keyword, all sources, specific date range:
#' five_years <- awardFindR(keywords="qualitative",
#' from="2015-01-01", to="2020-01-01")
#' }
awardFindR <- function(keywords,
                      sources=c("neh", "sloan", "nsf", "nih", "fedreporter", "ophil"),
                      from="2019-01-01", to=Sys.Date()) {

    # Check keywords for sanity
  stopifnot(is.character(keywords))

  # Is an argument of length 1 a path or a keyword?
  if (length(keywords) == 1) {
    if (file.exists(keywords)) {
      keywords <- read.csv(keywords,
                           header = FALSE,
                           stringsAsFactors = FALSE)$V1
    }
  }

  # check sources for sanity
  stopifnot(is.character(sources))

  # Calculate previous two years
  #today <- Sys.Date()
  #twoyrago <- as.POSIXlt(Sys.Date())
  #twoyrago$year <- twoyrago$year-2
  #twoyrago <- as.Date(twoyrago)

  # Validate dates
  from <- try(as.Date(from))
  if ("try-error" %in% class(from) || is.na(from)) {
    stop('"From" date invalid')
  }

  to <- try(as.Date(to))
  if ("try-error" %in% class(to) || is.na(to)) {
    stop('"To" date invalid')
  }

  if (from > to) {
    stop("Ending date must be after beginning date")
  }

  # Run the routines in apis.R
  awards <- award_scrape(keywords, from, to, sources)

  # Find, remove duplicates
  duplicates <- awards[duplicated(awards$id), ]
  awards <- awards[!duplicated(awards$id), ]
  # Merge keywords
  for (n in 1:nrow(duplicates)) {
    awards[awards$id==duplicates$id[n], ]$keyword <-
      paste0(awards[awards$id==duplicates$id[n], ]$keyword, "; ",
             duplicates$keyword[n])
  }

  return(awards)
}
