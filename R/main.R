#' awardsBot QDR routine
#'
#' Performs default QDR award query of all awards databases.
#'
#' @param keywords Path to keywords csv file or vector of keywords. Defaults to data/keywords.csv
#' @param sources A vector of sources to pull from. Supported: nsf, nih, ies, neh, sloan. Default: all
#' @param from A date object to limit the search, defaults to two years ago.
#' @param to A date object to limit the search, defaults to today
#' @return a data.frame
#' @export
#'
#' @examples
#' # Full pull of all keywords and sources
#' # full <- awardsBot()
#'
#' # Specific keywords, all sources:
#' # specific <- awardsBot(keywords=c("ethnography", "case studies"))
#'
#' # All keywords, specific sources:
#' # nsf <- awardsBot(sources="nsf")
#' # nsf_and_neh <- awardsBot(sources=c("nsf", "neh"))
#'
#' # All keywords, all sources, specific date range:
#' # five_years <- awardsBot(from="2015-01-01", to="2020-01-01")
awardsBot <- function(keywords=NULL, sources=NULL, from=NULL, to=NULL) {
  if (is.null(keywords)) {
    keywords <- "data/keywords.csv"
  }
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

  # No sources argument? Pull everything supported
  supported_sources <- c("nsf", "nih", "ies", "neh", "sloan")
  if (is.null(sources)) {
    sources <- supported_sources
  }
  # check sources for sanity
  stopifnot(is.character(sources))

  # Calculate previous two years
  today <- Sys.Date()
  twoyrago <- as.POSIXlt(Sys.Date())
  twoyrago$year <- twoyrago$year-2
  twoyrago <- as.Date(twoyrago)

  if (is.null(from)) {
    from <- twoyrago
  } else {
    from <- try(as.Date(from))
    if ("try-error" %in% class(from) || is.na(from)) {
      stop('"From" date invalid')
    }
  }

  if (is.null(to)) {
    to <- today
  } else {
    to <- try(as.Date(to))
    if ("try-error" %in% class(to) || is.na(to)) {
      stop('"To" date invalid')
    }
  }

  if (from > to) {
    stop("Ending date must be after beginning date")
  }

  awards <- award_scrape(keywords, from, to, sources)
  return(awards)
}
