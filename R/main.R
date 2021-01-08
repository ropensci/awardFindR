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
#' \dontrun{full <- awardsBot()}
#'
#' # Specific keywords, all sources:
#' \dontrun{specific <- awardsBot(keywords=c("ethnography", "case studies"))}
#'
#' # All keywords, specific sources:
#' \dontrun{nsf <- awardsBot(sources="nsf")}
#' \dontrun{some <- awardsBot(sources=c("nsf", "neh"))}
#'
#' # All keywords, all sources, specific date range:
#' \dontrun{five_years <- awardsBot(from="2015-01-01", to="2020-01-01")}
awardsBot <- function(keywords=NULL, sources=NULL, from=NULL, to=NULL) {
  if (is.null(keywords)) {
    keywords <- "data/keywords.csv"
  }

  # Is an argument of length 1 a path or a keyword?
  if (length(keywords) == 1) {
    if (file.exists(keywords)) {
      keywords <- read.csv(keywords,
                           header = FALSE,
                           stringsAsFactors = FALSE)$V1
    }
  }

  # No sources argument? Pull everything supported
  if (is.null(sources)) {
    sources <- c("nsf", "nih", "ies", "neh", "sloan")
  }

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

  # Run the static database queries first
  if ("neh" %in% sources || "sloan" %in% sources) {
    static <- static_scrape(keywords, from, to, sources)
  } else {
    static <- NULL
  }

  # Loop the API query routine through our keywords
  if ("nsf" %in% sources || "nih" %in% sources || "ies" %in% sources) {
    apis <- api_scrape(keywords, from, to, sources)
  } else {
    apis <- NULL
  }

  # Bind with the static results
  awards <- rbind.data.frame(static, apis)

  return(awards)
}
