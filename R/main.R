#' awardFindR grant search
#'
#' Query a collection of online grant databases for awards.
#' These queries can be limited by keyword, source and date terms.
#'
#' @param keywords Path to keywords csv file (1 term per line) or vector of keywords.
#' @param sources A vector of sources to pull from. Supported: fedreporter, gates, mellon, neh, nih, nsf, ophil, osociety, sloan, ssrc, usaspending, carnegie, macarthur. Default: all
#' @param from_date A date object to limit the search, defaults to Jan 1 2019
#' @param to_date A date object to limit the search, defaults to today
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
#' from_date="2015-01-01", to_date="2020-01-01")
#' }
awardFindR <- function(keywords,
                      sources=c("fedreporter", "gates", "mellon", "carnegie",
                                "macarthur", "neh", "nih", "nsf", "ophil",
                                "osociety", "sloan", "ssrc", "usaspending"),
                      from_date="2019-01-01", to_date=Sys.Date()) {

  options(stringAsFactors=FALSE)

  # Check args for sanity
  stopifnot(is.character(keywords))
  stopifnot(is.character(sources))

  if (length(keywords)==1) # Is an argument of length 1 a path or a keyword?
    # If it's a file, read it into memory as the new keywords term
    if (file.exists(keywords)) keywords <- readLines(keywords)

  # Validate dates
  from_date <- try(as.Date(from_date, format="%Y-%m-%d"))
  to_date <- try(as.Date(to_date, format="%Y-%m-%d"))
  if ("try-error" %in% class(from_date) || is.na(from_date)) stop('"From" date invalid')
  if ("try-error" %in% class(to_date) || is.na(to_date)) stop('"To" date invalid')
  if (from_date > to_date) stop("Ending date must be after beginning date")
  # Many sources can only handle years for date limiting, so calculate those
  from_year <- format.Date(from_date, "%Y")
  to_year <- format.Date(to_date, "%Y")

  # Assembling the final data.frame
  results <- NULL # Keep this var as a placeholder for rbind.data.frame
  # These first APIs here can handle multiple keywords, so we only have to run them once.
  if ("neh" %in% sources)
    results <- rbind.data.frame(results, neh_standardize(keywords, from_year, to_year))
  if ("sloan" %in% sources)
    results <- rbind.data.frame(results, sloan_standardize(keywords, from_year, to_year))
  if ("usaspending" %in% sources)
    results <- rbind.data.frame(results, usaspend_standardize(keywords, from_date, to_date))

  # These APIs below can only handle one keyword at a time, so we'll loop through
  for (keyword in keywords) {
    if ("carnegie" %in% sources)
      results <- rbind.data.frame(results, carnegie_standardize(keyword, from_year, to_year))
    if ("fedreporter" %in% sources)
      results <- rbind.data.frame(results, fedreporter_stardardize(keyword, from_year, to_year))
    if ("gates" %in% sources)
      results <- rbind.data.frame(results, gates_standardize(keyword, from_date, to_date))
    if ("macarthur" %in% sources)
      results <- rbind.data.frame(results, macarthur_standardize(keyword, from_date, to_date))
    if ("mellon" %in% sources)
      results <- rbind.data.frame(results, mellon_standardize(keyword, from_year, to_year))
    if ("nih" %in% sources)
      results <- rbind.data.frame(results, nih_standardize(keyword, from_date, to_date))
    if ("nsf" %in% sources)
      results <- rbind.data.frame(results, nsf_standardize(keyword, from_date, to_date))
    if ("ophil" %in% sources)
      results <- rbind.data.frame(results, ophil_standardize(keyword, from_year, to_year))
    if ("osociety" %in% sources)
      results <- rbind.data.frame(results, osociety_standardize(keyword, from_year, to_year))
    if ("ssrc" %in% sources)
      results <- rbind.data.frame(results, ssrc_standardize(keyword, from_year, to_year))
  }

  # No results?
  if (nrow(results)==0) {
    warning("No results from any source")
    return(NULL)
  }

  # Find and merge duplicates
  if (any(duplicated(results$id))) {
    duplicates <- stats::aggregate(keyword ~ id, data=results,
                                   # Merge keywords
                                   FUN=function(x) paste(x, collapse="; "))
    results$keyword <- NULL # Reset keywords field
    results <- merge(results, duplicates) # Replace with merged keywords
    results <- results[!duplicated(results$id), ] # Delete duplicates
  }

  # Get rid of all caps in some strings
  results$institution <- sapply(as.character(results$institution), title_case)
  results$pi <- sapply(results$pi, title_case)

  return(results)
}
