#' awardFindR grant search
#'
#' Query a collection of online grant databases for awards.
#' These queries can be limited by keyword, source and date terms.
#'
#' @param keywords Path to keywords csv file (1 term per line) or vector.
#' @param sources A vector of sources to pull from. Default: all
#' @param from_date A date object to limit the search, defaults to Jan 1 2019
#' @param to_date A date object to limit the search, defaults to today
#' @return a data.frame
#' @export
#'
#' @examples
#' # Results for "ethnography" from NSF between 1/1 and 2/1 2020
#' \dontrun{awards <- awardFindR("ethnography", "nsf",
#' "2020-01-01", "2020-02-01")}
#'
#' # More intensive queries
#' \dontrun{
#' # Specific keywords, all sources:
#' specific <- awardFindR(keywords=c("ethnography", "case studies"))
#'
#' # Specific keyword, all sources, specific date range:
#' five_years <- awardFindR(keywords="qualitative",
#' from_date="2015-01-01", to_date="2020-01-01")
#' }
awardFindR <- function(keywords,
                      sources=c("arnold", "carnegie", "fedreporter",
                                "gates", "macarthur", "mellon",
                                "neh", "nih", "nsf", "ophil",
                                "osociety", "rockefeller", "rsf",
                                "rwjf", "sloan", "ssrc", "templeton",
                                "usaspend"),
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
  if ("try-error" %in% class(from_date) || is.na(from_date)) {
    stop('"From" date invalid')
  }

  if ("try-error" %in% class(to_date) || is.na(to_date)) {
    stop('"To" date invalid')
  }

  if (from_date > to_date) {
    stop("Ending date must be after beginning date")
  }

  # Assembling the full data.frame
  results <- NULL # Keep this var as a placeholder for rbind.data.frame
  for (source in sources) {
    # Does source routine exist?
    stopifnot(exists(paste0(".", source, "_standardize")))
    results <- eval(parse(text=paste0( # eval the term and run it
      'rbind.data.frame(results, .',
      source, '_standardize(keywords, from_date, to_date))')))
  }

  # No results?
  if (nrow(results)==0 | is.null(results)) {
    message("No results from any source")
    return(data.frame())
  }

  # Find and merge duplicates
  if (any(duplicated(results$id))) {
    duplicates <- stats::aggregate(keyword ~ id + source, data=results,
                                   # Merge keywords
                                   FUN=function(x) paste(x, collapse="; "))
    results$keyword <- NULL # Reset keywords field
    results <- merge(results, duplicates) # Replace with merged keywords

    # Delete duplicates, but make sure it's not from a different source!
    results$dup_id <- with(results, paste(source, id))
    results <- results[!duplicated(results$dup_id), ] # Delete duplicates
    results$dup_id <- NULL
  }

  results
}
