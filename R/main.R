#' awardsBot QDR routine
#'
#' Performs default QDR award query of all awards databases.
#'
#' @param keywords Path to keywords csv file or vector of keywords. Defaults to data/keywords.csv
#' @return a data.frame of results
#' @export
#'
#' @examples
#' results <- awardsBot(keywords="data/keywords.csv")
#' results <- awardsBot(keywords=c("ethnography", "case studies"))
awardsBot <- function(keywords=NULL) {
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

  # Calculate previous two years
  today <- Sys.Date()
  twoyrago <- as.POSIXlt(Sys.Date())
  twoyrago$year <- twoyrago$year-2
  twoyrago <- as.Date(twoyrago)

  # Run the static database queries first
  static <- static_scrape(keywords, twoyrago, today)

  # Loop the API query routine through our keywords
  apis <- lapply(keywords, api_scrape,
                   from=twoyrago, to=today, sources=c("nsf", "nih", "ies"))
  apis <- do.call(rbind.data.frame, apis)

  # Bind with the static results
  awards <- rbind.data.frame(static, apis)

  return(awards)
}
