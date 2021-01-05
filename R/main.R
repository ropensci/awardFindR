#' awardsBot QDR routine
#'
#' Performs default QDR award query of all awards databases.
#'
#' @param keywords Path to csv file containing keywords to query. Defaults to keywords.csv
#' @return a data.frame
#' @export
#'
#' @examples results <- awardsBot(keywords="keywords.csv")
awardsBot <- function(keywords="data/keywords.csv") {
  stopifnot(file.exists(keywords))
  keywords <- read.csv(keywords,
                       header = FALSE,
                       stringsAsFactors = FALSE)$V1
  keywords <- c("focus group", "ethnography") # Test terms, remove this line for full query
  # Full keywords pull is API request heavy, do on weekends or outside 9-5 hours

  # Calculate previous two years
  today <- Sys.Date()
  twoyrago <- as.POSIXlt(Sys.Date())
  twoyrago$year <- twoyrago$year-2
  twoyrago <- as.Date(twoyrago)

  static <- static_scrape(keywords, twoyrago, today)

  # Run the API query routine
  apis <- lapply(keywords, api_scrape,
                   from=twoyrago, to=today, sources=c("nsf", "ies"))
  apis <- do.call(rbind.data.frame, apis)

  # Bind with the static results
  awards <- rbind.data.frame(static, apis)

  return(awards)
}
