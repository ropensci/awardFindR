#' Search the NSF API for awards
#' @param cfda Comma-separated CFDA codes to include, default: all
#' @inheritParams get_nih
#' @return A data.frame
#' @export
#' @examples nsf <- get_nsf("ethnography", "2020-01-01", "2020-02-01")
get_nsf <- function(keyword, from_date, to_date, verbose=FALSE, cfda=NULL) {
  base_url <- 'https://www.research.gov/awardapi-service/v1/awards.json?'
  output_cols <- c(
    "id", "date", "startDate", "expDate", "title", "awardeeName",
    "piFirstName", "piLastName", "piEmail", "cfdaNumber",
    "estimatedTotalAmt", "fundProgramName", "abstractText", "awardeeCounty"
  )
  #,publicationResearch")
  output_data <- paste0(output_cols, collapse = ",")

  query <- paste0('keyword=%22', gsub(" ", "%20", keyword), '%22')
  

  # Add CFDA search term if defined
  if (!is.null(cfda)) query <- paste0(query, '&cfdaNumber=', cfda)

  # Collate URL
  query_url <- paste0(base_url, query,  '&printFields=', output_data,
                      '&dateStart=', format.Date(from_date, "%m/%d/%Y"),
                      '&dateEnd=', format.Date(to_date, "%m/%d/%Y"))

  # actually query the API
  offset <- 1
  api <- request(paste0(query_url, '&offset=', offset), "get", verbose)
  api <- api$response$award
  if (length(api)==0) {
    return(NULL) # No results?
  }

  df <- Reduce(function(x, y) merge(x, y, all=TRUE), api)
  df <- as.data.frame(df, stringsAsFactors=FALSE)

  # Ensure all columns are in output data, as API doesn't return
  # cols that are entirely NA
  for (col in output_cols) {
    if (!(col %in% names(df))) df[[col]] <- NA
  }

  # Max results 25 per request. Do we need to loop the query?
  while (length(api)==25) {
    offset <- offset + 25

    Sys.sleep(3) # Be NICE to the API!

    api <- request(paste0(query_url, '&offset=', offset), "get", verbose)
    api <- api$response$award
    temp <- Reduce(function(x, y) merge(x, y, all=TRUE), api)

    # Make sure that the new data have the same columns
    for (col in output_cols) {
      if (!(col %in% names(temp))) temp[[col]] <- NA
    }

    df <- rbind.data.frame(df, temp)
  }

  # Remove factors
  df[] <- lapply(df, as.character)
  df$keyword <- keyword

  df
}

.standardize_nsf <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, get_nsf, from_date, to_date, verbose)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    message("No results from NSF")
    return(NULL)
  }

  with(raw, data.frame(
    institution=awardeeName, pi=paste0(piLastName, ", ", piFirstName),
    year=format.Date(as.Date(date, format="%m/%d/%Y"), format="%Y"),
    start=as.character(as.Date(startDate, format="%m/%d/%Y")),
    end=as.character(as.Date(expDate, format="%m/%d/%Y")),
    program=cfdaNumber, amount=estimatedTotalAmt, id=id, title=title,
    abstract=abstractText, keyword, source="NSF", stringsAsFactors = FALSE
  ))
}

