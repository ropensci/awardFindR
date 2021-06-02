#' Search the NSF API for awards
#' @param keyword Keyword to query, single string
#' @param from_date Beginning date object
#' @param to_date End date object
#' @param verbose enable verbose HTTP messages. TRUE/FALSE, default: false
#' @param cfda Comma-separated CFDA codes to include, default: all
#' @return A data.frame of raw NSF API output
#' @export
#' @examples nsf <- nsf_get("ethnography", "2020-01-01", "2020-02-01")
nsf_get <- function(keyword, from_date, to_date, verbose=FALSE, cfda=NULL) {
  base_url <- 'https://api.nsf.gov/services/v1/awards.json?'
  output_data <- paste0("id,date,startDate,expDate,title,awardeeName,",
                        "piFirstName,piLastName,piEmail,cfdaNumber,",
                        "fundsObligatedAmt,fundProgramName,abstractText,",
                        "awardeeCounty") #,publicationResearch")

  query <- paste0('keyword="', gsub(" ", "+", keyword), "\"")

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

  # Max results 25 per request. Do we need to loop the query?
  while (length(api)==25) {
    offset <- offset + 25

    Sys.sleep(3) # Be NICE to the API!

    api <- request(paste0(query_url, '&offset=', offset), "get", verbose)
    api <- api$response$award
    temp <- Reduce(function(x, y) merge(x, y, all=TRUE), api)
    df <- rbind.data.frame(df, temp)
  }

  # Remove factors
  df[] <- lapply(df, as.character)
  df$keyword <- keyword

  df
}

.nsf_standardize <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, nsf_get, from_date, to_date, verbose)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    return(NULL)
  }

  with(raw, data.frame(
    institution=awardeeName, pi=paste0(piLastName, ", ", piFirstName),
    year=format.Date(as.Date(date, format="%m/%d/%Y"), format="%Y"),
    start=as.character(as.Date(startDate, format="%m/%d/%Y")),
    end=as.character(as.Date(expDate, format="%m/%d/%Y")),
    program=cfdaNumber, amount=fundsObligatedAmt, id=id, title=title,
    abstract=abstractText, keyword, source="NSF", stringsAsFactors = FALSE
  ))
}

