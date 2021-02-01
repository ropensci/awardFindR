#' Search the NSF API
#'
#' Query the NSF API for awards by keyword and date
#'
#' @param keyword Keyword to query, single string
#' @param from Beginning date object
#' @param to End date object
#' @param cfda Comma-separated CFDA codes to include, SBE and EHR only by default
#' @return A data.frame of raw NSF API output, or NULL if no results
#' @export
#' @examples nsf <- nsf_get(keyword="ethnography", from="2020-01-01", to="2020-02-01")
nsf_get <- function(keyword, from, to, cfda="47.076,47.075") {
  base_url <- 'https://api.nsf.gov/services/v1/awards.json?'
  output_data <- 'id,date,startDate,expDate,title,awardeeName,piFirstName,piLastName,piEmail,cfdaNumber'
  output_data <- paste0(output_data, ",fundsObligatedAmt,fundProgramName") # Extra info

  keyword <- gsub(" ", "+", keyword)
  query <- paste0('&keyword="', keyword, '"&cfdaNumber=', cfda)

  # Collate URL
  query_url <- paste0(base_url, query,  '&printFields=', output_data,
                      '&dateStart=', format.Date(from, "%m/%d/%Y"),
                      '&dateEnd=', format.Date(to, "%m/%d/%Y"))

  # actually query the API
  offset <- 1
  api <- request(paste0(query_url, '&offset=', offset), "get")
  api <- api$response$award
  if (length(api)==0) return(NULL) # No results?
  df <- do.call(rbind.data.frame, api)

  # Max results 25 per request. Do we need to loop the query?
  while (length(api)==25) {
    offset <- offset + 25
    api <- request(paste0(query_url, '&offset=', offset), "get")
    api <- api$response$award
    temp <- do.call(rbind.data.frame, api)
    df <- rbind.data.frame(df, temp)
  }

  # Remove factors
  df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else {x})

  return(df)

}
