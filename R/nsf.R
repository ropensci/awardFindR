#' Scrape the NSF api
#'
#' Query the NSF API for awards, get the XML response(s) and process it into a data.frame
#'
#' @param keyword Keyword to query, single string
#' @param from Beginning date, standard format
#' @param to End date, standard format
#' @return A data.frame of raw NSF API output, or NULL if no results
#' @export
#' @examples nsf <- nsf_get(keyword="ethnography", from="2020-01-01", to="2020-02-01")
nsf_get <- function(keyword, from, to) {
  base_url <- 'https://api.nsf.gov/services/v1/awards.xml?'
  output_data <- 'id,date,startDate,expDate,title,awardeeName,piFirstName,piLastName,piEmail,cfdaNumber'
  output_data <- paste0(output_data, ",estimatedTotalAmt,fundProgramName") # Extra info
  cfda <- "47.076,47.075"

  keyword <- gsub(" ", "+", keyword)
  query <- paste0('&keyword="', keyword, '"&cfdaNumber=', cfda)

  # Collate URL
  query_url <- paste0(base_url, query,  '&printFields=', output_data,
                      '&dateStart=', format.Date(from, "%m/%d/%Y"),
                      '&dateEnd=', format.Date(to, "%m/%d/%Y"))

  # actually query the API
  api <- request(paste0(query_url, '&offset=', 1), "get")

  # API call produce an error? Print it
  if(length(xml2::xml_find_first(api, "/response/serviceNotification") > 0)) {
    stop(paste0(xml2::xml_text(xml2::xml_find_first(api, "//notificationType")),
                ": ",
                xml2::xml_text(xml2::xml_find_first(api, "//notificationMessage")))
         )
  }

  # No results?
  if (xml2::xml_length(api) == 0) {
    return(NULL)
  }

  # Max results 25. Do we need to loop the query?
  if(length(xml2::xml_find_all(api, xpath="/response/award"))==25) {
  # We need to loop using different offsets
    n <- 1
    repeat {
      start <- 1 + 25 * n
      extra <- request(paste0(query_url, '&offset=', start), "get")

      # Are there no results left yet?
      if (xml2::xml_length(extra) == 0) {
        break
      }

      lapply(xml2::xml_children(extra),
             function(x) xml2::xml_add_child(api, x))

      n <- n + 1
    }
  }

  # Convert internal XML tree to list
  awards <- xml2::xml_find_all(api, "/response/award")
  df <- xml2list_to_df(xml2::as_list(awards))

  return(df)

}
