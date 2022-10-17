#' Search MacArthur foundation for awards
#' @inheritParams get_neh
#' @return a data.frame
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' macarthur <- get_macarthur("qualitative",
#' "1999-01-01", "2020-01-01")
get_macarthur <- function(keyword, from_year, to_year, verbose=FALSE) {
  url <- "https://searchg2.crownpeak.net/live-macfound-rt/select?"
  parameters <- paste0("q=", xml2::url_escape(keyword),
                       "&wt=json&start=0&rows=25494")

  # I really don't know what most of this does below, but it was part of the
  # web interface's API query, so I'm leaving it in for now.
  extra <- paste0("&echoParams=explicit&fl=%2A&defType=edismax",
  "&fq=custom_s_template%3A%22grant%20detail%22&sort=score%20desc",
  "&qf=custom_t_title%20custom_t_description%20custom_t_name")
  #"&indent=true&json.wrf=searchg2_5445608496089546")

  query_url <- paste0(url, parameters, extra)
  response <- request(query_url, "get", verbose)
  response <- jsonlite::fromJSON(response)

  if (response$response$numFound==0) {
    return(NULL)
  }

  results <- response$response$docs

  # Date limit
  custom_i_year_approved <- NULL # R CMD Check note issue
  results <- subset(results, custom_i_year_approved > as.integer(from_year) &
                      custom_i_year_approved < as.integer(to_year))
  if (nrow(results)==0) {
    return(NULL) # No results after date limiting?
  }

  results$keyword <- keyword
  results[, c("id", "custom_s_name", "custom_i_year_approved",
              "custom_s_start_date", "custom_s_end_date",
              "custom_s_program_area_code", "custom_s_amount",
              "custom_s_title", "custom_s_description", "keyword")]
}

.standardize_macarthur <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, get_macarthur,
                format.Date(from_date, "%Y"), format.Date(to_date, "%Y"),
                verbose)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    message("No results from MacArthur")
    return(NULL)
  }

  with(raw, data.frame(
    institution=custom_s_name, pi=NA, year=custom_i_year_approved,
    start=substr(custom_s_start_date, 1, 10),
    end=substr(custom_s_end_date, 1, 10),
    program=custom_s_program_area_code,
    amount=as.integer(custom_s_amount), id, title=custom_s_title,
    abstract=custom_s_description,
    keyword, source="MacArthur", stringsAsFactors = FALSE
  ))
}
