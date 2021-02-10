#' Search MacArthur foundation for awards
#'
#' @param keyword Keyword to query, single string
#' @param from_date Date object to begin search
#' @param to_date Date object to end search
#' @return a data.frame
#' @export
#' @examples macarthur <- macarthur_get("qualitative", "1999-01-01", "2020-01-01")
macarthur_get <- function(keyword, from_date, to_date) {
  url <- "https://searchg2.crownpeak.net/live-macfound-rt/select?"
  parameters <- paste0("q=", xml2::url_escape(keyword), "&wt=xml&start=0&rows=100")
  # I really don't know what most of this does below, but it was part of the
  # web interface's API query, so I'm leaving it in for now.
  extra <- "&echoParams=explicit&fl=*&defType=edismax&fq=custom_s_template:%22grant%20detail%22&sort=score%20desc&qf=custom_t_title%20custom_t_description%20custom_t_name&indent=true&json.wrf=searchg2_5445608496089546"

  query_url <- paste0(url, parameters, extra)
  response <- request(query_url, "get")

  if (xml2::xml_integer(xml2::xml_find_first(response, "/response/result/@numFound"))==0)
    return(NULL) # No results?

  results <- xml2::xml_children(xml2::xml_children(response)[2])
  results <- lapply(results, function(x) {
    id <- xml2::xml_text(xml2::xml_find_first(x, ".//str[@name='id']"))
    institution <- xml2::xml_text(xml2::xml_find_first(x, ".//str[@name='custom_s_name']"))
    title <- xml2::xml_text(xml2::xml_find_first(x, ".//str[@name='custom_s_title']"))
    program <- xml2::xml_text(xml2::xml_find_first(x, ".//str[@name='custom_s_program_area_code']"))

    amount <- xml2::xml_integer(xml2::xml_find_first(x, ".//str[@name='custom_s_amount']"))

    approved <- xml2::xml_text(xml2::xml_find_first(x, ".//str[@name='custom_s_grant_approved_date']"))
    start <- xml2::xml_text(xml2::xml_find_first(x, ".//str[@name='custom_s_start_date']"))
    end <- xml2::xml_text(xml2::xml_find_first(x, ".//str[@name='custom_s_end_date']"))

    data.frame(id, institution, approved, start, end, amount, program, title,
               stringsAsFactors = F)
  })
  df <- do.call(rbind.data.frame, results)

  #scrap the exact time and format as date
  df$approved <- substr(df$approved, 1, 10)
  df$start <- substr(df$start, 1, 10)
  df$end <- substr(df$end, 1, 10)

  approved <- NULL # For R CMD check
  df <- subset(df, as.Date(approved) > from_date & as.Date(approved) < to_date) # Date limit
  if (nrow(df)==0) return(NULL) # No results after date limiting?

  df$year <- format.Date(df$approved, "%Y")
  return(df)
}

#' Standardize award results from the MacArthur Foundation
#' @param keyword Single keyword to query
#' @param from_date Date object to begin search
#' @param to_date Date object to end search
#' @return a standardized data.frame
macarthur_standardize <- function(keyword, from_date, to_date) {
  macarthur <- macarthur_get(keyword, from_date, to_date)
  if (is.null(macarthur)) return(NULL)
  with(macarthur, data.frame(
    institution, pi=NA, year=format.Date(approved, "%Y"),
    start=substr(start, 1, 10), end=substr(end, 1, 10),
    program, amount, id, title, keyword, source="MacArthur",
    stringsAsFactors = FALSE
  ))
}
