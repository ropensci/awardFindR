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
  parameters <- paste0("q=", keyword, "&wt=xml&start=0&rows=100")
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
    grantee <- xml2::xml_text(xml2::xml_find_first(x, ".//str[@name='custom_s_name']"))
    title <- xml2::xml_text(xml2::xml_find_first(x, ".//str[@name='custom_s_title']"))
    program <- xml2::xml_text(xml2::xml_find_first(x, ".//str[@name='custom_s_program_area_code']"))

    amount <- xml2::xml_integer(xml2::xml_find_first(x, ".//str[@name='custom_s_amount']"))

    approved <- xml2::xml_text(xml2::xml_find_first(x, ".//str[@name='custom_s_grant_approved_date']"))
    start <- xml2::xml_text(xml2::xml_find_first(x, ".//str[@name='custom_s_start_date']"))
    end <- xml2::xml_text(xml2::xml_find_first(x, ".//str[@name='custom_s_end_date']"))

    data.frame(id, grantee, approved, start, end, amount, program, title,
               stringsAsFactors = F)
  })
  df <- do.call(rbind.data.frame, results)

  #scrap the exact time and format as date
  df$approved <- as.Date(substr(df$approved, 1, 10))
  df$start <- as.Date(substr(df$start, 1, 10))
  df$end <- as.Date(substr(df$end, 1, 10))

  df <- subset(df, approved > from_date & approved < to_date) # Date limit
  if (nrow(df)==0) return(NULL) # No results after date limiting?
  df$year <- format.Date(df$approved, "%Y")
  return(df)
}
