#' Search MacArthur foundation for awards
#' @inheritParams get_nih
#' @return a data.frame
#' @export
#' @examples
#' macarthur <- get_macarthur("qualitative",
#' "1999-01-01", "2020-01-01")
get_macarthur <- function(keyword, from_date, to_date, verbose=FALSE) {
  url <- "https://searchg2.crownpeak.net/live-macfound-rt/select?"
  parameters <- paste0("q=", xml2::url_escape(keyword),
                       "&wt=xml&start=0&rows=100")

  # I really don't know what most of this does below, but it was part of the
  # web interface's API query, so I'm leaving it in for now.
  extra <- paste0("&echoParams=explicit&fl=*&defType=edismax",
  "&fq=custom_s_template:%22grant%20detail%22&sort=score%20desc",
  "&qf=custom_t_title%20custom_t_description%20custom_t_name&indent=true",
  "&json.wrf=searchg2_5445608496089546")

  query_url <- paste0(url, parameters, extra)
  response <- request(query_url, "get", verbose)

  if (xml2::xml_integer(
    xml2::xml_find_first(response, "/response/result/@numFound"))==0) {
    return(NULL) # No results?
  }

  results <- xml2::xml_children(xml2::xml_children(response)[2])
  results <- lapply(results, function(x) {
    id <- xml2::xml_text(
      xml2::xml_find_first(x, ".//str[@name='id']"))

    institution <- xml2::xml_text(
      xml2::xml_find_first(x, ".//str[@name='custom_s_name']"))

    title <- xml2::xml_text(
      xml2::xml_find_first(x, ".//str[@name='custom_s_title']"))

    program <- xml2::xml_text(
      xml2::xml_find_first(x, ".//str[@name='custom_s_program_area_code']"))

    amount <- xml2::xml_integer(
      xml2::xml_find_first(x, ".//str[@name='custom_s_amount']"))

    approved <- xml2::xml_text(
      xml2::xml_find_first(x, ".//str[@name='custom_s_grant_approved_date']"))

    start <- xml2::xml_text(
      xml2::xml_find_first(x, ".//str[@name='custom_s_start_date']"))

    end <- xml2::xml_text(
      xml2::xml_find_first(x, ".//str[@name='custom_s_end_date']"))

    data.frame(id, institution, approved, start, end, amount, program, title,
               stringsAsFactors = FALSE)
  })
  df <- do.call(rbind.data.frame, results)

  #scrap the exact time and format as date
  df$approved <- substr(df$approved, 1, 10)
  df$start <- substr(df$start, 1, 10)
  df$end <- substr(df$end, 1, 10)

  approved <- NULL # For R CMD check
  # Date limit
  df <- subset(df, as.Date(approved) > from_date & as.Date(approved) < to_date)
  if (nrow(df)==0) {
    return(NULL) # No results after date limiting?
  }

  df$year <- format.Date(df$approved, "%Y")
  df$keyword <- keyword

  df
}

.standardize_macarthur <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, get_macarthur, from_date, to_date, verbose)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    message("No results from MacArthur")
    return(NULL)
  }

  with(raw, data.frame(
    institution, pi=NA, year=format.Date(approved, "%Y"),
    start=substr(start, 1, 10), end=substr(end, 1, 10),
    program, amount, id, title, abstract=NA, keyword, source="MacArthur",
    stringsAsFactors = FALSE
  ))
}
