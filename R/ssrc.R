#' Extract details from SSRC webpage entry
#'
#' @param entry XML object passed by ssrc_get()
#'
#' @return A single line of a data.frame
ssrc_get_details <- function(entry) {
  pi_name <- xml2::xml_text(xml2::xml_find_first(entry, ".//h5/a/text()"))
  title <- xml2::xml_text(xml2::xml_find_first(entry, ".//p[@class='l-project-title']/em"))
  program <- xml2::xml_text(xml2::xml_find_first(entry, ".//dl[@class='l-inline-summary']/dd/a"))

  data <- xml2::xml_find_all(entry, ".//dl[@class='l-inline-summary']")
  data <- data.frame(label=xml2::xml_text(xml2::xml_find_all(data, ".//dt/text()")),
                     data=xml2::xml_text(xml2::xml_find_all(data, ".//dd")))

  year <- as.character(data$data[as.character(data$label)=="Year "])

  institution <- data$data[as.character(data$label)=="University/Institution (at time of award)"]
  if (length(institution)==0) {
    institution <- xml2::xml_text(xml2::xml_find_first(entry, ".//p[@class='l-institution']"))
  }

  id <- xml2::xml_text(xml2::xml_find_first(entry, ".//h5/a/@href"))
  id <- regmatches(id, regexpr("/([-A-Z0-9])+/$", id))

  data.frame(pi_name, institution, year, title, program, id, stringsAsFactors = F)
}

#' Scrape SSRC fellowships and grants by keyword and date
#'
#' @param keyword Keyword to query, single string
#' @param from Year to begin search, integer
#' @param to Year to end search, integer
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' ssrc <- ssrc_get("qualitative", 2015, 2016)
ssrc_get <- function(keyword, from, to) {
  base_url <- "https://www.ssrc.org/search/?"

  query <- "t=fellows&=&sort=relevance&fellowship="
  query <- paste0(query, "&q=", keyword)
  # Have to collate years to search by date
  for (year in from:to) query <- paste0(query, "&year[]=", year)

  url <- paste0(base_url, query)

  page <- request(url, "get")

  entries <- xml2::xml_find_all(page, "//li[@class='hit l-fellows-hit']")
  if (length(entries)==0) return(NULL)   # No results?
  df <- lapply(entries, ssrc_get_details)
  df <- do.call(rbind.data.frame, df)

  if (length(entries)==25) { # Need to loop?
    n <- 2
    repeat {
      url <- paste0(base_url, query, "&p=", n)
      page <- request(url, "get")

      entries <- xml2::xml_find_all(page, "//li[@class='hit l-fellows-hit']")
      if (length(entries)==0) break

      temp <- lapply(entries, ssrc_get_details)
      temp <- do.call(rbind.data.frame, temp)

      df <- rbind.data.frame(df, temp)

      n <- n + 1
    }
  }

  return(df)
}
