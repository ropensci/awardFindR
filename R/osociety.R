#' Query and scrape Open Society foundation awards
#' @inheritParams get_ssrc
#' @return a data.frame
#' @export
#' @examples osociety <- get_osociety("qualitative", 2016, 2019)
get_osociety <- function(keyword, from_year, to_year, verbose=FALSE) {
  base_url <- "https://www.opensocietyfoundations.org/grants/past?"
  query <- paste0("xhr=1&",
                  "filter_keyword=", gsub(" ", "%20", keyword),
                  "&filter_year=",
                  paste(from_year:to_year, collapse="%2C")) # URL escape

  url <- paste0(base_url, query)
  response <- request(url, "get", verbose)

  results <- xml2::xml_find_all(response, "//div[@data-grants-database-single]")
  if (length(results)==0) {
    return(NULL) # No results?
  }

  # Create object to iteratively add pages of results to
  all_results <- list()

  # Iterate through pages
  page <- 1
  while (length(results) > 0) {
    page_url <- paste0(url, '&page=', page)

    response <- request(page_url, "get", verbose)

    results <- xml2::xml_find_all(response, "//div[@data-grants-database-single]")

    results <- lapply(results, function(entry) {
      institution <- xml2::xml_text(xml2::xml_find_first(entry, ".//h2"))
      # Remove trailing and leading whitespace
      institution <- gsub("^\\s+|\\s+$", "", institution)

      id <- xml2::xml_text(xml2::xml_find_first(entry, ".//@id"))
      year <- xml2::xml_integer(
        xml2::xml_find_first(entry, ".//span[@class='a-grantsDatabase__value']"))

      amount_xpath <- paste0(".//span[@class='a-grantsDatabase__value ",
                             "a-grantsDatabase__value--amount']")
      amount <- xml2::xml_text(
        xml2::xml_find_first(entry, amount_xpath))
      # Remove $ and , in amounts (i.e. $1,000,000)
      amount <- gsub("^\\$|,", "", amount)

      # Make a data.frame of the labels and values that we can query later
      info <- data.frame(
        name = xml2::xml_text(
          xml2::xml_find_all(entry, ".//span[@class='a-grantsDatabase__label']")),

        value = xml2::xml_text(
          xml2::xml_find_all(entry, ".//p[@class='a-grantsDatabase__text']")))

      # Remove trailing and leading whitespace
      info$value <- gsub("^\\s+|\\s+$", "", info$value)
      # Now query it
      program <- info$value[info$name == "Referring Program"][1]
      description <- info$value[info$name == "Description"][1]

      data.frame(institution, year, id, amount, program, description, keyword,
                 stringsAsFactors = FALSE)
    })
    all_results <- append(all_results, results)

    Sys.sleep(3)

    page <- page + 1
  }

  do.call(rbind.data.frame, all_results)
}

.standardize_osociety <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, get_osociety,
                format.Date(from_date, "%Y"), format.Date(to_date, "%Y"),
                verbose)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    message("No results from Open Society")
    return(NULL)
  }

  with(raw, data.frame(
    institution, pi=NA, year, start=NA, end=NA, program,
    amount=as.integer(amount), id, title=description, abstract=NA, keyword,
    source="Open Society", stringsAsFactors = FALSE
  ))
}
