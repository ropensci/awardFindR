#' Query and scrape Open Society foundation awards
#'
#' @param keyword Keyword, single string
#' @param from Year to begin search, integer
#' @param to Year to end search, integer
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' osociety <- osociety_get("qualitative", 2016, 2019)
osociety_get <- function(keyword, from, to) {
  base_url <- "https://www.opensocietyfoundations.org/grants/past?"
  query <- paste0("xhr=1&", # filter_program=open-society-fellowship%2Chesp%2Cscholarship-programs&",
                  "filter_keyword=", keyword,
                  "&filter_year=", paste(from:to, collapse="%2C")) # URL escape

  url <- paste0(base_url, query)
  response <- request(url, "get")

  results <- xml2::xml_find_all(response, "//div[@data-grants-database-single]")
  if (length(results)==0) return(NULL) # No results?

  results <- lapply(results, function(entry) {
    institution <- xml2::xml_text(xml2::xml_find_first(entry, ".//h2"))
    institution <- gsub("^\\s+|\\s+$", "", institution)   # Remove trailing and leading whitespace

    id <- xml2::xml_text(xml2::xml_find_first(entry, ".//@id"))
    year <- xml2::xml_integer(xml2::xml_find_first(entry, ".//span[@class='a-grantsDatabase__value']"))

    amount <- xml2::xml_text(xml2::xml_find_first(entry, ".//span[@class='a-grantsDatabase__value a-grantsDatabase__value--amount']"))
    amount <- gsub("^\\$|,", "", amount) # Remove $ and , in amounts (i.e. $1,000,000)

    # Make a data.frame of the labels and values that we can query for data later
    info <- data.frame(name=xml2::xml_text(xml2::xml_find_all(entry, ".//span[@class='a-grantsDatabase__label']")),
                       value=xml2::xml_text(xml2::xml_find_all(entry, ".//p[@class='a-grantsDatabase__text']")))
    info$value <- gsub("^\\s+|\\s+$", "", info$value)   # Remove trailing and leading whitespace
    # Now query it
    program <- info$value[info$name=="Referring Program"][1]
    description <- info$value[info$name=="Description"][1]

    data.frame(institution, year, id, amount, program, description, stringsAsFactors = F)
  })

  do.call(rbind.data.frame, results)
}
