#' Get Russell Sage foundation awards
#' @param keyword Keyword to query, single string
#' @return a data.frame
#' @export
rsf_get <- function(keyword) {
  # First we use the general search function to find potential awards
  url <- "https://www.russellsage.org"
  path <- paste0("/search/node/", keyword)

  # Collect all the links, looping through pages as necessary
  links <- c()
  repeat {
    page <- request(paste0(url, path), "get")
    links <- c(links,
               rvest::html_attr(rvest::html_nodes(page, "li.search-result a"),
                                "href"))

    if (length(links)==0) return(NULL) # No results at all?
    path <- rvest::html_attr(rvest::html_node(page, "li.Pagination__next > a"),
                             "href")
    if (is.na(path)) break # Hit the last page?
  }

  # This is the general site-wide search feature, so pick only the awards
  links <- links[grepl("awarded-project", links)]

  # Getting details require seperate HTTP reqs for each award, ugh
  df <- lapply(links, function(x) {
    award <- request(x, "get")
    award <- rvest::html_node(award, "div.content > header > div.u-nubbed")
    program <- trimws(rvest::html_text(rvest::html_children(award)[1]))
    title <- trimws(rvest::html_text(rvest::html_children(award)[2]))

    # Extract data from these nasty unstructured divs
    info <- rvest::html_children(rvest::html_children(award)[3])
    info <- rvest::html_text(rvest::html_nodes(info, "strong + div, br + div"))
    awardee <- strsplit(info[1], ",")
    pi_name <- trimws(awardee[[1]][1])
    institution <- awardee[[1]][2]

    year <- substr_right(info[2], 4)
    amount <- gsub("^\\$|,", "", info[3]) # Remove $ and , in amounts (i.e. $1,000,000)

    data.frame(institution, pi_name, year, amount, title, program,
               id=paste0("RSF", text_hash(x))) # Return one df line
  })
  df <- do.call(rbind.data.frame, df) # Bind the df lines

  df$keyword <- keyword
  return(df)
}

#' Standardize Russell Sage foundation award results
#' @param keywords Vector of keywords to search
#' @param from_date Beginning date object to search
#' @param to_date Ending date object to search
#' @return a standardized data.frame
rsf_standardize <- function(keywords, from_date, to_date) {
  raw <- lapply(keywords, rsf_get)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) return(NULL)

  # Since the API doesn't have the feature, we'll do date handling here
  year <- NULL # for R CMD check
  raw <- subset(raw,
                as.integer(year) > format.Date(from_date, "%Y") &
                                                 as.integer(year) < format.Date(to_date, "%Y"))
  if (nrow(raw)==0) return(NULL) # Empty now after date subsetting?

  with(raw, data.frame(
    institution, pi=pi_name, year, start=NA, end=NA, program, amount,
    id, title, keyword, source="RSF"
  ))
}
