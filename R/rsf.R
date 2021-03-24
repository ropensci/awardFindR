#' Get Russell Sage foundation awards
#' @param keyword Keyword to query, single string
#' @return a data.frame
#' @export
#' @examples \dontrun{rsf <- rsf_get("ethnography")}
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
  if (length(links)==0) { # No results?
    return(NULL)
  }

  # Getting details require seperate HTTP reqs for each award, ugh
  df <- lapply(links, function(x) {
    award <- request(x, "get")
    award <- rvest::html_node(award, "div.content > header > div.u-nubbed")
    program <- rvest::html_children(award)[1] %>% rvest::html_text(trim=TRUE)
    program <- gsub("\t\t\t\t", "; ", program) # Separate with ;
    title <- rvest::html_children(award)[2] %>% rvest::html_text(trim=TRUE)

    # Extract data from these nasty unstructured divs
    info <- rvest::html_children(rvest::html_children(award)[3])
    info <- data.frame(
      label=rvest::html_text(rvest::html_nodes(info, "strong:first-child")),
      value=rvest::html_text(rvest::html_nodes(info, "strong + div, br + div")),
      stringsAsFactors = FALSE
    )
    awardee <- strsplit(info$value[info$label=="Awarded Scholars: "], ",")
    if (length(awardee)==0) # Try plan B
      awardee <- strsplit(
        info$value[info$label=="Other External Scholars: "], ",")

    pi_name <- trimws(awardee[[1]][1])
    institution <- trimws(awardee[[1]][2])

    year <- as.integer(
      .substr_right(info$value[info$label=="Project Date: "], 4))

    # Remove $ and , in amounts (i.e. $1,000,000)
    amount <- gsub("^\\$|,", "", info$value[info$label=="Award Amount: "])

    data.frame(institution, pi_name, year, amount, title, program,
               id=paste0("RSF", .text_hash(x))) # Return one df line
  })
  df <- do.call(rbind.data.frame, df) # Bind the df lines

  df$keyword <- keyword
  df
}

.rsf_standardize <- function(keywords, from_date, to_date) {
  raw <- lapply(keywords, rsf_get)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    return(NULL)
  }

  # Since the API doesn't have the feature, we'll do date handling here
  year <- NULL # for R CMD check
  raw <- subset(raw, year > format.Date(from_date, "%Y") &
                  year < format.Date(to_date, "%Y"))

  if (nrow(raw)==0) {
    return(NULL) # Empty now after date subsetting?
  }

  with(raw, data.frame(
    institution, pi=pi_name, year, start=NA, end=NA, program, amount,
    id, title, keyword, source="RSF"
  ))
}
