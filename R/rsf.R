#' Get Russell Sage foundation awards
#' @param keyword Keyword to query, single string
#' @param verbose enable verbose HTTP messages. TRUE/FALSE, default: false
#' @return a data.frame
#' @export
#' @examples \dontrun{rsf <- get_rsf("ethnography")}
get_rsf <- function(keyword, verbose=FALSE) {
  # First we use the general search function to find potential awards
  url <- "https://www.russellsage.org"
  path <- paste0("/search/node/", xml2::url_escape(keyword))

  # Collect all the links, looping through pages as necessary
  links <- c()
  repeat {
    page <- request(paste0(url, path), "get", verbose)
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
    award <- request(x, "get", verbose)
    award <- rvest::html_node(award, "div.content > header > div.u-nubbed")
    program <- rvest::html_children(award)[1] %>% rvest::html_text(trim=TRUE)
    program <- gsub("\t\t\t\t", "; ", program) # Separate with ;
    title <- rvest::html_nodes(award, ".Post__titleGroup.u-margin__10") %>% rvest::html_text(trim = TRUE)

    # Extract data from these nasty unstructured divs
    info <- rvest::html_children(rvest::html_nodes(award, ".u-text--meta.u-margin__20"))

    # Extract all strong nodes with relevant headings
    strong_nodes <- rvest::html_nodes(info, "strong")

    # Get the nodes for the relevant sections
    awarded_scholars_node <- strong_nodes[rvest::html_text(strong_nodes) == "Awarded Scholars: "]
    other_external_scholars_node <- strong_nodes[rvest::html_text(strong_nodes) == "Other External Scholars: "]

    # Function to get siblings until the next strong tag
    get_siblings_until_next_strong <- function(node) {
      siblings <- xml2::xml_siblings(node)
      siblings <- siblings[which(rvest::html_name(siblings) != "strong")]
      return(siblings)
    }

    # Extract the relevant text underneath the strong tags
    awarded_scholars_siblings <- get_siblings_until_next_strong(awarded_scholars_node)
    other_external_scholars_siblings <- get_siblings_until_next_strong(other_external_scholars_node)

    # Extract the text from each filtered node
    awardees_institutions <- c(
      sapply(awarded_scholars_siblings, rvest::html_text),
      sapply(other_external_scholars_siblings, rvest::html_text)
    )

    # Remove whitespace
    awardees_institutions <- trimws(awardees_institutions)
    awardees_institutions <- awardees_institutions[awardees_institutions != ""]


    # Function to split the text into awardee and institution
    split_results <- lapply(awardees_institutions, function(text) {
      # Find the position of the first comma
      first_comma <- regexpr(",", text)

      # Split the text into awardee and institution
      awardee <- trimws(substr(text, 1, first_comma - 1))
      institution <- trimws(substr(text, first_comma + 1, nchar(text)))


      return(list(awardee = awardee, institution = institution))
    })

    # Separate awardees and institutions into two lists
    pi_names <- paste(sapply(split_results, function(x) x$awardee), collapse = "; ")
    institution <- paste(sapply(split_results, function(x) x$institution), collapse = "; ")

    # Get date/amount
    info <- data.frame(
      label=rvest::html_text(rvest::html_nodes(info, "strong:first-child")),
      value=rvest::html_text(rvest::html_nodes(info, "strong + div, br + div")),
      stringsAsFactors = FALSE
    )

    year <- as.integer(
      .substr_right(info$value[info$label=="Project Date: "], 4))

    # Remove $ and , in amounts (i.e. $1,000,000)
    amount <- gsub("^\\$|,", "", info$value[info$label=="Award Amount: "])

    data.frame(institution, pi_names, year, amount, title, program,
               id=paste0("RSF", .text_hash(x))) # Return one df line
  })
  df <- do.call(rbind.data.frame, df) # Bind the df lines

  df$keyword <- keyword
  df
}

.standardize_rsf <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, get_rsf, verbose)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    message("No results from RSF")
    return(NULL)
  }

  # Since the API doesn't have the feature, we'll do date handling here
  year <- NULL # for R CMD check
  raw <- subset(raw, year > format.Date(from_date, "%Y") &
                  year < format.Date(to_date, "%Y"))

  if (nrow(raw)==0) { # Empty now after date subsetting?
    message("No results from RSF")
    return(NULL)
  }

  with(raw, data.frame(
    institution, pi=pi_names, year, start=NA, end=NA, program, amount,
    id, title, abstract=NA, keyword, source="RSF"
  ))
}
