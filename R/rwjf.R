#' Get Robert Wood Johnson foundation awards
#' @inheritParams get_ssrc
#' @return a data.frame
#' @export
#' @examples
#' rwjf <- get_rwjf("qualitative data analysis", 2014, 2014)
get_rwjf <- function(keyword, from_year, to_year, verbose=FALSE) {
  url <- paste0("https://www.rwjf.org/content/rwjf-web/us/en/_jcr_content.grants.json?",
  "k=", gsub(" ", "%20", keyword) , "&start=", from_year, "&end=", to_year,
  # Extra junk
  "&amt=-1&active=true&closed=true&featured=true&t=&m=",
  "&sortBy=year&ascending=false&fundid=")

  page <- 1
  response <- request(paste0(url, "&s=", page), "get", verbose)
  if (response$totalResults==0) {
    return(NULL) # No results?
  }

  full <- list() # Placeholder
  full[[page]] <- response$results
  while (page < response$totalPages) { # HTTP get all the pages
    page <- page + 1 # Next page
    response <- request(paste0(url, "&s=", page), "get", verbose)
    full[[page]] <- response$results
  }

  # Process the entries
  full <- lapply(full, function(p) { # P for page, x for individual entries
    step <- lapply(p, function(x) {
      contacts <- do.call(rbind.data.frame, x$contact)
      director <- paste(contacts$name[contacts$title=="Project Director"],
                        collapse="; ")
      if (director=="") director <- NA

      with(x, data.frame(
        title, amountAwarded, dateAwarded, grantNumber, startDate, endDate,
        director, description, orgName=granteeInfo$orgName,
        stringsAsFactors = FALSE
      ))
    })
    do.call(rbind.data.frame, step) # Assemble the page
  })
  full <- do.call(rbind.data.frame, full) # Assemble the full data.frame

  # Is the keyword actually in the description??
  full <- full[grepl(keyword, full$description, ignore.case = TRUE), ]
  if (nrow(full)==0) {
    return(NULL)
  }

  full$dateAwarded <- as.Date(as.POSIXct(full$dateAwarded/1000,
                                         origin="1970-01-01"))

  full$startDate <- as.Date(as.POSIXct(full$startDate/1000,
                                       origin="1970-01-01"))

  full$endDate <- as.Date(as.POSIXct(full$endDate/1000, origin="1970-01-01"))

  full$keyword <- keyword

  full
}

.standardize_rwjf <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, get_rwjf,
                format.Date(from_date, "%Y"), format.Date(to_date, "%Y"),
                verbose)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    message("No results from RWJF")
    return(NULL)
  }

  with(raw, data.frame(
    institution=orgName, pi=director, year=format.Date(dateAwarded, "%Y"),
    start=as.character(startDate), end=as.character(endDate),
    program=NA, amount=amountAwarded, id=grantNumber, title,
    abstract=description, keyword, source="RWJF", stringsAsFactors = FALSE
  ))
}
