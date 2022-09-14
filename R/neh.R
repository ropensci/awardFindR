#' Get relevant awards from NEH
#' @param keyword Keyword to query
#' @param from_year Beginning year to search
#' @param to_year Ending year to search
#' @param verbose enable verbose HTTP messages. TRUE/FALSE, default: false
#' @return A raw data.frame with the relevant results from NEH
#' @export
#' @examples
#' \dontrun{neh <- get_neh("ethnography", 2018, 2020)}
get_neh <- function(keyword, from_year, to_year, verbose=FALSE) {
  base_url <- "https://securegrants.neh.gov"
  query <- paste0("/publicquery/main.aspx?q=1&n=0&o=0&k=1&kv=",
                  gsub(" ", "+", keyword),
                  "&kj=phrase&w=1&f=0&s=0&cd=0&p=0&d=0&at=0&y=1&yf=",
                  from_year, "&yt=", to_year,
                  "&prd=0&cov=0&prz=0&wp=0&ca=0&or=DESC&ob=year")
  url <- paste0(base_url, query)

  session <- rvest::session(base_url)
  if (verbose==TRUE) message(paste("GET", url, "... "), appendLF = FALSE)
  page <- rvest::session_jump_to(session, url)
  if (verbose==TRUE) {
    httr::message_for_status(page)
    message()
  }

  page <- rvest::read_html(page)

  form <- rvest::html_form(page, NULL)[[1]]
  # Need to add this field to the form, which isn't straightforward
  form$fields$`__EVENTTARGET` <- form$fields$`__VIEWSTATE`
  form$fields$`__EVENTTARGET`$name <- "__EVENTTARGET"
  form$fields$`__EVENTTARGET`$attr$name <- "__EVENTTARGET"
  form$fields$`__EVENTTARGET`$attr$id <- "__EVENTTARGET"
  form$fields$`__EVENTTARGET`$value <- "lbSaveExcel"
  form$fields$`__EVENTTARGET`$attr$value <- "lbSaveExcel"

  if (verbose==TRUE) message(paste("POST", url, "... "), appendLF = FALSE)
  xlsx_response <- rvest::session_submit(session, form)
  if (verbose==TRUE) {
    httr::message_for_status(xlsx_response)
    message()
  }

  # Check if there's no results
  if (xlsx_response$response$url ==
      paste0("https://securegrants.neh.gov/PublicQuery/error.htm",
             "?aspxerrorpath=/publicquery/main.aspx")) {
    return (NULL)
    }

  # This returns a binary xlsx file, which needs to be saved and loaded
  xlsx_path <- withr::local_tempfile()
  writeBin(xlsx_response$response$content, xlsx_path)

  results <- readxl::read_xlsx(xlsx_path)
  results$keyword <- keyword

  results
}

.standardize_neh <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, get_neh,
                 format.Date(from_date, "%Y"), format.Date(to_date, "%Y"),
                 verbose)
  raw <- do.call(rbind.data.frame, raw)

  if (nrow(raw)==0) {
    message("No results from NEH")
    return(NULL)
  }

  dates <- strsplit(raw$GrantPeriod, " ")
  dates <- data.frame(start=unlist(lapply(dates, `[`, 1)),
                      end=unlist(lapply(dates, `[`, 3)))

  with(raw, data.frame(
    institution=Institution, pi=paste(PDFirstname, PDLastname),
    year=YearAwarded, start=dates$start, end=dates$end,
    program=ProgramName, amount=as.numeric(OriginalAmount),
    id=as.character(ApplicationNumber), title=ProjectTitle,
    abstract=ProjectDesc, keyword, source="NEH", stringsAsFactors = FALSE
  ))
}
