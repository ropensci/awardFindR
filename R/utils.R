#' @importFrom readr read_csv

# Internal, return basic text hash
# *NOT* cryptographic!! this is a toy hasher!
.text_hash <- function(string) {
  hash <- as.integer(charToRaw(string))
  sum(hash)
}

# internal substring from right function
.substr_right <- function(x, n) substr(x, nchar(x)-n+1, nchar(x))

# Make an HTTP request with httr
request <- function(url, method, payload=NULL) {
  if (method=="post" & !is.null(payload)) {
    message(paste("POST", url, "... "), appendLF=FALSE)
    response <- httr::POST(url, body=payload, encode="json")
  } else if (method=="get") {
    message(paste("GET", url, "... "), appendLF=FALSE)
    response <- httr::GET(url)
  } else stop("Invalid request")

  httr::message_for_status(response)
  message() # Because the message above doesn't end w/ a newline
  httr::warn_for_status(response)

  suppressMessages(httr::content(response))
}

rbind.match.columns <- function(frames) {
  frames <- lapply(frames, as.data.frame)

  # Find all unique column names
  fields <- unique(unlist(lapply(frames, names)))

  # Bind the lists into one data.frame and handle missing data
  do.call(rbind, lapply(frames, function(x) {
    data.frame(c(x, vapply(setdiff(fields, names(x)),
                           function(y) NA, c(1))),
               stringsAsFactors = FALSE)
  }))
}

