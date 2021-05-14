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
    response <- httr::POST(url, body=payload, encode="json",
                           config = httr::config(connecttimeout = 60))
  } else if (method=="get") {
    message(paste("GET", url, "... "), appendLF=FALSE)
    response <- httr::GET(url, config = httr::config(connecttimeout = 60))
  } else stop("Invalid request")

  httr::message_for_status(response)
  message() # Because the message above doesn't end w/ a newline
  httr::warn_for_status(response)

  suppressMessages(httr::content(response))
}
