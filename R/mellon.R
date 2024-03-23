#' Search the Andrew W. Mellon Foundation grant database
#' @inheritParams get_ssrc
#' @return a data.frame
#' @export
#' @examples
#' mellon <- get_mellon("qualitative", 2013, 2021)
get_mellon <- function(keyword, from_year, to_year, verbose = FALSE) {
  base_url <- "https://www.mellon.org/api/graphql"

  # Set default response limit
  RESPONSE_LIMIT <- 100

  # Define bulk query statement for getting grants associated with a keyword
  bulk_query_statement <- "
  query GrantFilterQuery($term: String!, $limit: Int!, $offset: Int!, $sort: SearchSort, $amountRanges: [FilterRangeInput!], $grantMakingAreas: [String!], $country: [String!], $pastProgram: Boolean, $yearRange: FilterRangeInput, $years: [Int!], $state: [String!], $ideas: [String!], $features: [String!]) {
      grantSearch(
          term: $term
          limit: $limit
          offset: $offset
          sort: $sort
          filter: {pastProgram: $pastProgram, grantMakingAreas: $grantMakingAreas, country: $country, years: $years, yearRange: $yearRange, amountRanges: $amountRanges, state: $state, ideas: $ideas, features: $features}
      ) {
          ...GrantSearchResults
      }
  }

  fragment GrantSearchResults on GrantSearchResultWithTotal {
      entities {
          data {
              title
              grantMakingArea
              description
              dateAwarded
              id
              grantee
              country
              state
          }
      }
      totalCount
  }
  "

  # Create a default payload that we can later edit for making specific requests
  # using the above query
  default_bulk_payload <- list(
    operationName = "GrantFilterQuery",
    variables = list(
      limit = RESPONSE_LIMIT,
      offset = 0,
      term = keyword,
      sort = "MOST_RELEVANT",
      years = as.list(from_year:to_year),
      grantMakingAreas = list(),
      ideas = list(),
      pastProgram = FALSE,
      amountRanges = list(),
      country = list(),
      state = list(),
      features = list()
    ),
    "query" = bulk_query_statement
  )


  # Create a query statement for getting a specific grant's amount,
  # which isn't available in the bulk query
  single_query_statement <- "
  query($grantId: String!) {
      grantDetails(grantId: $grantId) {
          grant {
              amount
          }
      }
  }
  "

  # Create a default payload for single-grant query
  default_single_payload <- list(
    variables = list(
      grantId = ""
    ),
    "query" = single_query_statement
  )

  # Create a payload for getting total num grants
  total_grants_payload <- default_bulk_payload
  total_grants_payload$variables$limit <- 1

  # Make request
  total_grants_content <- request(
    url = base_url,
    method = "post",
    payload = total_grants_payload,
    verbose = verbose
  )

  # Get the count of total number of grants
  total_grants <- total_grants_content$data$grantSearch$totalCount

  # Check if there are no results, if so, return NULL now
  if (total_grants == 0) {
    return(NULL)
  }

  # Loop through and get results in batches
  results <- list()
  for (offset in seq(0, total_grants, by = RESPONSE_LIMIT)) {
    # Be nice to the API
    Sys.sleep(3)

    # Make payload for the offset
    offset_payload <- default_bulk_payload
    offset_payload$variables$offset <- offset

    # Make request
    offset_content <- request(
      url = base_url,
      method = "post",
      payload = offset_payload,
      verbose = verbose
    )

    # Append to other results
    results <- append(results, offset_content$data$grantSearch$entities)
  }

  # Combine responses
  results <- do.call(
    rbind,
    lapply(
      results,
      function(entry) {
        # Fill nulls
        entry$data <- lapply(entry$data, function(x) if (is.null(x)) NA else x)

        # Convert list to data.frame
        data.frame(entry$data)
      }
    )
  )

  # Get the amount for each grant, which isn't available in the bulk query
  results$amount <- unlist(
    lapply(
      results$id,
      function(id) {
        # Be nice to the API
        Sys.sleep(3)

        # Make payload for the ID
        id_payload <- default_single_payload
        id_payload$variables$grantId <- id

        # Make request
        id_content <- request(
          url = base_url,
          method = "post",
          payload = id_payload,
          verbose = verbose
        )

        # return the amount
        id_content$data$grantDetails$grant$amount
      }
    )
  )

  # Normalize dates
  results$dateAwarded <- as.Date(results$dateAwarded, format = "%Y-%m-%d")

  # Add keyword
  results$keyword <- keyword

  # Condense location (checking for grants without states)
  results$location <- ifelse(
    results$state == "",
    results$country,
    paste(results$state, results$country, sep = ", ")
  )

  # Delete country and state columns
  results$country <- NULL
  results$state <- NULL

  # Rename columns
  names(results)[names(results) == "dateAwarded"] <- "date"
  names(results)[names(results) == "grantee"] <- "institution"
  names(results)[names(results) == "grantMakingArea"] <- "program"

  # Append grant details to the ID, so it becomes a URL path
  results$id <- paste0("/grant-details/", results$id)

  results
}

.standardize_mellon <- function(keywords, from_date, to_date, verbose) {
  raw <- lapply(keywords, get_mellon,
                format.Date(from_date, "%Y"), format.Date(to_date, "%Y"),
                verbose)
  raw <- do.call(rbind.data.frame, raw)
  if (nrow(raw)==0) {
    message("No results from Mellon")
    return(NULL)
  }

  with(raw, data.frame(
    institution, pi=NA, year=format.Date(date, "%Y"), start=NA, end=NA,
    program, amount, id, title, abstract= description, keyword,
    source="Mellon", stringsAsFactors = FALSE
  ))
}
