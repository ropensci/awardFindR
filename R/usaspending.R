#' Search the USAspending database
#' @param keywords Vector of strings to search
#' @inheritParams get_nih
#' @return a data.frame
#' @export
#' @examples
#' \dontrun{
#' results <- usaspend_get(c("qualitative", "interview"),
#'  "2019-01-01", "2020-01-01")
#' }
get_usaspend <- function(keywords, from_date, to_date, verbose) {
  url <- "https://api.usaspending.gov/api/v2/search/spending_by_award/"

  # Check if from date is prior to 2007-10-01, and if so raise a warning
  # message. Earlier data is not available through this end point, but if you
  # search for it the warning message from the API gets hidden.
  if (from_date < "2007-10-01") {
    warning("USAspending data only available from 2007-10-01. Please adjust the from_date.")
    return(NULL)
  }

  payload <- list(
    fields=c("Award ID", "Recipient Name", "Description",
             "Start Date", "End Date", "Award Amount",
             "Awarding Agency", "Awarding Sub Agency",
             "Funding Agency", "Funding Sub Agency"),
    filters=list(
      agencies=list(
        # These should be agencies that are not covered by:
        # Federal Reporter, NIH RePORTER or NSF
        # because the data is limited from USAspending compared to other sources
        list(name="Department of Agriculture", tier="toptier", type="awarding"),
        list(name="Department of Defense", tier="toptier", type="awarding"),
        list(name="National Aeronautics and Space Administration", tier="toptier",
             type="awarding"),
        list(name="Environmental Protection Agency", tier="toptier",
             type="awarding"),
        list(name="Department of Education", tier="toptier", type="awarding"),
        list(name="Institute of Museum and Library Services", tier="toptier",
             type="awarding"),
        list(name="Smithsonian Institution", tier="toptier", type="awarding"),
        list(name="Department of Commerce", tier="toptier", type="awarding"),
        list(name="Department of Education", tier="toptier", type="awarding"),
        list(name="Department of the Interior", tier="toptier",
             type="awarding")),
      award_type_codes=c("02", "03", "04", "05"), # Only grants
      keywords=as.list(keywords),
      recipient_type_names=list("higher_education"),
      # An array syntax quirk in the API demands this double list(list())
      time_period=list(list(start_date=from_date, end_date=to_date))),
    limit=50, page=1, order="desc", subawards="false"
  )

  # query API
  response <- request(url, "post", verbose, payload)

  # Unload awards
  awards <- response$results
  if (length(awards)==0) {
    return(NULL)
  }

  # Replace NULL with NA
  awards <- lapply(awards, lapply, function(x)ifelse(is.null(x), NA, x))
  awards <- do.call(rbind.data.frame, awards)

  # Need to loop queries?
  while (response$page_metadata$hasNext == TRUE) {
    payload$page <- response$page_metadata$page + 1
    response <- request(url, "post", verbose, payload)

    temp <- response$results
    temp <- lapply(temp, lapply, function(x)ifelse(is.null(x), NA, x))
    temp <- do.call(rbind.data.frame, temp)

    awards <- rbind.data.frame(awards, temp)
  }


  # Create payload for retrieving all records with the same IDs as records above.
  # When multiple sub-agencies fund an award, sometimes multiple records with the
  # same Award.ID will be returned, (e.g. one for the Navy and one for the Army).
  # This searches for all award IDs we've already found, and ensures we aren't
  # leaving out any info about an award from sub-agencies who weren't found with
  # the first search.
  all_id_payload <- list(
    fields = c("Award ID", "Recipient Name", "Description",
               "Start Date", "End Date", "Award Amount",
               "Awarding Agency", "Awarding Sub Agency",
               "Funding Agency", "Funding Sub Agency"),
    filters = list(
      agencies = list(
        list(name = "Department of Agriculture", tier = "toptier", type = "awarding"),
        list(name = "Department of Defense", tier = "toptier", type = "awarding"),
        list(name = "National Aeronautics and Space Administration", tier = "toptier",
             type = "awarding"),
        list(name = "Environmental Protection Agency", tier = "toptier",
             type = "awarding"),
        list(name = "Department of Education", tier = "toptier", type = "awarding"),
        list(name = "Institute of Museum and Library Services", tier = "toptier",
             type = "awarding"),
        list(name = "Smithsonian Institution", tier = "toptier", type = "awarding"),
        list(name = "Department of Commerce", tier = "toptier", type = "awarding"),
        list(name = "Department of Education", tier = "toptier", type = "awarding"),
        list(name = "Department of the Interior", tier = "toptier",
             type = "awarding")),
      award_type_codes = c("02", "03", "04", "05"), # Only grants
      recipient_type_names = list("higher_education"),
      award_ids = awards$Award.ID
    ),
    limit = 50, page = 1, order = "desc", subawards = "false"
  )

  # query API
  all_id_response <- request(url, "post", verbose, all_id_payload)

  # Unload awards
  all_id_awards <- all_id_response$results

  # Replace NULL with NA
  all_id_awards <- lapply(all_id_awards, lapply, function(x)ifelse(is.null(x), NA, x))
  all_id_awards <- do.call(rbind.data.frame, all_id_awards)

  # Need to loop queries?
  while (all_id_response$page_metadata$hasNext == TRUE) {
    payload$page <- all_id_response$page_metadata$page + 1
    all_id_response <- request(url, "post", verbose, payload)

    temp <- all_id_response$results
    temp <- lapply(temp, lapply, function(x)ifelse(is.null(x), NA, x))
    temp <- do.call(rbind.data.frame, temp)

    all_id_awards <- rbind.data.frame(all_id_awards, temp)
  }

  awards <- rbind(awards, all_id_awards)

  # Deduplicate
  awards <- awards[!duplicated(awards),]

  # Merge any awards that were granted by multiple subagencies under the same Award.ID
  award_sums <- stats::aggregate(Award.Amount ~ Award.ID, data = awards, FUN = sum, na.action = na.pass) # Calculate total award amount
  award_mins <- stats::aggregate(Start.Date ~ Award.ID, data = awards, FUN = min, na.action = na.pass) # Start date is min start date for any sub-agency
  award_maxes <- stats::aggregate(End.Date ~ Award.ID, data = awards, FUN = max, na.action = na.pass) # End date is max end date for any sub-agency
  awards <- stats::aggregate(awards, by=list(awards$Award.ID),
                             FUN=function(x) {paste(sort(unique(x)), collapse = "; ") }) # Concatenate all fields
  # Drop the concatenated award amounts/start dates/end dates and merge to the correct ones
  awards <- merge(awards[,-which(names(awards) %in% c("Group.1","Award.Amount"))],
                  award_sums, by="Award.ID")
  awards <- merge(awards[, -which(names(awards) %in% c("Start.Date"))],
                award_mins, by = "Award.ID")
  awards <- merge(awards[, -which(names(awards) %in% c("End.Date"))],
                award_maxes, by = "Award.ID")


  awards
}

.standardize_usaspend <- function(keywords, from_date, to_date, verbose) {
  raw <- get_usaspend(keywords, from_date, to_date, verbose)
  if (is.null(raw)) {
    message("No results from USAspending")
    return(NULL)
  }

  with(raw, data.frame(
    institution=Recipient.Name, pi=NA, year=substr(Start.Date, 1, 4),
    start=Start.Date, end=End.Date, program=Awarding.Agency,
    amount=as.integer(Award.Amount), id=Award.ID, title=Description,
    abstract=NA, keyword=NA, source="USASpending", stringsAsFactors = FALSE
  ))
}
