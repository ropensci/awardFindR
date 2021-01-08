#' Download and query online static grant databases
#'
#' @param queries Vector of keyword strings to search
#' @param from Standard date format to begin search, only year is applied
#' @param to Standard date format to end search, only year is applied
#' @param sources Vector of sources to search. Supports: NEH, Sloan
#' @return A data.frame
static_scrape <- function(queries, from, to, sources) {

  # All implemented static sources only can accommodate year search terms
  from <- as.integer(format.Date(from, "%Y"))
  to <- as.integer(format.Date(to, "%Y"))

  if ("neh" %in% sources) {
    neh <- neh_get(queries, from, to)

    if (!is.null(neh)) {
      # Make a harmonized data.frame
      neh <- with(neh, data.frame(institution=Institution,
                                  pi_name=pi,
                                  pi_email=NA,
                                  start=as.Date(BeginGrant),
                                  end=as.Date(EndGrant),
                                  program=Program,
                                  source="NEH",
                                  id=AppNumber,
                                  keyword=query,
                                  title=ProjectTitle,
                                  stringsAsFactors = FALSE))
    } else {
      message("NOTICE (non-fatal): No NEH results at all")
      neh <- NULL
    }
  } else {
    neh <- NULL
  }

  if ("sloan" %in% sources) {
    sloan <- sloan_get(queries, from, to)

    if (!is.null(sloan)) {
      # Make a harmonized data.frame
      sloan <- with(sloan, data.frame(institution=grantee,
                                      pi_name=pi,
                                      pi_email=NA,
                                      start=NA,
                                      end=NA,
                                      program=program,
                                      source="Sloan",
                                      id=id,
                                      keyword=query,
                                      title=description,
                                      stringsAsFactors = FALSE))
    } else {
      message("NOTICE (non-fatal): No Sloan results at all")
      sloan <- NULL
    }
  } else {
    sloan <- NULL
  }

  full <- rbind.data.frame(neh, sloan)

  return(full)
}

#' Query grant APIs for a single keyword
#'
#' @param query Keyword to search for, single string
#' @param from Search beginning date, standard date format
#' @param to Search end date, standard date format
#' @param sources vector of databases to query. Supported sources: nsf, nih, ies
#' @return A data.frame in wide format
api_scrape_keyword <- function(query, from, to, sources) {

  # Run source routines
  if("nsf" %in% sources) {
    nsf <- nsf_get(query, from, to)

    # Empty results?
    if(!is.null(nsf)) {

      nsf$directorate[nsf$cfdaNumber=="47.075"] <- "SBE"
      nsf$directorate[nsf$cfdaNumber=="47.076"] <- "EHR"

      # Format of harmonized data.frame, should follow across sources
      full <- with(nsf, data.frame(institution=awardeeName,
                                   pi_name=paste0(piLastName, ", ", piFirstName),
                                   pi_email=piEmail,
                                   start=as.Date(startDate, format="%m/%d/%Y"),
                                   end=as.Date(expDate, format="%m/%d/%Y"),
                                   program=directorate,
                                   source="NSF",
                                   id=id,
                                   keyword=query,
                                   title=title,
                                   stringsAsFactors = FALSE))

    } else {
      message(paste0("NOTICE (non-fatal): NSF query \"", query, "\" returned empty response"))
    }
  }

  # Start NIH block
  if ("nih" %in% sources) {
    nih <- fedreporter_get(query,
                           format.Date(from, "%Y"),
                           format.Date(to, "%Y"),
                           agency = "nih")

    # Make the harmonized data.frame
    if (!is.null(nih)) {
      nih <- with(nih, data.frame(institution=OrgName,
                                  pi_name=ContactPi,
                                  pi_email=NA,
                                  start=as.Date(BudgetStartDate),
                                  end=as.Date(BudgetEndDate),
                                  program=Department,
                                  source="NIH",
                                  id=ProjectNumber,
                                  keyword=query,
                                  title=Title,
                                  stringsAsFactors = FALSE))

      # Merge
      if(exists("full")) {
        full <- merge(full, nih, all=T)
      } else {
        full <- nih
      }

    }  else {
      message(paste0("NOTICE (non-fatal): NIH query \"", query, "\" returned empty response"))
    }
  }

  # Start IES block
  if ("ies" %in% sources) {
    ies <- fedreporter_get(query,
                           format.Date(from, "%Y"),
                           format.Date(to, "%Y"),
                           agency = "ies")

    # Make the harmonized data.frame
    if (!is.null(ies)) {
      ies <- with(ies, data.frame(institution=OrgName,
                                  pi_name=ContactPi,
                                  pi_email=NA,
                                  start=as.Date(BudgetStartDate),
                                  end=as.Date(BudgetEndDate),
                                  program=Department,
                                  source="IES",
                                  id=ProjectNumber,
                                  keyword=query,
                                  title=Title,
                                  stringsAsFactors = FALSE))

      # Merge
      if(exists("full")) {
        full <- merge(full, ies, all=T)
      } else {
        full <- ies
      }

    }  else {
      message(paste0("NOTICE (non-fatal): IES query \"", query, "\" returned empty response"))
    }
  }

  # Did nothing work?
  if (!exists("full")) {
    return(NULL)
  }

  full$institution <- sapply(as.character(full$institution), title_case)
  full$pi_name <- sapply(full$pi_name, title_case)

  return(full)
}

#' Search grant APIs by keywords and date
#'
#' @param queries Keywords to search for, vector
#' @param from Search beginning date, standard date format
#' @param to Search end date, standard date format
#' @param sources vector of databases to query. Supported sources: nsf, nih, ies
#' @return A data.frame in wide format
api_scrape <- function(queries, from, to, sources) {
  # Check passed arguments for sanity
  stopifnot(is.character(queries))
  results <- lapply(queries, api_scrape_keyword, from, to, sources)
  results <- do.call(rbind.data.frame, results)
  return(results)

}
