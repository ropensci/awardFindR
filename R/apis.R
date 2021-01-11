#' Download and query online static grant databases
#'
#' @param queries Vector of keyword strings to search
#' @param from Standard date format to begin search, only year is applied
#' @param to Standard date format to end search, only year is applied
#' @param sources Vector of sources to search. Supports: NEH, Sloan
#' @return A data.frame
award_scrape <- function(queries, from, to, sources) {

  # All implemented static sources only can accommodate year search terms
  from_yr <- as.integer(format.Date(from, "%Y"))
  to_yr <- as.integer(format.Date(to, "%Y"))

  # start NEH block
  if ("neh" %in% sources) {
    neh <- neh_get(queries, from_yr, to_yr)

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

  # Start sloan block
  if ("sloan" %in% sources) {
    sloan <- sloan_get(queries, from_yr, to_yr)

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

  # Run the API queries, which require one term at a time
  apis <- lapply(queries, award_scrape_api, from, to, sources)
  apis <- do.call(rbind.data.frame, apis)

  full <- rbind.data.frame(neh, sloan, apis)
  if (nrow(full)==0) {
    return(NULL)
  }

  return(full)
}

#' Query grant APIs for a single keyword
#'
#' @param query Keyword to search for, single string
#' @param from Search beginning date, standard date format
#' @param to Search end date, standard date format
#' @param sources vector of databases to query. Supported sources: nsf, nih, ies
#' @return A data.frame in wide format
award_scrape_api <- function(query, from, to, sources) {

  # Run source routines
  if("nsf" %in% sources) {
    nsf <- nsf_get(query, from, to)

    # Empty results?
    if(!is.null(nsf)) {

      nsf$directorate[nsf$cfdaNumber=="47.075"] <- "SBE"
      nsf$directorate[nsf$cfdaNumber=="47.076"] <- "EHR"

      # Format of harmonized data.frame, should follow across sources
      nsf <- with(nsf, data.frame(institution=awardeeName,
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
      nsf <- NULL
    }
  } else {
    nsf <- NULL
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
                                  start=as.Date(BudgetStartDate), # Or ProjectStartDate??
                                  end=as.Date(BudgetEndDate), # Or projectEndDate??
                                  program=Department,
                                  source="NIH",
                                  id=ProjectNumber,
                                  keyword=query,
                                  title=Title,
                                  stringsAsFactors = FALSE))

    }  else {
      message(paste0("NOTICE (non-fatal): NIH query \"", query, "\" returned empty response"))
      nih <- NULL
    }
  } else {
    nih <- NULL
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
                                  start=as.Date(ProjectStartDate),
                                  end=as.Date(ProjectEndDate),
                                  program=Department,
                                  source="IES",
                                  id=ProjectNumber,
                                  keyword=query,
                                  title=Title,
                                  stringsAsFactors = FALSE))
    }  else {
      message(paste0("NOTICE (non-fatal): IES query \"", query, "\" returned empty response"))
      ies <- NULL
    }
  } else {
    ies <- NULL
  }

  full <- rbind.data.frame(nsf, nih, ies)
  if (nrow(full)==0) {
    return(NULL)
  }

  full$institution <- sapply(as.character(full$institution), title_case)
  full$pi_name <- sapply(full$pi_name, title_case)
  return(full)
}
