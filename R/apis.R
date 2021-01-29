#' Download and query online grant databases
#'
#' This functions as the glue code between individual API routines and the top level.
#' After running each individual source routine,
#' it renames columns in the data.frames so that we can rbind.data.frame() everything together.
#' Intended to be run through the awardFindR() routine, which handles all input validation.
#'
#' @param queries Vector of keyword strings to search
#' @param sources Vector of sources to search
#' @param from Standard date format to begin search, only year is applied
#' @param to Standard date format to end search, only year is applied
#' @return A data.frame
award_scrape <- function(queries, sources, from, to) {
  from_yr <- as.integer(format.Date(from, "%Y"))
  to_yr <- as.integer(format.Date(to, "%Y"))

  columns <- c("institution", "pi",
               "start", "end",
               "program", "source",
               "amount", "id",
               "keyword", "title")

  if ("neh" %in% sources) {
    neh <- neh_get(queries, from_yr, to_yr)
    if (!is.null(neh)) {
      neh$source <- "NEH"
      neh <- neh[, c("Institution", "pi",
                     "BeginGrant", "EndGrant",
                     "Program", "source",
                     "OriginalAmount", "AppNumber",
                     "query", "ProjectTitle")]
      names(neh) <- columns
    }
  } else neh <- NULL

  if ("sloan" %in% sources) {
    sloan <- sloan_get(queries, from_yr, to_yr)

    if (!is.null(sloan)) {
      sloan$source <- "Sloan"
      sloan$start <- NA
      sloan$end <- NA
      sloan <- sloan[, c("grantee", "pi",
                         "start", "end",
                         "program", "source",
                         "amount", "id",
                         "query", "description")]
      names(sloan) <- columns
    }
  } else sloan <- NULL

  if ("usaspend" %in% sources) {
    usa <- usaspend_get(queries, from, to)

    if (is.null(usa)) warning("No USAspending results")
    else {
      usa$source <- "USAspending"
      usa$keyword <- NA
      usa$pi <- NA
      usa <- usa[, c("Recipient.Name", "pi",
                     "Start.Date", "End.Date",
                     "Awarding.Sub.Agency", "source",
                     "Award.Amount", "Award.ID",
                     "keyword", "Description")]
      names(usa) <- columns
    }
  } else usa <- NULL

  # Run the API queries, which require one term at a time
  apis <- lapply(queries, award_scrape_api, sources, from, to)
  apis <- do.call(rbind.data.frame, apis)

  full <- rbind.data.frame(neh, sloan, usa, apis)
  if (nrow(full)==0) {
    return(NULL)
  }

  full$institution <- sapply(as.character(full$institution), title_case)
  full$pi <- sapply(full$pi, title_case)
  return(full)
}

#' Query grant APIs for a single keyword
#'
#' Large sources with complex APIs can only support on keyword at a time,
#' and this is a limitation on the source routines, including NSF and the Federal Reporter.
#' To remedy this, this function should to be looped in a wrapper for each individual keyword.
#' Otherwise, this is equivalent in function to award_scrape(),
#' but providing support to different sources.
#'
#' @param query Keyword to search for, single string
#' #' @param sources vector of databases to query
#' @param from Search beginning date, standard date format
#' @param to Search end date, standard date format
#' @return A data.frame
award_scrape_api <- function(query, sources, from, to) {
  from_yr <- as.integer(format.Date(from, "%Y"))
  to_yr <- as.integer(format.Date(to, "%Y"))

  # Run source routines
  if("nsf" %in% sources) {
    nsf <- nsf_get(query, from, to)

    # Empty results?
    if(!is.null(nsf)) {

      nsf$directorate[nsf$cfdaNumber=="47.075"] <- "SBE"
      nsf$directorate[nsf$cfdaNumber=="47.076"] <- "EHR"

      # Format of harmonized data.frame, should follow across sources
      nsf <- with(nsf, data.frame(institution=awardeeName,
                                  pi=paste0(piLastName, ", ", piFirstName),
                                  #pi_email=piEmail,
                                  start=as.Date(startDate, format="%m/%d/%Y"),
                                  end=as.Date(expDate, format="%m/%d/%Y"),
                                  program=directorate,
                                  source="NSF",
                                  # Lower levels deliver factors
                                  amount=as.integer(as.character(estimatedTotalAmt)),
                                  id=id,
                                  keyword=query,
                                  title=title,
                                  stringsAsFactors = FALSE))

    } else {
      warning(paste0("NSF query \"", query, "\" returned empty response"))
      nsf <- NULL
    }
  } else {
    nsf <- NULL
  }


  # Start nih block
  if ("nih" %in% sources) {
    nih <- nih_get(query, from, to)

    # Make the harmonized data.frame
    if (!is.null(nih)) {
      nih <- with(nih, data.frame(institution=org_name,
                                  pi=contact_pi_name,
                                  #pi_email=NA,
                                  start=as.Date(project_start_date),
                                  end=as.Date(project_end_date),
                                  program=agency_code,
                                  source="NIH",
                                  amount=as.integer(award_amount),
                                  id=project_num,
                                  keyword=query,
                                  title=project_title,
                                  stringsAsFactors = FALSE))

    }  else {
      warning(paste0("NIH RePORTER query \"",
                     query, "\" returned empty response"))
      nih <- NULL
    }
  } else {
    nih <- NULL
  }

  # Start fedreporter block
  if ("fedreporter" %in% sources) {
    fedreport <- fedreporter_get(query, from_yr, to_yr)

    # Make the harmonized data.frame
    if (!is.null(fedreport)) {
      fedreport <- with(fedreport, data.frame(institution=orgName,
                                              pi=contactPi,
                                              #pi_email=NA,
                                              start=as.Date(projectStartDate),
                                              end=as.Date(projectEndDate),
                                              program=agency,
                                              source="Federal Reporter",
                                              amount=as.integer(totalCostAmount),
                                              id=projectNumber,
                                              keyword=query,
                                              title=title,
                                              stringsAsFactors = FALSE))

    }  else {
      warning(paste0("Federal Reporter query \"",
                     query, "\" returned empty response"))
      fedreport <- NULL
    }
  } else {
    fedreport <- NULL
  }

  # Begin SSRC block
  if ("ssrc" %in% sources) {
    ssrc <- ssrc_get(query, from_yr, to_yr)

    if (!is.null(ssrc)) {
      ssrc <- with(ssrc, data.frame(institution=institution,
                                    pi=pi_name,
                                    start=NA,
                                    end=NA,
                                    program=program,
                                    source="SSRC",
                                    amount=NA,
                                    id=id,
                                    keyword=query,
                                    title=title,
                                    stringsAsFactors = FALSE))
    } else {
      warning(paste0("SSRC query \"", query, "\" returned empty response"))
      ssrc <- NULL
    }
  } else {
    ssrc <- NULL
  }

  # Begin open philanthropy block
  if ("ophil" %in% sources) {
    ophil <- ophil_get(query, from_yr, to_yr)

    # Make the harmonized data.frame
    if (!is.null(ophil)) {
      ophil <- with(ophil, data.frame(institution=grantee,
                                  pi=NA,
                                  #pi_email=NA,
                                  #start=month,
                                  start=NA,
                                  end=NA,
                                  program=focus,
                                  source="Open Philanthropy",
                                  amount=as.integer(amount),
                                  id=id,
                                  keyword=query,
                                  title=title,
                                  stringsAsFactors = FALSE))
    } else {
      warning(paste0("Open Philanthropy query \"",
                     query, "\" returned empty response"))
      ophil <- NULL
    }
  } else {
    ophil <- NULL
  }

  # Start Mellon block
  if ("mellon" %in% sources) {
    mellon <- mellon_get(query, from_yr, to_yr)

    if (!is.null(mellon)) {
      mellon <- with(mellon, data.frame(institution=institution,
                                        pi=NA,
                                        start=NA,
                                        end=NA,
                                        program=program,
                                        source="Mellon",
                                        amount=as.integer(amount),
                                        id=id,
                                        keyword=query,
                                        title=description,
                                        stringsAsFactors = FALSE))
    } else {
      warning(paste0("Mellon query \"", query, "\" returned empty response"))
      mellon <- NULL
    }
  } else {
    mellon <- NULL
  }

  # Start gates block
  if ("gates" %in% sources) {
    gates <- gates_get(query, from, to)

    if (!is.null(gates)) {
      gates <- with(gates, data.frame(institution=grantee,
                                      pi=NA,
                                      start=NA,
                                      end=NA,
                                      program=NA,
                                      source="Gates",
                                      amount=amount,
                                      id=id,
                                      keyword=query,
                                      title=description,
                                      stringsAsFactors = FALSE))
    } else {
      warning(paste0("Gates query \"", query, "\" returned empty response"))
      gates <- NULL
    }
  } else {
    gates <- NULL
  }

  # Start Open Society block
  if ("osociety" %in% sources) {
    osociety <- osociety_get(query, from_yr, to_yr)

    if (!is.null(osociety)) {
      osociety <- with(osociety, data.frame(institution=institution,
                                            pi=NA,
                                            start=NA,
                                            end=NA,
                                            program=program,
                                            source="Open Society",
                                            amount=amount,
                                            id=id,
                                            keyword=query,
                                            title=description,
                                            stringsAsFactors = F))
    } else {
      warning(paste0("Open Society query \"", query, "\" returned empty response"))
      osociety <- NULL
    }
  } else {
    osociety <- NULL
  }

  full <- rbind.data.frame(nsf, nih, fedreport, ssrc, ophil, osociety, gates, mellon)
  if (nrow(full)==0) {
    return(NULL)
  }

  return(full)
}
