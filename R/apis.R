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
  # Globals
  from_yr <- as.integer(format.Date(from, "%Y"))
  to_yr <- as.integer(format.Date(to, "%Y"))

  columns <- c("institution", "pi", "year",
               "start", "end",
               "program", "source",
               "amount", "id",
               "title", "keyword")

  if ("neh" %in% sources) {
    neh <- neh_get(queries, from_yr, to_yr)
    if (!is.null(neh)) {
      neh$source <- "NEH"
      neh <- neh[, c("Institution", "pi", "YearAwarded",
                     "BeginGrant", "EndGrant",
                     "Program", "source",
                     "AwardOutright", "AppNumber",
                     "ProjectTitle", "query")]
      names(neh) <- columns
      neh$start <- as.Date(neh$start)
      neh$end <- as.Date(neh$end)
    }
  } else neh <- NULL

  if ("sloan" %in% sources) {
    sloan <- sloan_get(queries, from_yr, to_yr)

    if (!is.null(sloan)) {
      sloan$source <- "Sloan"
      sloan$start <- as.Date(NA)
      sloan$end <- as.Date(NA)
      sloan <- sloan[, c("grantee", "pi", "year",
                         "start", "end",
                         "program", "source",
                         "amount", "id",
                         "description", "query")]
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
      usa$year <- format.Date(usa$Start.Date, "%Y")
      usa <- usa[, c("Recipient.Name", "pi", "year",
                     "Start.Date", "End.Date",
                     "Awarding.Sub.Agency", "source",
                     "Award.Amount", "Award.ID",
                     "Description", "keyword")]
      names(usa) <- columns
      usa[] <- lapply(usa, function(x) if (is.factor(x)) as.character(x) else {x})
    }
  } else usa <- NULL

  # Run the API queries which require one term at a time
  apis <- lapply(queries, function(keyword, sources, from, to, columns) {
    # Globals
    from_yr <- as.integer(format.Date(from, "%Y"))
    to_yr <- as.integer(format.Date(to, "%Y"))
    columns <- columns[-length(columns)]

    # NSF block
    if("nsf" %in% sources) {
      nsf <- nsf_get(keyword, from, to)
      if(is.null(nsf)) warning(paste0("NSF query \"", keyword, "\" returned empty response"))
      else {
        nsf$source <- "NSF"
        nsf$pi <- with(nsf, paste0(piLastName, ", ", piFirstName))
        nsf$date <- as.Date(nsf$date, format="%m/%d/%Y")
        nsf$year <- format.Date(nsf$date, format="%Y")

        nsf$startDate <- as.Date(nsf$startDate, format="%m/%d/%Y")
        nsf$expDate <- as.Date(nsf$expDate, format="%m/%d/%Y")

        nsf$directorate <- NA
        nsf$directorate[nsf$cfdaNumber=="47.075"] <- "SBE"
        nsf$directorate[nsf$cfdaNumber=="47.076"] <- "EHR"

        nsf <- nsf[, c("awardeeName", "pi", "year",
                       "startDate", "expDate",
                       "directorate", "source",
                       "fundsObligatedAmt", "id",
                       "title")]
        names(nsf) <- columns
        #neh$amount <- as.integer(neh$amount)
      }
    } else nsf <- NULL

    # Start nih block
    if ("nih" %in% sources) {
      nih <- nih_get(keyword, from, to)
      if (is.null(nih)) warning(paste0("NIH RePORTER query \"",
                                       keyword, "\" returned empty response"))
      else {
        nih$source <- "NIH"
        nih <- nih[, c("org_name", "contact_pi_name", "fiscal_year",
                       "project_start_date", "project_end_date",
                       "agency_code", "source",
                       "award_amount", "project_num", "project_title")]
        names(nih) <- columns
      }
    } else nih <- NULL

    # Start fedreporter block
    if ("fedreporter" %in% sources) {
      fedreport <- fedreporter_get(keyword, from_yr, to_yr)
      if (is.null(fedreport)) warning(paste0("Federal Reporter query \"",
                                             keyword, "\" returned empty response"))
      else {
        fedreport$source <- "Federal REPORTER"
        fedreport <- fedreport[, c("orgName", "contactPi", "fy",
                                   "projectStartDate", "projectEndDate",
                                   "agency", "source",
                                   "totalCostAmount", "projectNumber", "title")]
        names(fedreport) <- columns
      }
    } else fedreport <- NULL

    # Begin SSRC block
    if ("ssrc" %in% sources) {
      ssrc <- ssrc_get(keyword, from_yr, to_yr)

      if (is.null(ssrc)) warning(paste0("SSRC query \"", keyword,
                                        "\" returned empty response"))
      else {
        ssrc$source <- "SSRC"
        ssrc$start <- NA
        ssrc$end <- NA
        ssrc$amount <- NA
        ssrc <- ssrc[, c("institution", "pi_name", "year", "start", "end",
                         "program", "source", "amount", "id", "title")]
        names(ssrc) <- columns
      }
    } else ssrc <- NULL

    # Begin open philanthropy block
    if ("ophil" %in% sources) {
      ophil <- ophil_get(keyword, from_yr, to_yr)

      if (is.null(ophil)) warning(paste0("Open Philanthropy query \"",
                                         keyword, "\" returned empty response"))
      else {
        ophil$source <- "Open Philosophy"
        ophil$pi <- NA
        ophil$start <- NA
        ophil$end <- NA
        ophil <- ophil[, c("grantee", "pi", "year", "start", "end",
                           "focus", "source", "amount", "id", "title")]
        names(ophil) <- columns
      }
    } else ophil <- NULL

    # Start Mellon block
    if ("mellon" %in% sources) {
      mellon <- mellon_get(keyword, from_yr, to_yr)

      if (is.null(mellon)) warning(paste0("Mellon query \"", keyword,
                                          "\" returned empty response"))

      else {
        mellon$pi <- NA
        mellon$start <- NA
        mellon$end <- NA
        mellon$source <- "Mellon"
        mellon$year <- format.Date(mellon$date, "%Y")
        mellon <- mellon[, c("institution", "pi", "year", "start", "end",
                             "program", "source", "amount", "id",
                             "description")]
        names(mellon) <- columns
      }
    } else mellon <- NULL

    # Start gates block
    if ("gates" %in% sources) {
      gates <- gates_get(keyword, from, to)

      if (is.null(gates)) warning(paste0("Gates query \"", keyword,
                                         "\" returned empty response"))
      else {
        gates$pi <- NA
        gates$start <- NA
        gates$end <- NA
        gates$program <- NA
        gates$source <- "Gates"
        gates$year <- format.Date(gates$date, "%Y")
        gates <- gates[, c("grantee", "pi", "year", "start", "end", "program",
                           "source", "amount", "id", "description")]
        names(gates) <- columns
      }
    } else gates <- NULL

    # Start Open Society block
    if ("osociety" %in% sources) {
      osociety <- osociety_get(keyword, from_yr, to_yr)

      if (is.null(osociety)) warning(paste0("Open Society query \"", keyword,
                                            "\" returned empty response"))

      else {
        osociety$pi <- NA
        osociety$start <- NA
        osociety$end <- NA
        osociety$source <- "Open Society"
        osociety <- osociety[, c("institution", "pi", "year", "start", "end",
                                 "program", "source", "amount", "id",
                                 "description")]
        names(osociety) <- columns
      }
    } else osociety <- NULL

    if ("carnegie" %in% sources) {
      carnegie <- carnegie_get(keyword, from_yr, to_yr)
      if (is.null(carnegie)) warning(paste0("Carnegie query \"", keyword,
                                            "\" returtned empty response"))
      else {
        carnegie$pi <- NA
        carnegie$start <- NA
        carnegie$end <- NA
        carnegie$source <- "Carnegie"
        carnegie <- carnegie[, c("grantee", "pi", "year", "start", "end",
                                 "program", "source", "amount", "id", "title")]
        names(carnegie) <- columns
      }
    } else carnegie <- NULL

    full <- rbind.data.frame(nsf, nih, fedreport,
                             ssrc, ophil, osociety,
                             gates, mellon, carnegie)
    if (nrow(full)==0) return(NULL)

    full$keyword <- keyword

    return(full)
  }, sources, from, to, columns)
  apis <- do.call(rbind.data.frame, apis)

  full <- rbind.data.frame(neh, sloan, usa, apis)
  if (nrow(full)==0) return(NULL)

  full$institution <- sapply(as.character(full$institution), title_case)
  full$pi <- sapply(full$pi, title_case)
  full$amount <- as.integer(full$amount)

  return(full)
}
