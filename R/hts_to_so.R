#' Transform travel survey data.table to a survey object
#'
#' @param prepped_dt Dataframe in data.table format to transform to survey object.
#' @param weighted If true creates a weighted survey object. Default is TRUE.
#' @param wtname Name of the weight column in the dataframe. Defaults to NULL,
#'  but must exist if weighted is true.
#' @param psu_var Name of the PSU variable to use in the survey design. Defaults
#'  to NULL and will be resolved from available ID columns, preferring `hh_id`
#'  and then `person_id`.
#' @param strataname Name of strata name to bring in. Default is NULL.
#' @param use_strata Whether to include strata in the survey design. Defaults to
#'  `TRUE` when `strataname` is provided and `FALSE` otherwise.
#'
#' @return Inputted dataframe transformed into a survey object.
#' @export
#'
#' @examples
#'
#' require(data.table)
#' hts_to_so(prepped_dt = trip, wtname = "trip_weight")
#'
hts_resolve_psu_var <- function(prepped_dt, psu_var = NULL) {
  if (!is.null(psu_var)) {
    if (!psu_var %in% names(prepped_dt)) {
      stop(paste0(psu_var, " PSU column not found."))
    }

    return(psu_var)
  }

  preferred_psu <- c("hh_id", "person_id", "day_id", "trip_id", "vehicle_id")
  available_psu <- preferred_psu[preferred_psu %in% names(prepped_dt)]

  if (length(available_psu) == 0L) {
    stop("Could not resolve a PSU variable from the prepared data.")
  }

  available_psu[[1]]
}

hts_to_so = function(prepped_dt,
                      weighted = TRUE,
                      wtname = NULL,
                      psu_var = NULL,
                      strataname = NULL,
                      use_strata = !is.null(strataname)) {
  if (weighted & is.null(wtname)) {
    stop("Must provide wtname if weighted = TRUE.")
  }
  # FIXME: Do I really need to copy prepped_dt here?
  wso = data.table::copy(prepped_dt)
  resolved_psu = hts_resolve_psu_var(wso, psu_var)
  data.table::setnames(wso, resolved_psu, "psu")
  if (is.character(wso$psu)) {
    wso[, psu := as.factor(psu)]
  }

  if (!weighted) {
    if (use_strata && !is.null(strataname)) {
      if (!strataname %in% names(wso)) {
        stop(paste0(strataname, " strata column not found."))
      }

      data.table::setnames(wso, strataname, "strata_var")
      if (is.character(wso$strata_var)) {
        wso[, strata_var := as.factor(strata_var)]
      }
      so = srvyr::as_survey_design(wso, ids = psu, strata = strata_var, w = NULL)
    } else {
      so = srvyr::as_survey_design(wso, ids = psu, w = NULL)
    }
  } else if (weighted) {
    if (!wtname %in% names(wso)) {
      stop(paste0(wtname, " weight column not found."))
    }

    if (use_strata && !is.null(strataname)) {
      if (!strataname %in% names(wso)) {
        stop(paste0(strataname, " strata column not found."))
      }
    }

    data.table::setnames(wso, wtname, "weight")

    # filter to where weight > 0 (for appropriate counts):
    wso = wso[weight > 0]

    if (use_strata && !is.null(strataname)) {
      data.table::setnames(wso, strataname, "strata_var")
      if (is.character(wso$strata_var)) {
        wso[, strata_var := as.factor(strata_var)]
      }
      so = srvyr::as_survey_design(wso, ids = psu, w = weight, strata = strata_var)
    } else {
      so = srvyr::as_survey_design(wso, ids = psu, w = weight)
    }
  }

  return(so)
}

## quiets concerns of R CMD check
utils::globalVariables(c("weight", "psu", "strata_var"))
