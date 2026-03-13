#' Summarize categorical data from an analysis table bundle
#'
#' @param analysis_bundle Bundle returned by [hts_build_analysis_table()].
#' @param weighted Whether to calculate weighted summaries. Default is `TRUE`.
#' @param se Whether to calculate standard errors and confidence intervals for
#'  weighted summaries. Default is `FALSE`.
#' @param conf_level Confidence level for confidence intervals when available.
#'  Default is `0.95`.
#'
#' @return A list containing `summary_data` with unweighted and weighted
#'  categorical summaries, plus survey design field names used.
#' @export
#'
#' @examples
#' variables_dt <- data.table::data.table(
#'   variable = c("employment", "age"),
#'   entity = c("person", "person"),
#'   data_type = c("categorical", "categorical")
#' )
#' normalized <- hts_normalize_inputs(
#'   data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
#'   variables_dt = variables_dt,
#'   value_labels = value_labels,
#'   settings = hts_summary_settings()
#' )
#' analysis_bundle <- hts_build_analysis_table("employment", "age", normalized)
#' hts_summarize_categorical(analysis_bundle)
hts_summarize_categorical <- function(
    analysis_bundle,
    weighted = TRUE,
    se = FALSE,
    conf_level = 0.95
) {
  if (!is.list(analysis_bundle) ||
      !all(c("analysis_table", "meta") %in% names(analysis_bundle))) {
    stop("`analysis_bundle` must be a bundle returned by `hts_build_analysis_table()`.")
  }

  analysis_dt <- data.table::copy(analysis_bundle$analysis_table)
  target_var <- analysis_bundle$meta$target$variable
  group_vars <- analysis_bundle$meta$group_by$variables %||% character()
  wtname <- analysis_bundle$meta$design$weight_var %||% NULL
  psu_var <- analysis_bundle$meta$design$psu_var %||% NULL
  strataname <- analysis_bundle$meta$design$strata_var %||% NULL
  use_strata <- !is.null(strataname)

  groupbyvars <- c(group_vars, target_var)

  unwtd_summary <- analysis_dt[, .(count = .N), keyby = groupbyvars]

  if (length(group_vars) == 0L) {
    unwtd_summary[, prop := count / sum(count)]
  } else {
    unwtd_summary[, prop := count / sum(count), by = group_vars]
  }

  setcolorder(unwtd_summary, c(groupbyvars, "count", "prop"))

  summary_data <- list(unwtd = unwtd_summary[])

  if (isTRUE(weighted)) {
    if (is.null(wtname) || !wtname %in% names(analysis_dt)) {
      stop("Weighted categorical summaries require a valid weight variable in the analysis table.")
    }

    if (isTRUE(se)) {
      so <- hts_to_so(
        prepped_dt = analysis_dt,
        weighted = TRUE,
        wtname = wtname,
        psu_var = psu_var,
        strataname = strataname,
        use_strata = use_strata
      )

      wtd_summary <- so |>
        dplyr::group_by_at(unlist(groupbyvars)) |>
        dplyr::summarize(
          count = length(get(target_var)),
          prop = srvyr::survey_prop(
            proportion = FALSE,
            vartype = c("se", "cv", "ci"),
            level = conf_level,
            deff = TRUE
          ),
          est = srvyr::survey_total(
            vartype = c("se", "cv", "ci"),
            level = conf_level,
            deff = TRUE
          )
        ) |>
        data.table::setDT()

      if ("prop_cv" %in% names(wtd_summary)) {
        wtd_summary[, rse := prop_cv]
      }

      if ("prop_deff" %in% names(wtd_summary)) {
        wtd_summary[, deff := prop_deff]
        wtd_summary[, ess := data.table::fifelse(
          is.na(deff) | !is.finite(deff) | deff <= 0,
          NA_real_,
          as.numeric(count) / deff
        )]
      }
    } else {
      wtd_summary <- analysis_dt[
        ,
        .(
          count = .N,
          est = sum(get(wtname))
        ),
        by = groupbyvars
      ]

      if (length(group_vars) == 0L) {
        wtd_summary[, prop := est / sum(est)]
      } else {
        wtd_summary[, prop := est / sum(est), by = group_vars]
      }

      setcolorder(wtd_summary, c(groupbyvars, "count", "prop", "est"))
    }

    summary_data$wtd <- wtd_summary[]
  } else {
    summary_data$wtd <- NULL
  }

  list(
    summary_data = summary_data,
    weight_var = wtname,
    psu_var = psu_var,
    strata_var = strataname
  )
}
