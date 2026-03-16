#' Summarize checkbox data from an analysis table bundle
#'
#' @param analysis_bundle Bundle returned by [hts_build_analysis_table()] for a
#'  checkbox target.
#' @param weighted Whether to calculate weighted summaries. Default is `TRUE`.
#' @param se Whether to calculate standard errors and confidence intervals for
#'  weighted summaries. Default is `FALSE`.
#' @param conf_level Confidence level for confidence intervals when available.
#'  Default is `0.95`.
#' @param selected_value Value indicating a selected checkbox response.
#'  Default is `1`.
#'
#' @return A list containing `summary_data` with unweighted and weighted
#'  checkbox summaries, plus survey design field names used.
#' @export
hts_summarize_checkbox <- function(
    analysis_bundle,
    weighted = TRUE,
    se = FALSE,
    conf_level = 0.95,
    selected_value = 1
) {
  if (!is.list(analysis_bundle) ||
      !isTRUE(analysis_bundle$meta$target$is_checkbox)) {
    stop("`analysis_bundle` must be a checkbox bundle returned by `hts_build_analysis_table()`.")
  }

  analysis_dt <- data.table::copy(analysis_bundle$analysis_table)
  target_var <- analysis_bundle$meta$target$variable
  group_vars <- analysis_bundle$meta$group_by$variables %||% character()
  wtname <- analysis_bundle$meta$design$weight_var %||% NULL
  psu_var <- analysis_bundle$meta$design$psu_var %||% NULL
  strataname <- analysis_bundle$meta$design$strata_var %||% NULL
  use_strata <- isTRUE(analysis_bundle$meta$design$use_strata) && !is.null(strataname)
  denominator <- analysis_bundle$meta$checkbox$denominator
  groupbyvars <- c(group_vars, target_var)

  unwtd_summary <- analysis_dt[
    ,
    .(
      count = sum(checkbox_value == selected_value, na.rm = TRUE)
    ),
    by = groupbyvars
  ]

  if (length(group_vars) == 0L) {
    unwtd_summary[, prop := count / denominator$unwtd_denom[[1]]]
  } else {
    unwtd_summary <- merge(unwtd_summary, denominator, by = group_vars, all.x = TRUE)
    unwtd_summary[, prop := count / unwtd_denom]
    unwtd_summary[, c("unwtd_denom", "wtd_denom") := NULL]
  }

  summary_data <- list(unwtd = unwtd_summary[])

  if (isTRUE(weighted)) {
    if (is.null(wtname) || !wtname %in% names(analysis_dt)) {
      stop("Weighted checkbox summaries require a valid weight variable in the analysis table.")
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
          count = sum(get("checkbox_value") == selected_value, na.rm = TRUE),
          prop = srvyr::survey_mean(
            get("checkbox_value") == selected_value,
            vartype = c("se", "cv", "ci"),
            level = conf_level,
            deff = TRUE,
            na.rm = TRUE
          ),
          est = srvyr::survey_total(
            get("checkbox_value") == selected_value,
            vartype = c("se", "cv", "ci"),
            level = conf_level,
            deff = TRUE,
            na.rm = TRUE
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
          count = sum(checkbox_value == selected_value, na.rm = TRUE),
          est = sum(get(wtname) * (checkbox_value == selected_value), na.rm = TRUE)
        ),
        by = groupbyvars
      ]

      if (length(group_vars) == 0L) {
        wtd_summary[, prop := est / denominator$wtd_denom[[1]]]
      } else {
        wtd_summary <- merge(wtd_summary, denominator, by = group_vars, all.x = TRUE)
        wtd_summary[, prop := est / wtd_denom]
        wtd_summary[, c("unwtd_denom", "wtd_denom") := NULL]
      }
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
