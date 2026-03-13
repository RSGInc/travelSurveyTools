#' Summarize numeric data from an analysis table bundle
#'
#' @param analysis_bundle Bundle returned by [hts_build_analysis_table()].
#' @param weighted Whether to calculate weighted summaries. Default is `TRUE`.
#' @param se Whether to calculate standard errors for the weighted mean.
#'  Default is `FALSE`.
#' @param conf_level Confidence level for confidence intervals when available.
#'  Default is `0.95`.
#'
#' @return A list containing `summary_data` with unweighted and weighted
#'  numeric summaries, plus survey design field names used.
#' @export
#'
#' @examples
#' variables_dt <- data.table::data.table(
#'   variable = c("speed_mph", "employment"),
#'   entity = c("trip", "person"),
#'   data_type = c("numeric", "categorical")
#' )
#' normalized <- hts_normalize_inputs(
#'   data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
#'   variables_dt = variables_dt,
#'   value_labels = value_labels,
#'   settings = hts_summary_settings()
#' )
#' analysis_bundle <- hts_build_analysis_table("speed_mph", "employment", normalized)
#' hts_summarize_numeric(analysis_bundle)
hts_summarize_numeric <- function(
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

  if (length(group_vars) == 0L) {
    unwtd_summary <- analysis_dt[, .(
      count = sum(!is.na(get(target_var))),
      min = min(get(target_var), na.rm = TRUE),
      max = max(get(target_var), na.rm = TRUE),
      mean = mean(get(target_var), na.rm = TRUE),
      median = stats::median(get(target_var), na.rm = TRUE)
    )]
  } else {
    unwtd_summary <- analysis_dt[, .(
      count = sum(!is.na(get(target_var))),
      min = min(get(target_var), na.rm = TRUE),
      max = max(get(target_var), na.rm = TRUE),
      mean = mean(get(target_var), na.rm = TRUE),
      median = stats::median(get(target_var), na.rm = TRUE)
    ), by = group_vars]
  }

  summary_data <- list(unwtd = unwtd_summary[])

  if (isTRUE(weighted)) {
    if (is.null(wtname) || !wtname %in% names(analysis_dt)) {
      stop("Weighted numeric summaries require a valid weight variable in the analysis table.")
    }

    so <- hts_to_so(
      prepped_dt = analysis_dt,
      weighted = TRUE,
      wtname = wtname,
      psu_var = psu_var,
      strataname = strataname,
      use_strata = use_strata
    )

    mean_vartype <- if (isTRUE(se)) c("se", "ci") else NULL

    wtd_summary <- so |>
      dplyr::group_by_at(unlist(group_vars)) |>
      dplyr::summarize(
        count = sum(!is.na(get(target_var))),
        min = min(get(target_var), na.rm = TRUE),
        max = max(get(target_var), na.rm = TRUE),
        mean = srvyr::survey_mean(
          get(target_var),
          vartype = mean_vartype,
          level = conf_level,
          na.rm = TRUE
        ),
        median = srvyr::survey_median(get(target_var), vartype = NULL, na.rm = TRUE)
      ) |>
      data.table::setDT()

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

hts_datetime_from_numeric_summary <- function(numeric_summary, prototype) {
  datetime_cols <- c("min", "max", "mean", "median")

  for (summary_type in c("unwtd", "wtd")) {
    summary_dt <- numeric_summary$summary_data[[summary_type]]

    if (is.null(summary_dt)) {
      next
    }

    for (col_name in intersect(datetime_cols, names(summary_dt))) {
      data.table::set(
        summary_dt,
        j = col_name,
        value = hts_coerce_datetime_vector(summary_dt[[col_name]], prototype)
      )
    }

    numeric_summary$summary_data[[summary_type]] <- summary_dt
  }

  numeric_summary
}

#' Summarize date/time data from an analysis table bundle
#'
#' @param analysis_bundle Bundle returned by [hts_build_analysis_table()].
#' @param weighted Whether to calculate weighted summaries. Default is `TRUE`.
#' @param se Whether to calculate standard errors for the weighted mean.
#'  Default is `FALSE`.
#' @param conf_level Confidence level for confidence intervals when available.
#'  Default is `0.95`.
#'
#' @return A list containing `summary_data` with unweighted and weighted
#'  datetime summaries, plus survey design field names used.
#' @export
hts_summarize_datetime <- function(
    analysis_bundle,
    weighted = TRUE,
    se = FALSE,
    conf_level = 0.95
) {
  target_var <- analysis_bundle$meta$target$variable
  prototype <- analysis_bundle$analysis_table[[target_var]]

  numeric_summary <- hts_summarize_numeric(
    analysis_bundle = analysis_bundle,
    weighted = weighted,
    se = se,
    conf_level = conf_level
  )

  hts_datetime_from_numeric_summary(
    numeric_summary = numeric_summary,
    prototype = prototype
  )
}
