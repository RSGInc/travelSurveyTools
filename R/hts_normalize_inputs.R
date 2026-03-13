#' Normalize settings, metadata, labels, and data for the rewrite path
#'
#' @param data Named list of input tables.
#' @param variables_dt Entity-based variable metadata.
#' @param value_labels Optional value-label metadata.
#' @param settings Settings object created by [hts_summary_settings()] or an
#'  equivalent list containing a valid `entity_map`.
#'
#' @return A normalized bundle with `data`, `variables`, `value_labels`, and
#'  `settings`.
#' @export
#'
#' @examples
#' variables_dt <- data.table::data.table(
#'   variable = c("employment", "travel_date"),
#'   entity = c("person", "trip"),
#'   data_type = c("categorical", "date")
#' )
#' hts_normalize_inputs(
#'   data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
#'   variables_dt = variables_dt,
#'   value_labels = value_labels,
#'   settings = hts_summary_settings()
#' )
hts_normalize_inputs <- function(
    data,
    variables_dt,
    value_labels = NULL,
    settings = NULL
) {
  if (!is.list(data) || is.null(names(data)) || any(names(data) == "")) {
    stop("`data` must be a named list of tables.")
  }

  normalized_data <- lapply(data, data.table::as.data.table)
  names(normalized_data) <- names(data)

  normalized_settings <- hts_validate_settings(
    settings = settings,
    data = normalized_data
  )

  normalized_variables <- hts_validate_variables(
    variables_dt = variables_dt,
    settings = normalized_settings,
    data = normalized_data
  )

  normalized_value_labels <- if (is.null(value_labels)) {
    data.table::data.table(
      variable = character(),
      value = character(),
      label = character(),
      val_order = integer()
    )
  } else {
    value_labels <- data.table::as.data.table(value_labels)

    required_label_cols <- c("variable", "value", "label")
    missing_label_cols <- setdiff(required_label_cols, names(value_labels))

    if (length(missing_label_cols) > 0L) {
      stop(
        "`value_labels` is missing required column(s): ",
        paste(missing_label_cols, collapse = ", "), "."
      )
    }

    if (!"val_order" %in% names(value_labels)) {
      value_labels[, val_order := seq_len(.N)]
    }

    value_labels[]
  }

  list(
    data = normalized_data,
    variables = normalized_variables,
    value_labels = normalized_value_labels,
    settings = normalized_settings
  )
}
