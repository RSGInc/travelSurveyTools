#' Validate entity-based variable metadata
#'
#' @param variables_dt Data table containing one row per variable with at least
#'  `variable`, `entity`, and `data_type`.
#' @param settings Settings object created by [hts_summary_settings()] or an
#'  equivalent list containing a valid `entity_map`.
#'
#' @return A normalized variable metadata table.
#' @export
#'
#' @examples
#' variables_dt <- data.table::data.table(
#'   variable = c("employment", "travel_mode"),
#'   entity = c("person", "trip"),
#'   data_type = c("categorical", "categorical")
#' )
#' hts_validate_variables(variables_dt, hts_summary_settings())
hts_validate_variables <- function(variables_dt, settings = NULL) {
  settings <- hts_validate_settings(settings)

  if (!data.table::is.data.table(variables_dt)) {
    variables_dt <- data.table::as.data.table(variables_dt)
  } else {
    variables_dt <- data.table::copy(variables_dt)
  }

  required_cols <- c("variable", "entity", "data_type")
  missing_cols <- setdiff(required_cols, names(variables_dt))

  if (length(missing_cols) > 0L) {
    stop(
      "`variables_dt` is missing required column(s): ",
      paste(missing_cols, collapse = ", "), "."
    )
  }

  required_missing <- vapply(
    required_cols,
    function(col_name) {
      sum(is.na(variables_dt[[col_name]]) | variables_dt[[col_name]] == "")
    },
    FUN.VALUE = integer(1)
  )

  if (any(required_missing > 0L)) {
    stop(
      "Missing values in required variable metadata column(s): ",
      paste(names(required_missing)[required_missing > 0L], collapse = ", "), "."
    )
  }

  if (variables_dt[, data.table::uniqueN(variable)] != nrow(variables_dt)) {
    stop("Duplicate variables appear in `variables_dt`.")
  }

  valid_entities <- names(settings$entity_map)
  unknown_entities <- setdiff(unique(variables_dt$entity), valid_entities)

  if (length(unknown_entities) > 0L) {
    stop(
      "Unknown entity name(s) in `variables_dt`: ",
      paste(unknown_entities, collapse = ", "),
      "."
    )
  }

  optional_character_cols <- c(
    "variable_label",
    "question_text",
    "variable_description",
    "variable_logic",
    "universe",
    "topic",
    "notes"
  )

  for (col_name in optional_character_cols) {
    if (!col_name %in% names(variables_dt)) {
      variables_dt[, (col_name) := NA_character_]
    }
  }

  if (!"is_checkbox" %in% names(variables_dt)) {
    variables_dt[, is_checkbox := FALSE]
  }

  if (!is.logical(variables_dt$is_checkbox)) {
    variables_dt[, is_checkbox := as.logical(is_checkbox)]
  }

  if (any(is.na(variables_dt$is_checkbox))) {
    variables_dt[is.na(is_checkbox), is_checkbox := FALSE]
  }

  if (!"shared_name" %in% names(variables_dt)) {
    variables_dt[, shared_name := variable]
  }

  variables_dt[
    is.na(shared_name) | shared_name == "",
    shared_name := variable
  ]

  checkbox_missing_shared <- variables_dt[is_checkbox == TRUE & (is.na(shared_name) | shared_name == ""), variable]
  if (length(checkbox_missing_shared) > 0L) {
    stop(
      "Checkbox variable(s) must declare a `shared_name`: ",
      paste(checkbox_missing_shared, collapse = ", "), "."
    )
  }

  checkbox_groups <- variables_dt[is_checkbox == TRUE, .N, by = .(entity, shared_name)]
  singleton_checkbox_groups <- if (nrow(checkbox_groups) == 0L) {
    character()
  } else {
    checkbox_groups[N < 2L, paste0(entity, "::", shared_name)]
  }

  if (length(singleton_checkbox_groups) > 0L) {
    warning(
      "Checkbox shared_name group(s) contain only one variable: ",
      paste(singleton_checkbox_groups, collapse = ", ")
    )
  }

  variables_dt[]
}
