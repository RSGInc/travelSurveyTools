#' Create summary settings for the settings-driven workflow
#'
#' @param entity_map Named list describing the entities available in the study.
#'  If `NULL`, a default map based on the package's canonical household/person/
#'  day/trip/vehicle tables is used.
#' @param survey Optional list overriding survey defaults.
#' @param missing Optional list overriding missing-data defaults.
#' @param checkbox Optional list overriding checkbox defaults.
#' @param numeric Optional list overriding numeric-summary defaults.
#' @param labels Optional list overriding value-label defaults.
#' @param filters Optional list of named or entity-specific filters.
#' @param derivations Optional list describing derived variables, entities, and
#'  measures.
#'
#' @return A normalized settings list.
#' @export
#'
#' @examples
#' hts_summary_settings()
#'
#' hts_summary_settings(
#'   entity_map = list(
#'     foo = list(
#'       table = "foos",
#'       id = "foo_id",
#'       weight = "foo_weight",
#'       psu = "foo_id",
#'       strata = "sample_segment"
#'     ),
#'     bar = list(
#'       table = "bars",
#'       id = "bar_id",
#'       parent = "foo",
#'       join_keys = c("foo_id"),
#'       weight = "bar_weight"
#'     )
#'   ),
#'   survey = list(
#'     weighted = TRUE,
#'     use_strata = TRUE,
#'     conf_level = 0.90
#'   )
#' )
hts_summary_settings <- function(
    entity_map = NULL,
    survey = NULL,
    missing = NULL,
    checkbox = NULL,
    numeric = NULL,
    labels = NULL,
    filters = NULL,
    derivations = NULL
) {
  if (is.null(entity_map)) {
    entity_map <- hts_default_entity_map()
  }

  defaults <- hts_default_settings_sections()

  list(
    entity_map = entity_map,
    survey = hts_merge_settings_section(defaults$survey, survey),
    missing = hts_merge_settings_section(defaults$missing, missing),
    checkbox = hts_merge_settings_section(defaults$checkbox, checkbox),
    numeric = hts_merge_settings_section(defaults$numeric, numeric),
    labels = hts_merge_settings_section(defaults$labels, labels),
    filters = hts_merge_settings_section(defaults$filters, filters),
    derivations = hts_merge_settings_section(defaults$derivations, derivations)
  )
}

#' Create a reusable study settings template
#'
#' @param entity_map Named list describing the study entities. If `NULL`, the
#'  package default entity map is used.
#'
#' @return A settings list ready to customize for a study.
#' @export
#'
#' @examples
#' hts_study_settings_template()
hts_study_settings_template <- function(entity_map = NULL) {
  hts_summary_settings(entity_map = entity_map)
}

hts_default_settings_sections <- function() {
  list(
    survey = list(
      weighted = TRUE,
      se = FALSE,
      conf_level = 0.95,
      use_strata = FALSE,
      psu_entity = NULL,
      default_weight_entity = NULL
    ),
    missing = list(
      remove_missing = TRUE,
      missing_values = c("Missing Response", "995"),
      not_imputable = -1,
      drop_all_missing_checkbox_rows = TRUE
    ),
    checkbox = list(
      value_col = "value",
      selected_value = 1,
      strict_validity = TRUE
    ),
    numeric = list(
      remove_outliers = TRUE,
      threshold = 0.975
    ),
    labels = list(
      variable_col = "variable",
      value_col = "value",
      label_col = "label",
      order_col = "val_order"
    ),
    filters = list(
      entities = list(),
      named = list()
    ),
    derivations = list(
      variables = list(),
      entities = list(),
      measures = list()
    )
  )
}

hts_merge_settings_section <- function(defaults, overrides = NULL) {
  if (is.null(overrides)) {
    return(defaults)
  }

  if (!is.list(overrides)) {
    stop("Settings section overrides must be lists.")
  }

  utils::modifyList(defaults, overrides)
}

hts_default_entity_map <- function() {
  list(
    household = list(
      table = "hh",
      id = "hh_id",
      weight = "hh_weight",
      psu = "hh_id",
      strata = "sample_segment"
    ),
    person = list(
      table = "person",
      id = "person_id",
      parent = "household",
      join_keys = c("hh_id"),
      weight = "person_weight"
    ),
    day = list(
      table = "day",
      id = "day_id",
      parent = "person",
      join_keys = c("hh_id", "person_id"),
      weight = "day_weight",
      date_var = "travel_date"
    ),
    trip = list(
      table = "trip",
      id = "trip_id",
      parent = "day",
      join_keys = c("hh_id", "person_id", "day_id"),
      weight = "trip_weight",
      date_var = "travel_date"
    ),
    vehicle = list(
      table = "vehicle",
      id = "vehicle_id",
      parent = "household",
      join_keys = c("hh_id"),
      weight = "hh_weight"
    )
  )
}

hts_validate_scalar_setting <- function(section_name, field_name, value, expected_type) {
  if (is.null(value)) {
    return(invisible(NULL))
  }

  valid <- switch(
    expected_type,
    logical = is.logical(value) && length(value) == 1L && !is.na(value),
    character_scalar = is.character(value) && length(value) == 1L && !is.na(value),
    numeric_scalar = is.numeric(value) && length(value) == 1L && !is.na(value),
    list = is.list(value),
    stop("Unknown expected_type in settings validation.")
  )

  if (!valid) {
    stop(
      "settings$", section_name, "$", field_name,
      " must be a valid ", expected_type, "."
    )
  }
}

hts_validate_character_vector_setting <- function(section_name, field_name, value) {
  if (is.null(value)) {
    return(invisible(NULL))
  }

  if (!is.character(value) || any(is.na(value))) {
    stop("settings$", section_name, "$", field_name, " must be a character vector.")
  }
}

hts_validate_entity_spec <- function(entity_name, entity_spec, entity_names, data = NULL) {
  if (!is.list(entity_spec) || is.null(names(entity_spec))) {
    stop("Each entity in settings$entity_map must be a named list.")
  }

  required_fields <- c("table", "id")
  missing_fields <- required_fields[!required_fields %in% names(entity_spec)]
  if (length(missing_fields) > 0L) {
    stop(
      "Entity `", entity_name, "` is missing required field(s): ",
      paste(missing_fields, collapse = ", "), "."
    )
  }

  scalar_fields <- c("table", "id", "parent", "weight", "psu", "strata", "date_var")
  for (field_name in intersect(names(entity_spec), scalar_fields)) {
    value <- entity_spec[[field_name]]
    if (!is.null(value) && (!is.character(value) || length(value) != 1L || is.na(value))) {
      stop("Entity `", entity_name, "` field `", field_name, "` must be a single character value or NULL.")
    }
  }

  parent <- entity_spec[["parent"]]
  join_keys <- entity_spec[["join_keys"]]

  if (!is.null(join_keys) && (!is.character(join_keys) || length(join_keys) == 0L || any(is.na(join_keys)))) {
    stop("Entity `", entity_name, "` field `join_keys` must be a character vector with at least one value.")
  }

  if (!is.null(parent) && !parent %in% entity_names) {
    stop("Entity `", entity_name, "` references unknown parent entity `", parent, "`.") 
  }

  if (!is.null(parent) && is.null(join_keys)) {
    stop("Entity `", entity_name, "` must declare `join_keys` when `parent` is provided.")
  }

  if (is.null(parent) && !is.null(join_keys)) {
    stop("Entity `", entity_name, "` must declare `parent` when `join_keys` is provided.")
  }

  if (!is.null(data)) {
    table_name <- entity_spec$table

    if (!table_name %in% names(data)) {
      stop("Entity `", entity_name, "` references missing table `", table_name, "` in `data`.")
    }

    table_cols <- names(data[[table_name]])
    required_cols <- c(entity_spec$id, join_keys)
    optional_cols <- c(entity_spec[["weight"]], entity_spec[["psu"]], entity_spec[["strata"]], entity_spec[["date_var"]])
    check_cols <- unique(c(required_cols, optional_cols))
    check_cols <- check_cols[!is.na(check_cols)]

    missing_cols <- setdiff(check_cols, table_cols)
    if (length(missing_cols) > 0L) {
      stop(
        "Entity `", entity_name, "` references missing column(s) in table `",
        table_name, "`: ", paste(missing_cols, collapse = ", "), "."
      )
    }
  }

  entity_spec
}

hts_validate_settings_sections <- function(settings, entity_names) {
  if (is.null(settings$survey)) {
    settings$survey <- hts_default_settings_sections()$survey
  }
  if (is.null(settings$missing)) {
    settings$missing <- hts_default_settings_sections()$missing
  }
  if (is.null(settings$checkbox)) {
    settings$checkbox <- hts_default_settings_sections()$checkbox
  }
  if (is.null(settings$numeric)) {
    settings$numeric <- hts_default_settings_sections()$numeric
  }
  if (is.null(settings$labels)) {
    settings$labels <- hts_default_settings_sections()$labels
  }
  if (is.null(settings$filters)) {
    settings$filters <- hts_default_settings_sections()$filters
  }
  if (is.null(settings$derivations)) {
    settings$derivations <- hts_default_settings_sections()$derivations
  }

  hts_validate_scalar_setting("survey", "weighted", settings$survey$weighted, "logical")
  hts_validate_scalar_setting("survey", "se", settings$survey$se, "logical")
  hts_validate_scalar_setting("survey", "conf_level", settings$survey$conf_level, "numeric_scalar")
  if (!settings$survey$conf_level > 0 || !settings$survey$conf_level < 1) {
    stop("settings$survey$conf_level must be between 0 and 1.")
  }
  hts_validate_scalar_setting("survey", "use_strata", settings$survey$use_strata, "logical")
  if (!is.null(settings$survey$psu_entity) && !settings$survey$psu_entity %in% entity_names) {
    stop("settings$survey$psu_entity must name an entity in settings$entity_map.")
  }
  if (!is.null(settings$survey$default_weight_entity) && !settings$survey$default_weight_entity %in% entity_names) {
    stop("settings$survey$default_weight_entity must name an entity in settings$entity_map.")
  }

  hts_validate_scalar_setting("missing", "remove_missing", settings$missing$remove_missing, "logical")
  hts_validate_character_vector_setting("missing", "missing_values", settings$missing$missing_values)
  hts_validate_scalar_setting("missing", "drop_all_missing_checkbox_rows", settings$missing$drop_all_missing_checkbox_rows, "logical")

  hts_validate_scalar_setting("checkbox", "value_col", settings$checkbox$value_col, "character_scalar")
  if (!length(settings$checkbox$selected_value) == 1L || any(is.na(settings$checkbox$selected_value))) {
    stop("settings$checkbox$selected_value must be a single non-missing value.")
  }
  hts_validate_scalar_setting("checkbox", "strict_validity", settings$checkbox$strict_validity, "logical")

  hts_validate_scalar_setting("numeric", "remove_outliers", settings$numeric$remove_outliers, "logical")
  hts_validate_scalar_setting("numeric", "threshold", settings$numeric$threshold, "numeric_scalar")

  label_scalar_fields <- c("variable_col", "value_col", "label_col", "order_col")
  for (field_name in label_scalar_fields) {
    hts_validate_scalar_setting("labels", field_name, settings$labels[[field_name]], "character_scalar")
  }

  if (!is.list(settings$filters$entities)) {
    stop("settings$filters$entities must be a named list.")
  }
  if (!is.list(settings$filters$named)) {
    stop("settings$filters$named must be a named list.")
  }
  if (length(settings$filters$entities) > 0L && is.null(names(settings$filters$entities))) {
    stop("settings$filters$entities must be a named list.")
  }
  if (length(settings$filters$named) > 0L && is.null(names(settings$filters$named))) {
    stop("settings$filters$named must be a named list.")
  }
  unknown_filter_entities <- setdiff(names(settings$filters$entities), entity_names)
  if (length(unknown_filter_entities) > 0L) {
    stop(
      "settings$filters$entities references unknown entity name(s): ",
      paste(unknown_filter_entities, collapse = ", "), "."
    )
  }

  derivation_sections <- c("variables", "entities", "measures")
  for (section_name in derivation_sections) {
    if (!is.list(settings$derivations[[section_name]])) {
      stop("settings$derivations$", section_name, " must be a list.")
    }
  }

  settings
}

#' Validate summary settings and entity map
#'
#' @param settings Settings object created by [hts_summary_settings()] or an
#'  equivalent list containing `entity_map`.
#' @param data Optional named list of input tables. When provided, table and
#'  column references in the entity map are checked against the data.
#'
#' @return A normalized settings list with validated sections.
#' @export
#'
#' @examples
#' settings <- hts_summary_settings()
#' hts_validate_settings(settings)
hts_validate_settings <- function(settings = NULL, data = NULL) {
  if (is.null(settings)) {
    settings <- hts_summary_settings()
  }

  if (!is.list(settings)) {
    stop("`settings` must be a list.")
  }

  if (is.null(settings$entity_map)) {
    stop("`settings` must contain an `entity_map`.")
  }

  entity_map <- settings$entity_map

  if (!is.list(entity_map) || is.null(names(entity_map)) || any(names(entity_map) == "")) {
    stop("`settings$entity_map` must be a named list of entity definitions.")
  }

  entity_names <- names(entity_map)
  validated_entity_map <- lapply(
    entity_names,
    function(entity_name) {
      hts_validate_entity_spec(
        entity_name = entity_name,
        entity_spec = entity_map[[entity_name]],
        entity_names = entity_names,
        data = data
      )
    }
  )
  names(validated_entity_map) <- entity_names

  settings$entity_map <- validated_entity_map
  settings <- hts_validate_settings_sections(settings, entity_names)
  settings
}
