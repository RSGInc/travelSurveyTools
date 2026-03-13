#' Create summary settings for travel survey summaries
#'
#' @param entity_map Named list describing the entities available in the study.
#'  If `NULL`, a default map based on the package's canonical household/person/
#'  day/trip/vehicle tables is used.
#'
#' @return A normalized settings list containing an `entity_map`.
#' @export
#'
#' @examples
#' hts_summary_settings()
#'
#' hts_summary_settings(
#'   entity_map = list(
#'     household = list(
#'       table = "households",
#'       id = "hhid",
#'       weight = "hh_weight",
#'       psu = "hhid",
#'       strata = "sample_segment"
#'     ),
#'     person = list(
#'       table = "persons",
#'       id = "pid",
#'       parent = "household",
#'       join_keys = c("hhid"),
#'       weight = "person_weight"
#'     )
#'   )
#' )
hts_summary_settings <- function(entity_map = NULL) {
  if (is.null(entity_map)) {
    entity_map <- hts_default_entity_map()
  }

  list(entity_map = entity_map)
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

#' Validate summary settings and entity map
#'
#' @param settings Settings object created by [hts_summary_settings()] or an
#'  equivalent list containing `entity_map`.
#' @param data Optional named list of input tables. When provided, table and
#'  column references in the entity map are checked against the data.
#'
#' @return A normalized settings list with a validated `entity_map`.
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
  settings
}
