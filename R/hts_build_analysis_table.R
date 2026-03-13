#' Build an analysis table from normalized entity-based inputs
#'
#' @param summarize_var Variable to summarize.
#' @param summarize_by Optional variable or vector of variables to summarize by.
#' @param normalized_inputs Normalized bundle returned by [hts_normalize_inputs()].
#'
#' @return A list with the prepared `analysis_table` and metadata about the
#'  target entity, grouping entities, joins, and design defaults.
#' @export
#'
#' @examples
#' variables_dt <- data.table::data.table(
#'   variable = c("employment", "age", "mode_type"),
#'   entity = c("person", "person", "trip"),
#'   data_type = c("categorical", "categorical", "categorical")
#' )
#' normalized <- hts_normalize_inputs(
#'   data = list(hh = hh, person = person, day = day, trip = trip, vehicle = vehicle),
#'   variables_dt = variables_dt,
#'   value_labels = value_labels,
#'   settings = hts_summary_settings()
#' )
#' hts_build_analysis_table("mode_type", "employment", normalized)
hts_entity_parent <- function(entity_name, entity_map) {
  entity_map[[entity_name]][["parent"]] %||% NULL
}

hts_entity_ancestors <- function(entity_name, entity_map) {
  ancestors <- character()
  current <- entity_name

  while (!is.null(hts_entity_parent(current, entity_map))) {
    current <- hts_entity_parent(current, entity_map)
    ancestors <- c(ancestors, current)
  }

  ancestors
}

hts_variable_row <- function(variable_name, variables_dt) {
  variables_dt[variable == variable_name][1]
}

hts_variable_rows <- function(variable_name, variables_dt) {
  exact_rows <- variables_dt[variable == variable_name]
  if (nrow(exact_rows) > 0L) {
    return(exact_rows[])
  }

  shared_rows <- variables_dt[shared_name == variable_name]
  shared_rows[]
}

hts_checkbox_label <- function(variable_rows) {
  labels <- variable_rows$variable_label
  descriptions <- variable_rows$variable_description
  label_values <- ifelse(
    !is.na(labels) & nzchar(labels),
    labels,
    ifelse(!is.na(descriptions) & nzchar(descriptions), descriptions, variable_rows$variable)
  )

  sub("^.*?:\\s*", "", label_values)
}

hts_join_entity_table <- function(analysis_dt, entity_name, normalized_inputs) {
  entity_spec <- normalized_inputs$settings$entity_map[[entity_name]]
  entity_table <- data.table::copy(normalized_inputs$data[[entity_spec$table]])

  common_cols <- intersect(names(analysis_dt), names(entity_table))

  if (length(common_cols) == 0L) {
    stop("No join keys found between analysis table and entity `", entity_name, "`.")
  }

  merge(
    analysis_dt,
    entity_table,
    by = common_cols,
    all.x = TRUE,
    all.y = FALSE
  )
}

hts_build_analysis_table <- function(
    summarize_var,
    summarize_by = NULL,
    normalized_inputs
) {
  if (!is.list(normalized_inputs) ||
      !all(c("data", "variables", "value_labels", "settings") %in% names(normalized_inputs))) {
    stop("`normalized_inputs` must be a bundle returned by `hts_normalize_inputs()`.")
  }

  variables_dt <- normalized_inputs$variables
  entity_map <- normalized_inputs$settings$entity_map

  target_rows <- hts_variable_rows(summarize_var, variables_dt)
  if (nrow(target_rows) == 0L) {
    stop("Target variable `", summarize_var, "` not found in normalized variables.")
  }

  if (data.table::uniqueN(target_rows$entity) != 1L) {
    stop("Target variable `", summarize_var, "` maps to multiple entities.")
  }

  target_entity <- target_rows$entity[[1]]
  target_spec <- entity_map[[target_entity]]
  analysis_dt <- data.table::copy(normalized_inputs$data[[target_spec$table]])
  target_is_checkbox <- all(target_rows$is_checkbox)
  target_vars <- if (target_is_checkbox) target_rows$variable else summarize_var

  keep_cols <- unique(c(
    target_spec$id,
    target_spec$join_keys %||% character(),
    target_spec$weight %||% character(),
    target_spec$psu %||% character(),
    target_spec$strata %||% character(),
    summarize_by %||% character(),
    target_vars
  ))
  keep_cols <- intersect(keep_cols, names(analysis_dt))
  analysis_dt <- analysis_dt[, ..keep_cols]

  grouping_vars <- summarize_by %||% character()
  joins_performed <- list()

  for (group_var in grouping_vars) {
    group_row <- hts_variable_row(group_var, variables_dt)
    if (nrow(group_row) == 0L) {
      stop("Grouping variable `", group_var, "` not found in normalized variables.")
    }

    group_entity <- group_row$entity[[1]]

    if (group_entity == target_entity) {
      next
    }

    target_ancestors <- hts_entity_ancestors(target_entity, entity_map)
    if (!group_entity %in% target_ancestors) {
      stop(
        "Grouping variable `", group_var, "` belongs to entity `", group_entity,
        "`, which is not the same as or an ancestor of target entity `", target_entity, "`."
      )
    }

    if (!group_var %in% names(analysis_dt)) {
      analysis_dt <- hts_join_entity_table(
        analysis_dt = analysis_dt,
        entity_name = group_entity,
        normalized_inputs = normalized_inputs
      )
      joins_performed[[group_entity]] <- intersect(
        names(normalized_inputs$data[[entity_map[[group_entity]]$table]]),
        names(analysis_dt)
      )
    }
  }

  checkbox_meta <- NULL

  if (target_is_checkbox) {
    valid_mask <- analysis_dt[
      ,
      Reduce(
        `&`,
        lapply(.SD, function(x) !is.na(x))
      ),
      .SDcols = target_vars
    ]

    denominator_dt <- analysis_dt[valid_mask]
    denominator_group_cols <- c(grouping_vars)

    if (length(denominator_group_cols) == 0L) {
      denominator <- data.table::data.table(
        unwtd_denom = denominator_dt[, data.table::uniqueN(get(target_spec$id))],
        wtd_denom = if (!is.null(target_spec$weight) && target_spec$weight %in% names(denominator_dt)) {
          denominator_dt[, sum(get(target_spec$weight))]
        } else {
          NA_real_
        }
      )
    } else {
      denominator <- denominator_dt[
        ,
        .(
          unwtd_denom = data.table::uniqueN(get(target_spec$id)),
          wtd_denom = if (!is.null(target_spec$weight) && target_spec$weight %in% names(.SD)) {
            sum(get(target_spec$weight))
          } else {
            NA_real_
          }
        ),
        by = denominator_group_cols
      ]
    }

    id_cols <- unique(c(
      target_spec$id,
      target_spec$join_keys %||% character(),
      target_spec$weight %||% character(),
      target_spec$psu %||% character(),
      target_spec$strata %||% character(),
      grouping_vars
    ))
    id_cols <- intersect(id_cols, names(analysis_dt))

    checkbox_long <- data.table::melt(
      analysis_dt[valid_mask],
      id.vars = id_cols,
      measure.vars = target_vars,
      variable.name = "checkbox_variable",
      value.name = "checkbox_value"
    )

    checkbox_lookup <- data.table::data.table(
      checkbox_variable = target_rows$variable,
      checkbox_label = hts_checkbox_label(target_rows)
    )

    checkbox_long <- merge(
      checkbox_long,
      checkbox_lookup,
      by = "checkbox_variable",
      all.x = TRUE
    )

    data.table::setnames(checkbox_long, "checkbox_label", summarize_var)
    analysis_dt <- checkbox_long[]

    checkbox_meta <- list(
      checkbox_vars = target_vars,
      selected_value = 1,
      denominator = denominator
    )
  } else {
    selected_output_cols <- unique(c(
      target_spec$id,
      target_spec$join_keys %||% character(),
      target_spec$weight %||% character(),
      target_spec$psu %||% character(),
      target_spec$strata %||% character(),
      grouping_vars,
      summarize_var
    ))
    selected_output_cols <- intersect(selected_output_cols, names(analysis_dt))
    analysis_dt <- analysis_dt[, ..selected_output_cols]
  }

  group_entities <- if (length(grouping_vars) == 0L) {
    data.table::data.table(variable = character(), entity = character())
  } else {
    variables_dt[variable %in% grouping_vars, .(variable, entity)]
  }

  list(
    analysis_table = analysis_dt,
    meta = list(
      target = list(
        variable = summarize_var,
        entity = target_entity,
        table = target_spec$table,
        is_checkbox = target_is_checkbox,
        shared_name = if (target_is_checkbox) summarize_var else target_rows$shared_name[[1]]
      ),
      group_by = list(
        variables = grouping_vars,
        entities = group_entities
      ),
      design = list(
        weight_var = target_spec$weight %||% NULL,
        psu_var = target_spec$psu %||% NULL,
        strata_var = target_spec$strata %||% NULL
      ),
      joins = joins_performed,
      checkbox = checkbox_meta
    )
  )
}
