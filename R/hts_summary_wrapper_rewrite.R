hts_wrapper_use_rewrite_path <- function(variables_dt, settings) {
  !is.null(settings) || "entity" %in% names(variables_dt)
}

hts_wrapper_pick_entity_from_legacy <- function(row_dt, table_entity_map, data) {
  candidate_tables <- names(table_entity_map)[vapply(
    names(table_entity_map),
    function(table_name) isTRUE(as.logical(row_dt[[table_name]][[1]])),
    FUN.VALUE = logical(1)
  )]

  if (length(candidate_tables) == 0L) {
    return(NA_character_)
  }

  chosen_table <- candidate_tables[[which.max(vapply(
    candidate_tables,
    function(table_name) nrow(data[[table_name]]),
    FUN.VALUE = integer(1)
  ))]]

  table_entity_map[[chosen_table]]
}

hts_wrapper_as_entity_variables <- function(variables_dt, settings, data) {
  if ("entity" %in% names(variables_dt)) {
    return(data.table::as.data.table(variables_dt))
  }

  legacy_dt <- data.table::as.data.table(data.table::copy(variables_dt))
  table_entity_map <- stats::setNames(
    names(settings$entity_map),
    vapply(settings$entity_map, `[[`, FUN.VALUE = character(1), "table")
  )
  table_cols <- names(table_entity_map)
  missing_table_cols <- setdiff(table_cols, names(legacy_dt))

  if (length(missing_table_cols) > 0L) {
    stop(
      "Legacy `variables_dt` is missing entity table column(s): ",
      paste(missing_table_cols, collapse = ", "),
      "."
    )
  }

  legacy_dt[, entity := vapply(
    seq_len(.N),
    function(i) {
      hts_wrapper_pick_entity_from_legacy(
        row_dt = legacy_dt[i],
        table_entity_map = table_entity_map,
        data = data
      )
    },
    FUN.VALUE = character(1)
  )]

  if (legacy_dt[is.na(entity) | entity == "", .N] > 0L) {
    stop(
      "Could not map the following variable(s) to an entity: ",
      paste(legacy_dt[is.na(entity) | entity == "", variable], collapse = ", "),
      "."
    )
  }

  if (!"label" %in% names(legacy_dt)) {
    legacy_dt[, label := NA_character_]
  }
  if (!"question_text" %in% names(legacy_dt)) {
    legacy_dt[, question_text := NA_character_]
  }
  if (!"logic" %in% names(legacy_dt)) {
    legacy_dt[, logic := NA_character_]
  }
  if (!"universe" %in% names(legacy_dt)) {
    legacy_dt[, universe := NA_character_]
  }
  if (!"topic" %in% names(legacy_dt)) {
    legacy_dt[, topic := NA_character_]
  }
  if (!"notes" %in% names(legacy_dt)) {
    legacy_dt[, notes := NA_character_]
  }

  legacy_dt[
    ,
    .(
      variable = variable,
      entity = entity,
      data_type = data_type,
      variable_label = label,
      question_text = question_text,
      variable_description = description,
      variable_logic = logic,
      is_checkbox = as.logical(is_checkbox),
      shared_name = shared_name,
      universe = universe,
      topic = topic,
      notes = notes
    )
  ]
}

hts_wrapper_filter_data <- function(dt, expr, label) {
  filter_vars <- all.vars(expr)
  missing_vars <- setdiff(filter_vars, names(dt))

  if (length(missing_vars) > 0L) {
    stop(
      label, " references missing column(s): ",
      paste(missing_vars, collapse = ", "),
      "."
    )
  }

  dt[eval(expr)]
}

hts_wrapper_filter_vars <- function(settings, apply_filters = NULL) {
  entity_filter_vars <- unique(unlist(lapply(
    settings$filters$entities,
    all.vars
  )))

  named_filter_vars <- unique(unlist(lapply(
    settings$filters$named[apply_filters %||% character()],
    all.vars
  )))

  unique(c(entity_filter_vars, named_filter_vars))
}

hts_wrapper_checkbox_denominator <- function(dt, analysis_bundle) {
  target_entity <- analysis_bundle$meta$target$entity
  target_id <- analysis_bundle$meta$target$id
  group_vars <- analysis_bundle$meta$group_by$variables %||% character()
  wtname <- analysis_bundle$meta$design$weight_var %||% NULL

  id_cols <- unique(c(target_id, group_vars, wtname))
  unique_dt <- unique(dt[, ..id_cols])

  if (length(group_vars) == 0L) {
    data.table::data.table(
      unwtd_denom = unique_dt[, data.table::uniqueN(get(target_id))],
      wtd_denom = if (!is.null(wtname) && wtname %in% names(unique_dt)) {
        unique_dt[, sum(get(wtname))]
      } else {
        NA_real_
      }
    )
  } else {
    unique_dt[
      ,
      .(
        unwtd_denom = data.table::uniqueN(get(target_id)),
        wtd_denom = if (!is.null(wtname) && wtname %in% names(.SD)) {
          sum(get(wtname))
        } else {
          NA_real_
        }
      ),
      by = group_vars
    ]
  }
}

hts_wrapper_apply_settings_filters <- function(
    analysis_bundle,
    settings,
    apply_filters = NULL
) {
  dt <- data.table::copy(analysis_bundle$analysis_table)
  participating_entities <- unique(c(
    analysis_bundle$meta$target$entity,
    analysis_bundle$meta$group_by$entities$entity %||% character()
  ))

  for (entity_name in intersect(names(settings$filters$entities), participating_entities)) {
    dt <- hts_wrapper_filter_data(
      dt,
      expr = settings$filters$entities[[entity_name]],
      label = paste0("Entity filter `", entity_name, "`")
    )
  }

  for (filter_name in apply_filters %||% character()) {
    if (!filter_name %in% names(settings$filters$named)) {
      stop("Unknown named filter `", filter_name, "`.")
    }

    dt <- hts_wrapper_filter_data(
      dt,
      expr = settings$filters$named[[filter_name]],
      label = paste0("Named filter `", filter_name, "`")
    )
  }

  analysis_bundle$analysis_table <- dt

  if (isTRUE(analysis_bundle$meta$target$is_checkbox)) {
    analysis_bundle$meta$checkbox$denominator <- hts_wrapper_checkbox_denominator(
      dt = dt,
      analysis_bundle = analysis_bundle
    )
  }

  analysis_bundle
}

hts_wrapper_apply_missing_rules <- function(analysis_bundle, settings) {
  if (!isTRUE(settings$missing$remove_missing)) {
    return(analysis_bundle)
  }

  dt <- data.table::copy(analysis_bundle$analysis_table)
  vars_to_check <- if (isTRUE(analysis_bundle$meta$target$is_checkbox)) {
    analysis_bundle$meta$group_by$variables %||% character()
  } else {
    unique(c(
      analysis_bundle$meta$target$variable,
      analysis_bundle$meta$group_by$variables %||% character()
    ))
  }

  if (length(vars_to_check) == 0L) {
    return(analysis_bundle)
  }

  keep_mask <- rep(TRUE, nrow(dt))

  for (var_name in intersect(vars_to_check, names(dt))) {
    keep_mask <- keep_mask & !hts_wrapper_is_missing_value(
      dt[[var_name]],
      missing_values = settings$missing$missing_values,
      not_imputable = settings$missing$not_imputable
    )
  }

  dt <- dt[keep_mask]
  analysis_bundle$analysis_table <- dt

  if (isTRUE(analysis_bundle$meta$target$is_checkbox)) {
    analysis_bundle$meta$checkbox$denominator <- hts_wrapper_checkbox_denominator(
      dt = dt,
      analysis_bundle = analysis_bundle
    )
  }

  analysis_bundle
}

hts_wrapper_apply_numeric_rules <- function(analysis_bundle, settings, data_type) {
  if (!data_type %in% c("numeric", "integer") || !isTRUE(settings$numeric$remove_outliers)) {
    return(analysis_bundle)
  }

  dt <- data.table::copy(analysis_bundle$analysis_table)
  target_var <- analysis_bundle$meta$target$variable

  if (!target_var %in% names(dt) || nrow(dt) == 0L) {
    return(analysis_bundle)
  }

  outlier_results <- hts_remove_outliers(
    var_dt = dt,
    numvar = target_var,
    threshold = settings$numeric$threshold
  )

  analysis_bundle$analysis_table <- outlier_results$dt
  analysis_bundle
}

hts_wrapper_unit_counts <- function(analysis_bundle, weighted) {
  dt <- analysis_bundle$analysis_table
  if (nrow(dt) == 0L) {
    return(list(unwtd = list(), wtd = if (isTRUE(weighted)) 0 else NULL))
  }

  target_entity <- analysis_bundle$meta$target$entity
  entity_ids <- unique(unlist(lapply(
    analysis_bundle$meta$entities,
    function(spec) spec$id %||% NULL
  )))
  entity_ids <- intersect(entity_ids, names(dt))

  ns_unwtd <- lapply(entity_ids, function(id_name) data.table::uniqueN(dt[[id_name]]))
  names(ns_unwtd) <- paste("Count of unique", entity_ids)

  wtname <- analysis_bundle$meta$design$weight_var %||% NULL
  ns_wtd <- if (isTRUE(weighted) && !is.null(wtname) && wtname %in% names(dt)) {
    stats::setNames(list(dt[, sum(get(wtname))]), paste("Sum of", wtname))
  } else {
    NULL
  }

  list(unwtd = ns_unwtd, wtd = ns_wtd)
}

hts_wrapper_target_description <- function(target_rows) {
  descriptions <- target_rows$variable_description[!is.na(target_rows$variable_description)]

  if (length(descriptions) == 0L) {
    return(NA_character_)
  }

  prefixes <- trimws(sub(":.*$", "", descriptions))
  shared_prefixes <- unique(prefixes[nzchar(prefixes)])

  if (length(shared_prefixes) == 1L) {
    return(shared_prefixes[[1]])
  }

  descriptions[[1]]
}

hts_wrapper_meta_from_rewrite <- function(normalized_inputs, analysis_bundle) {
  variables_dt <- normalized_inputs$variables
  settings <- normalized_inputs$settings
  summarize_var <- analysis_bundle$meta$target$variable
  target_rows <- hts_variable_rows(summarize_var, variables_dt)
  target_is_checkbox <- isTRUE(analysis_bundle$meta$target$is_checkbox)
  group_entities <- analysis_bundle$meta$group_by$entities
  source_entities <- unique(c(
    analysis_bundle$meta$target$entity,
    group_entities$entity %||% character()
  ))
  source_tables <- unique(vapply(
    source_entities,
    function(entity_name) settings$entity_map[[entity_name]]$table,
    FUN.VALUE = character(1)
  ))

  list(
    target = list(
      variable = summarize_var,
      variable_label = target_rows$variable_label[[1]] %||% NA_character_,
      question_text = target_rows$question_text[[1]] %||% NA_character_,
      variable_description = if (target_is_checkbox) {
        hts_wrapper_target_description(target_rows)
      } else {
        target_rows$variable_description[[1]] %||% NA_character_
      },
      variable_logic = target_rows$variable_logic[[1]] %||% NA_character_,
      variable_universe = target_rows$universe[[1]] %||% NA_character_,
      variable_topic = target_rows$topic[[1]] %||% NA_character_,
      variable_notes = target_rows$notes[[1]] %||% NA_character_,
      data_type = target_rows$data_type[[1]],
      shared_name = target_rows$shared_name[[1]] %||% summarize_var,
      is_checkbox = target_is_checkbox
    ),
    group_by = list(
      variables = analysis_bundle$meta$group_by$variables %||% character(),
      entities = group_entities
    ),
    source_tables = source_tables,
    design = list(
      weight_var = analysis_bundle$meta$design$weight_var %||% NULL,
      psu_var = analysis_bundle$meta$design$psu_var %||% NULL,
      strata_var = if (isTRUE(analysis_bundle$meta$design$use_strata)) {
        analysis_bundle$meta$design$strata_var %||% NULL
      } else {
        NULL
      },
      use_strata = isTRUE(analysis_bundle$meta$design$use_strata)
    )
  )
}

hts_wrapper_apply_value_labels <- function(summary_payload, vals_df) {
  if (is.null(summary_payload) || is.null(vals_df) || nrow(vals_df) == 0L) {
    return(summary_payload)
  }

  for (summary_type in c("unwtd", "wtd")) {
    summary_dt <- summary_payload$summary_data[[summary_type]]

    if (is.null(summary_dt)) {
      next
    }

    summary_payload$summary_data[[summary_type]] <- factorize_df(
      summary_dt,
      vals_df = vals_df,
      value_label_colname = "label",
      verbose = FALSE
    )
  }

  summary_payload
}

hts_wrapper_diagnostics_from_rewrite <- function(
    normalized_inputs,
    analysis_bundle,
    unit_counts,
    settings
) {
  target_entity <- analysis_bundle$meta$target$entity
  target_table <- normalized_inputs$settings$entity_map[[target_entity]]$table
  target_id <- analysis_bundle$meta$target$id
  target_var <- analysis_bundle$meta$target$variable
  source_dt <- normalized_inputs$data[[target_table]]
  analysis_dt <- analysis_bundle$analysis_table

  n_total <- nrow(source_dt)
  n_valid <- if (nrow(analysis_dt) == 0L) {
    0L
  } else {
    data.table::uniqueN(analysis_dt[[target_id]])
  }
  n_missing <- n_total - n_valid
  pct_missing <- if (n_total == 0L) 0 else n_missing / n_total
  n_distinct <- if (!target_var %in% names(analysis_dt) || nrow(analysis_dt) == 0L) {
    0L
  } else {
    data.table::uniqueN(analysis_dt[[target_var]])
  }

  checkbox_diagnostics <- if (isTRUE(analysis_bundle$meta$target$is_checkbox) && nrow(analysis_dt) > 0L) {
    list(
      n_selected = data.table::uniqueN(
        analysis_dt[checkbox_value == settings$checkbox$selected_value][[target_id]]
      ),
      n_responses = as.integer(sum(
        analysis_dt$checkbox_value == settings$checkbox$selected_value,
        na.rm = TRUE
      ))
    )
  } else {
    list(n_selected = NULL, n_responses = NULL)
  }

  list(
    unit_counts = unit_counts,
    n_total = n_total,
    n_valid = as.integer(n_valid),
    n_missing = as.integer(n_missing),
    pct_missing = pct_missing,
    n_distinct = as.integer(n_distinct),
    all_missing = n_valid == 0L,
    n_selected = checkbox_diagnostics$n_selected,
    n_responses = checkbox_diagnostics$n_responses,
    notes = character(),
    warnings = character()
  )
}

hts_summary_wrapper_rewrite <- function(
    summarize_var,
    summarize_by = NULL,
    variables_dt,
    vals_df,
    data,
    settings = NULL,
    weighted = TRUE,
    se = FALSE,
    conf_level = 0.95,
    apply_filters = NULL
) {
  settings <- hts_validate_settings(settings, data = data)
  entity_variables <- hts_wrapper_as_entity_variables(
    variables_dt = variables_dt,
    settings = settings,
    data = data
  )

  normalized_inputs <- hts_normalize_inputs(
    data = data,
    variables_dt = entity_variables,
    value_labels = vals_df,
    settings = settings
  )

  analysis_bundle <- hts_build_analysis_table(
    summarize_var = summarize_var,
    summarize_by = summarize_by,
    normalized_inputs = normalized_inputs,
    include_vars = hts_wrapper_filter_vars(
      settings = normalized_inputs$settings,
      apply_filters = apply_filters
    )
  )

  analysis_bundle <- hts_wrapper_apply_settings_filters(
    analysis_bundle = analysis_bundle,
    settings = normalized_inputs$settings,
    apply_filters = apply_filters
  )
  analysis_bundle <- hts_wrapper_apply_missing_rules(
    analysis_bundle = analysis_bundle,
    settings = normalized_inputs$settings
  )

  target_rows <- hts_variable_rows(summarize_var, normalized_inputs$variables)
  data_type <- target_rows$data_type[[1]]

  analysis_bundle <- hts_wrapper_apply_numeric_rules(
    analysis_bundle = analysis_bundle,
    settings = normalized_inputs$settings,
    data_type = data_type
  )

  summary_payloads <- list(
    categorical = NULL,
    numeric = NULL,
    datetime = NULL
  )

  if (isTRUE(analysis_bundle$meta$target$is_checkbox)) {
    categorical_payload <- hts_summarize_checkbox(
      analysis_bundle = analysis_bundle,
      weighted = weighted,
      se = se,
      conf_level = conf_level,
      selected_value = normalized_inputs$settings$checkbox$selected_value
    )
    summary_payloads$categorical <- categorical_payload
  } else if (data_type %in% c("categorical", "character", "integer/categorical")) {
    summary_payloads$categorical <- hts_summarize_categorical(
      analysis_bundle = analysis_bundle,
      weighted = weighted,
      se = se,
      conf_level = conf_level
    )
  } else if (data_type %in% c("numeric", "integer")) {
    summary_payloads$numeric <- hts_summarize_numeric(
      analysis_bundle = analysis_bundle,
      weighted = weighted,
      se = se,
      conf_level = conf_level
    )
  } else if (data_type %in% c("date", "datetime")) {
    summary_payloads$datetime <- hts_summarize_datetime(
      analysis_bundle = analysis_bundle,
      weighted = weighted,
      se = se,
      conf_level = conf_level
    )
  } else {
    stop("Unsupported data_type for rewrite summary wrapper: `", data_type, "`.")
  }

  unit_counts <- hts_wrapper_unit_counts(
    analysis_bundle = analysis_bundle,
    weighted = weighted
  )

  for (payload_name in names(summary_payloads)) {
    if (!is.null(summary_payloads[[payload_name]])) {
      summary_payloads[[payload_name]]$unit_counts <- unit_counts
      summary_payloads[[payload_name]] <- hts_wrapper_apply_value_labels(
        summary_payload = summary_payloads[[payload_name]],
        vals_df = normalized_inputs$value_labels
      )
    }
  }

  analysis_bundle$meta$entities <- normalized_inputs$settings$entity_map

  list(
    meta = hts_wrapper_meta_from_rewrite(
      normalized_inputs = normalized_inputs,
      analysis_bundle = analysis_bundle
    ),
    diagnostics = hts_wrapper_diagnostics_from_rewrite(
      normalized_inputs = normalized_inputs,
      analysis_bundle = analysis_bundle,
      unit_counts = unit_counts,
      settings = normalized_inputs$settings
    ),
    summaries = summary_payloads
  )
}
