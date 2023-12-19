hts_remove_missing_data = function(hts_data,
                                   variables_dt,
                                   summarize_var,
                                   summarize_by,
                                   missing_value,
                                   not_imputable){

  summarize_var_loc = hts_find_var(summarize_var)

  #get variable or first occurrence for checkbox
  summarize_var_name = variables_dt[shared_name == summarize_var, variable][1]

  summarize_var_tbl = hts_data[[summarize_var_loc]][
    !get(summarize_var_name) %in% c(missing_value, not_imputable) |
      is.na(get(summarize_var_name))]

  summarize_var_id = hts_get_keycols(summarize_var_tbl,
                                     ids = TRUE,
                                     weights = FALSE,
                                     priority = TRUE)

  hts_data = hts_filter_data(
    data = hts_data,
    ids = summarize_var_tbl[, get(summarize_var_id)],
    id_type = stringr::str_remove(summarize_var_id, '_id')
  )

  if (!is.null(summarize_by)){

    summarize_by_loc = hts_find_var(summarize_by)

    #get variable or first occurrence for checkbox
    summarize_by_name = variables_dt[shared_name == summarize_by, variable][1]

    summarize_by_tbl = hts_data[[summarize_by_loc]][
      !get(summarize_by_name) %in% c(missing_value, not_imputable) |
        is.na(get(summarize_by_name))]

    summarize_by_id = hts_get_keycols(summarize_by_tbl,
                                      ids = TRUE,
                                      weights = FALSE,
                                      priority = TRUE)

    hts_data = hts_filter_data(
      data = hts_data,
      ids = summarize_by_tbl[, get(summarize_by_id)],
      id_type = stringr::str_remove(summarize_by_id, '_id')
    )

  }

  return(hts_data)

}
