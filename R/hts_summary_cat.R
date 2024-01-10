#' Summarize a categorical variable
#'
#' @param prepped_dt Dataset containing the summary variables and key columns in
#'  data.table format.
#' @param summarize_var Name of the categorical variable to summarize. Default is NULL.
#' @param summarize_by Name of the variable to summarize the summarize_var by.
#'  Default is NULL.
#' @param weighted Whether the data is weighted. Default is TRUE.
#' @param se Whether to calculate standard error. Default is FALSE.
#' @param wtname Name of the weight column to use. Default is NULL.
#' @param strataname  Name of strata name to bring in. Default is NULL.
#' 
#' @import dplyr
#' @import srvyr
#'
#' @return List of unweighted and weighted categorical summaries including counts
#' and proportions.
#' @export
#'
#' @examples
#'
#' require(data.table)
#' require(stringr)
#' require(dplyr)
#' require(srvyr)
#' DT = hts_prep_data(summarize_var = 'age',
#'                    variables_dt = variable_list,
#'                    data = list('hh' = hh,
#'                                'person' = person,
#'                                'day' = day,
#'                                'trip' = trip,
#'                                'vehicle' = vehicle))$cat
#' hts_summary_cat(prepped_dt = DT,
#'                 summarize_var = 'age',
#'                 values_dt = value_labels)
#' DT = hts_prep_data(summarize_var = 'age',
#'                    summarize_by = 'employment',
#'                    variables_dt = variable_list,
#'                    data = list('hh' = hh,
#'                                'person' = person,
#'                                'day' = day,
#'                                'trip' = trip,
#'                                'vehicle' = vehicle))$cat
#' hts_summary_cat(prepped_dt = DT,
#'                 summarize_var = 'age',
#'                 summarize_by = 'employment',
#'                 values_dt = value_labels,
#'                 extra_labels = 'Missing')
#'                 
hts_summary_cat = function(prepped_dt,
                           summarize_var = NULL,
                           summarize_by = NULL,
                           weighted = TRUE,
                           se = FALSE,
                           wtname = NULL,
                           strataname = NULL, 
                           checkbox_valname = NULL,
                           checkbox_yesval = 1) {
  
  groupbyvars = c(
    summarize_by,
    summarize_var,
    checkbox_valname
  )
  
  groupbyvars = groupbyvars[groupbyvars %in% names(prepped_dt)]
  
  unwtd_summary = copy(prepped_dt)
  unwtd_summary = unwtd_summary[, .(count = .N), groupbyvars]
    
  if (is.null(summarize_by)){
    
    unwtd_summary[, unprop := count/ sum(count)]
    
  } else {
    
    unwtd_summary[, prop := count/ sum(count), summarize_by]
    
  }
  
  setcolorder(unwtd_summary, c(groupbyvars, 'count', 'prop'))

  if (weighted) {
    
    so = hts_to_so(
      prepped_dt = prepped_dt,
      weighted = weighted,
      wtname = wtname,
      strataname = strataname)
    
    if (se) {
      
      wtd_summary =
        so %>%
        group_by_at(unlist(groupbyvars)) %>%
        summarize(
          count = length(get(summarize_var)),
          prop =  srvyr::survey_prop(proportion = FALSE,
                                     vartype = "se"),
          est = survey_total(vartype = "se")
        ) %>%
        setDT()
      
    } else if (!se) {
      
      wtd_summary =
        so %>%
        group_by_at(unlist(groupbyvars)) %>%
        summarize(
          count = length(get(summarize_var)),
          prop =  srvyr::survey_prop(proportion = FALSE),
          est = survey_total()
        ) %>%
        setDT()
    }
    
  }
  
  # Only get "selected" rows:
    if(checkbox_value_colname %in% groupbyvars){
      
      is_checkbox = TRUE
      
      setnames(cat_summary_w, old = checkbox_valname, new = 'checkbox_valname')

      cat_summary_w = cat_summary_w[checkbox_valname == checkbox_yesval]
      cat_summary_w[, checkbox_valname := NULL]

      # recalculate prop without value == 0
      if (is.null(summarize_by)){

        cat_summary_w[, prop := count/ sum(count)]

      } else {

        cat_summary_w[, prop := count/ sum(count), summarize_by]

      }

    } else{

      is_checkbox = FALSE

    }

    cat_summary_w = factorize_df(
      df = cat_summary_w,
      vals_df = values_dt,
      variable_colname = "variable",
      value_colname = "value",
      value_label_colname = "label",
      value_order_colname = "val_order",
      verbose = FALSE,
      extra_labels = extra_labels
    )

    # Skip reordering if var is a checkbox
    if(!is_checkbox){

      if (is.null(summarize_by)){

        cat_summary_w = cat_summary_w[order(get(groupbyvars[1]))]

      } else {

        cat_summary_w = cat_summary_w[order(
          get(groupbyvars[1]),
          get(groupbyvars[2])
          )
        ]

      }
    }

    if(is_checkbox & !is.null(summarize_by)){

      cat_summary_w = cat_summary_w[order(get(groupbyvars[1]),
                                          get(groupbyvars[2]))]

    }

    cat_summary_ls[[wt_type]] = cat_summary_w[]

    if (wt_type == 'wtd'){

      cat_summary_ls$weight_name = weight_name

    }

  }

  return(cat_summary_ls)

}
