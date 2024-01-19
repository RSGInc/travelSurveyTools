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
#' @param checkbox_valname Name of the column with the checkbox value. Default is NULL.
#'  Must be provided if summarize_var is a checkbox variable.
#' @param checkbox_yesval Value of checkbox_valname that indicates it was selected.
#'  Default is NULL. Must be provided if summarize_var is a checkbox variable.
#' 
#' 
#' @importFrom srvyr survey_prop
#' @importFrom srvyr survey_total
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by_at
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
#'                 wtname = 'person_weight')
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
#'                 wtname = 'person_weight')
#'                 
#' DT = hts_prep_data(summarize_var = 'employment',
#'                 summarize_by = c('race', 'income_detailed', 'gender'),
#'                 variables_dt = variable_list,
#'                 data = list('hh' = hh,
#'                             'person' = person,
#'                             'day' = day,
#'                             'trip' = trip,
#'                             'vehicle' = vehicle))$cat
#'
#' hts_summary_cat(prepped_dt = DT,
#'                 summarize_var = 'employment',
#'                 summarize_by = c('race', 'income_detailed', 'gender'),
#'                 wtname = 'person_weight')$unwtd

hts_summary_cat = function(prepped_dt,
                           summarize_var = NULL,
                           summarize_by = NULL,
                           weighted = TRUE,
                           se = FALSE,
                           wtname = NULL,
                           strataname = NULL, 
                           checkbox_valname = NULL,
                           checkbox_yesval = NULL) {
  
  if ( !weighted & se ){
    
    message("Standard errors require weighted data; setting se = FALSE. 
            Set weighted = TRUE and specify a wtname if standard errors are desired.")
    
    se = FALSE
    
    
  }
  
  if ( !se & weighted & !is.null(strataname)){
    
    message("Stratanames are only used in calculating standard errors; setting se = TRUE 
            Set se = FALSE and remove the strataname if standard errors are not desired.")
    
    se = TRUE
    
    
  }
  
  groupbyvars = c(
    summarize_by,
    summarize_var,
    checkbox_valname
  )
  
  groupbyvars = groupbyvars[groupbyvars %in% names(prepped_dt)]
  
  unwtd_summary = copy(prepped_dt)
  unwtd_summary = unwtd_summary[, .(count = .N), keyby = groupbyvars]
  
  if (is.null(summarize_by)){
    
    unwtd_summary[, prop := count/ sum(count)]
    
  } else {
    
    unwtd_summary[, prop := count/ sum(count), keyby = summarize_by]
    
  }
  
  setcolorder(unwtd_summary, c(groupbyvars, 'count', 'prop'))
  
  cat_summary_ls = list()
  
  if (weighted) {
    
    if (se) {
      
      so = hts_to_so(
        prepped_dt = prepped_dt,
        weighted = weighted,
        wtname = wtname,
        strataname = strataname)
      
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
        prepped_dt[,.(
          count = .N,
          est = 
            sum(
              get(wtname)
            )
        ),
        groupbyvars
        ]
      
      if (is.null(summarize_by)){
        
        wtd_summary[, prop := est/ sum(est)]
        
      } else {
        
        wtd_summary[, prop := est/ sum(est), summarize_by]
        
      }
      
      setcolorder(wtd_summary, c(groupbyvars, 'count', 'prop', 'est'))
      
    }
    
  }
  
  # Only get "selected" rows:
  if(!is.null(checkbox_valname)){
    if(checkbox_valname %in% groupbyvars){
      
      is_checkbox = TRUE
      
      if (weighted){
        
        setnames(wtd_summary, old = checkbox_valname, new = 'checkbox_valname')

        wtd_summary = wtd_summary[checkbox_valname == checkbox_yesval]
        wtd_summary[, checkbox_valname := NULL]
  
        # recalculate prop without value == 0
        if (is.null(summarize_by)){
  
          wtd_summary[, prop := est/ sum(est)]
  
        } else {
  
          wtd_summary[, prop := est/ sum(est), summarize_by]
  
        }
        
      }
      
      setnames(unwtd_summary, old = checkbox_valname, new = 'checkbox_valname')
      
      unwtd_summary = unwtd_summary[checkbox_valname == checkbox_yesval]
      unwtd_summary[, checkbox_valname := NULL]
      
      # recalculate prop without value == 0
      if (is.null(summarize_by)){
        
        unwtd_summary[, prop := count/ sum(count)]
        
      } else {
        
        unwtd_summary[, prop := count/ sum(count), summarize_by]
        
      }
      
    } else {
      
      stop('Only provide checkbox_valname and checkbox_yesval if summarize_var is
           a checkbox variable.')
      
    }
    
  } else{
    
    is_checkbox = FALSE
    
  }


  # Skip reordering if var is a checkbox
  if(!is_checkbox){

    if (is.null(summarize_by)){

      unwtd_summary = unwtd_summary[order(get(groupbyvars[1]))]

    } else {

      unwtd_summary = unwtd_summary[order(
        get(groupbyvars[1]),
        get(groupbyvars[2])
        )
      ]

    }
  }

  if(is_checkbox & !is.null(summarize_by)){

    unwtd_summary = unwtd_summary[order(get(groupbyvars[1]),
                                        get(groupbyvars[2]))]

  }

  cat_summary_ls[['unwtd']] = unwtd_summary[]

  if (weighted){
    
    # Skip reordering if var is a checkbox
    if(!is_checkbox){
      
      if (is.null(summarize_by)){
        
        wtd_summary = wtd_summary[order(get(groupbyvars[1]))]
        
      } else {
        
        wtd_summary = wtd_summary[order(
          get(groupbyvars[1]),
          get(groupbyvars[2])
        )
        ]
        
      }
    }
    
    if(is_checkbox & !is.null(summarize_by)){
      
      wtd_summary = wtd_summary[order(get(groupbyvars[1]),
                                      get(groupbyvars[2]))]
      
    }
    
    cat_summary_ls[['wtd']] = wtd_summary[]

    cat_summary_ls$weight_name = wtname
    
  }
  
  return(cat_summary_ls)
  
}

## quiets concerns of R CMD check
utils::globalVariables(c("value_labels", "prop", "est", "count"))

