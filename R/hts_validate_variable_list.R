#' Check that variable list has required elements for hts_prep_data returns updated variable list or prompts error
#'
#' @param variable_list Name of the variable to summarize. Default is NULL
#' @param hts_data List of named tables specified in hts_prep_data
#' 
#' @return Cleaned variable_list
#' @export
#'
#' @examples
#' hts_validate_variable_list(variable_list, test_data)
#'

hts_validate_variable_list = function(variable_list, 
                                      hts_data){
  
  
  var_dt = copy(variable_list)
  
  # Get expected variable names
  tbl_names = names(hts_data)
  
  admin_names = c('variable',
                  'is_checkbox',
                  'data_type',
                  'description',
                  'shared_name')
  
  expected_names = c(admin_names, tbl_names)
  
  # Stop if missing required columns
  if (length(setdiff(expected_names, names(var_dt))) > 0){
    
    stop(
      'Columns are missing: ',
      paste(setdiff(expected_names, names(var_dt)), collapse = ', '),
      "See https://rsginc.github.io/travelSurveyTools/reference/variable_list.html for variable requirements."
    )

  }
  
  # Check for missing values in admin names
  missing_admin_counts = lapply(var_dt[, ..admin_names], function(x) sum(is.na(x)))
  
  if (length(missing_admin_counts[missing_admin_counts != 0]) > 0){
    
    stop(
      'Missing values in: ',
      paste(names(missing_admin_counts)[missing_admin_counts != 0], collapse = ', '),
      "See https://rsginc.github.io/travelSurveyTools/reference/variable_list.html for variable requirements."
    )
    
  }
  
  # Check that each variable is in at least one table
  var_dt$tbl_count = rowSums(var_dt[, ..tbl_names])
  
  if (var_dt[tbl_count == 0, .N]){
    
    warning(
      'Variable(s) do not have a location specified: ',
      paste0(var_dt[tbl_count == 0, variable], collapse = ', ' )
    )
    
  }
  
  # Check for duplicate variables
  if (var_dt[, .N, variable][N > 1, .N] != 0){
    
    stop('Duplicate variables appear in variable list')
    
  }
  
  # Check that checkbox variables are "integer/categorical"
  
  not_categorical = var_dt[is_checkbox == 1 & data_type != 'integer/categorical', .N]
  
  if(not_categorical > 0){
    
    message('Editing ', not_categorical, ' categorical variables to "integer/categorical"')
    
  }
  
  var_dt = var_dt[, ..expected_names]
   
  return(var_dt)
  
}

## quiets concerns of R CMD check
utils::globalVariables(c("..admin_names", "..tbl_names", "tbl_count", "N", 
                         "..expected_names"))
