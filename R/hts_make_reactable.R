
hts_make_reactable = function(dt,
                              summarize_by = NULL,
                              rows = 20){
  
  
  data = copy(dt)
  
  #add commas to vars
  
  for (var in c('count', 'est', 'est_se')){
    
    if (var %in% names (data)){
      
      data[, stringr::str_glue('{var}') := format(get(var), big.mark = ',')]
      
    }
    
  }
  
  for (var in c('prop', 'prop_se')){
    
    if (var %in% names (data)){
      
      data[, stringr::str_glue('{var}') := paste0(round(get(var) * 100, 1), '%')]
      
    }
    
  }
  
  
  #Write out full names
  
  oldnames = c('count',
               'prop',
               'est',
               'prop_se',
               'est_se')
  
  new_names = c('Count',
                'Percent',
                'Estimate',
                'Percent Standard Error',
                'Estimate Standard Error')
  
  data.table::setnames(data, oldnames, new_names, skip_absent = TRUE)
  
  reactable::reactable(
    data,
    groupBy = stringr::str_glue('{summarize_by}'),
    sortable = FALSE,
    striped = TRUE,
    searchable = TRUE,
    highlight = TRUE,
    outlined = TRUE,
    defaultPageSize = rows
  )
  
  
}
