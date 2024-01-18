#' Filter datasets to only keep specified ids
#'
#' @param hts_data List of containing household travel data tables.
#' @param ids List of ids to keep in all of the tables
#' @param id_name Name of id being used for filtering (e.g., hh_id, person_id)
#'
#' @return Inputted list of tables filtered to the specified ids.
#' @export
#'
#' @examples
#'
#' require(data.table)
#' 
#' data(test_data)
#' 
#' hts_filter_data(hts_data = test_data,
#'                 ids = hh[num_people > 5, hh_id],
#'                 id_name = 'hh_id')
#'

hts_filter_data = function(hts_data,
                           ids,
                           id_name){
  
  tbl_names = names(hts_data)
  
  filtered_tbls = list()
  
  for (i in 1:length(hts_data)){
    
    tbl = hts_data[[i]]
    
    if (id_name %in% names(tbl)){
      
      filtered_tbl = tbl[get(id_name) %in% ids]
      
    } else {
      
      filtered_tbl = tbl
      
    }
    
    filtered_tbls[[i]] = filtered_tbl
    
  }
  
  names(filtered_tbls) = tbl_names


  return(filtered_tbls)
}
