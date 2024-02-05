#' Find table location of a variable
#'
#' @param var searchable variable in string format
#' @param data List of household, person, vehicle, day, and trip tables in
#'  data.table format.
#' @param variables_dt codebook variable list in data table format
#'
#' @importFrom data.table melt
#'
#' @return variable location in string format
#' @export
#'
#' @examples
#'
#' hts_find_var('income_detailed', data = test_data)
#'
hts_find_var = function(var, 
                        data, 
                        variables_dt = variable_list) {

  varlist = data.table::copy(variables_dt)
  varlist = varlist[, .SD[1], keyby = .(shared_name)]
  data.table::setDT(varlist)

  table_names = names(data)
  
  # Find the locations of the variable:
  var_location = data.table::melt(varlist[shared_name == var,
                              c('variable',
                                table_names), with = FALSE],
                      id.vars = c("variable"),
                      variable.name = "table")

  if (nrow(var_location) == 0) {
    msg = paste0("Variable ", var, " not found")
    similar_vars =
      variable_list[agrep(pattern = var,
                          x = variable,
                          max.distance =  .25),
                    variable]
    if (length(similar_vars) > 0) {
      msg = paste0(msg,
                   "\n",
                   "Perhaps you meant ",
                   paste0(similar_vars, collapse = ", "))
    }
    stop(msg)
  }

  table_locs = var_location[value == 1, table]
  
  max_index = which.max(
    lapply(data[table_locs], nrow)
  )
  
  var_location = as.character(table_locs[[max_index]])

  return(var_location)
}

## quiets concerns of R CMD check
utils::globalVariables(c(".", "variable_list", "variable", "shared_name",
                         "hh", "person","day", "trip", "vehicle"))

