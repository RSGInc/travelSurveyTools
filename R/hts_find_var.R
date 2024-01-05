#' Find table location of a variable
#'
#' @param var searchable variable in string format
#' @param variables_dt codebook variable list in data table format
#'
#' @return variable location in string format
#' @export
#'
#' @examples
#' 
#' hts_find_var('income_detailed')
#'
hts_find_var = function(var, variables_dt = variable_list) {

  varlist = data.table::copy(variables_dt)
  varlist = varlist[, .SD[1], keyby = .(shared_name)]
  data.table::setDT(varlist)

  # Find the locations of the variable:
  var_location = data.table::melt(varlist[shared_name == var,
                              .(variable,
                                hh,
                                person,
                                vehicle,
                                trip,
                                day)],
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

  if (nrow(var_location) > 0) {


  # choose finest level of dis-aggregation for this variable:
  var_location =
    ifelse(var_location[table == "trip", "value"] == 1,
           "trip",
           ifelse(
             var_location[table == "day", "value"] == 1,
             "day",
             ifelse(
               var_location[table == "person", "value"] == 1,
               "person",
               ifelse(var_location[table == "vehicle", "value"] == 1,
                      "vehicle",
                      "hh")
             )
           ))

  var_location = var_location[1,][["value"]]

  }

  return(var_location)
}
