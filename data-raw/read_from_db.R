#' Read data from a database as a data.table
#'
#' Queries a database using the query & connection provided and saves results
#' as a data.table.
#'
#'
#' @param con A connection object (can use \code{\link{connect_to_pops}} to get
#' a POPS connection).
#' @param query Query syntax.
#' @param disconnect Logical. flag for closing db connection
#' @param verbose Logical. Should warnings be emitted?
#' @author Leah Flake
#' @examples
#'
#'   \dontrun{
#'     con = connect_to_pops("metcouncil_wave1")
#'     query = "select household_id, participant, idx from ms_person where study_id = 52"
#'     data = read_from_db(con,query)
#'     disconnect = TRUE
#'   }
#' @import data.table
#' @export read_from_db
read_from_db = function(
  con,
  query,
  disconnect = NULL,
  verbose = TRUE
){

  result =
    tryCatch({
        DBI::dbGetQuery(con,query)
      },
      error = function(cond) {
        warning(cond)
        # Choose a return value in case of error
        return(NA)
      }
    )

  dt =  tryCatch({
      setDT(result)
    },
    error = function(cond) {
      return(NA)
    }
  )

  if ( verbose ){

    if (any(lapply(dt, class) %like% 'POSIXct') & !Sys.getenv('TZ') %in% c('UTC','GMT')) {
      warning('Contains timestamps that might be converted to local time - double check that times came through as expected.')
    }

    if (is.null(disconnect)) {
      warning('You\'re still connected to the database. Please remember to disconnect before you leave!')
    } else if (disconnect){
      DBI::dbDisconnect(con)
    }
  }
  return(dt)

}
