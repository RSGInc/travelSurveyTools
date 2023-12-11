
#' Connect to the POPS server using Rpostgres
#'
#' This creates a connection between R and the pops server for querying. This
#' assumes that you have a pops.password and pops.username available in your
#' .Rprofile file.
#'
#'
#' @param dbname The database to query against.
#' @param env The environment in which the pops credentials are stored (ONLY FOR AZURE PIPELINES)
#' @author Jeffrey Dumont
#' @examples
#'
#' \dontrun{
#' con = connect_to_pops(dbname)}
#' @export
connect_to_pops = function(
  dbname,
  env = parent.frame() # needed for azure pipelines
){

  drv = RPostgres::Postgres()

  if (dbname == 'calgary') {

    host_name = '40.86.250.54'

  } else {

    host_name = 'pops.rsginc.com'

  }

  con = DBI::dbConnect(
    drv,
    host = host_name,
    user = get_pops_creds(what = 'user_name', env),
    password = get_pops_creds(what = 'password', env),
    dbname = dbname,
    port = "5432",
    sslcert = "",
    sslkey = "",
    sslmode = "require",
    bigint = c("numeric")
  )

  return(con)
}
