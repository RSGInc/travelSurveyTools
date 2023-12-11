#' Get pops credentials
#'
#' Get pops credentials
#' @param what character.  Should the function retrieve the username or the password.
#' @param env The environment in which the pops credentials are stored passed from connect_to_pops (ONLY FOR AZURE PIPELINES)
#' @export get_pops_creds
#' @author Matt Landis
#'
get_pops_creds = function(what = c('user_name', 'password'), env = parent.frame()) {

  if ( !what %in% c('user_name', 'password')){
    stop('please specify what = "user_name" or "password"')
  }

  # On Azure Pipelines, pops creds are stored as variables in the environment
  # Keep this here so it will continue to function

  if (paste0('pops.', what) %in% names(env)) {

    value = get(paste0('pops.', what), envir = env)
    message('Using ', what, ' from environment')
    return(value)

  }

  #if ( !requireNamespace('keyring', quietly = TRUE) ) {
  #  return(value)
  #}

  try({
    value = keyring::key_get('pops', what)
  }, silent = TRUE)

  if ( !exists('value') || is.null(value) || value == '') {

    message(what, ' not found in keyring')

    value = readline(prompt = paste0('Please enter your POPS ', what, ': '))

    keyring::key_set_with_value(
      service = 'pops',
      username = what,
      password = value)

    value = keyring::key_get(
      'pops',
      what)

    message('Saved ', what, ' in default keyring')

  }

  return(value)
}
