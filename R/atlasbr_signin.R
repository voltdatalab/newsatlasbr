#' Retrieve an News Atlas' API token
#'
#' @description Sets News Atlas API's user and password for the current R
#'   session, in order to require a token to access it. See the details section
#'   for more information.
#'
#' @usage atlas_signin(email, password)
#'
#' @param email e-mail which was registered to access News Atlas' API.
#' @param password password which was registered to access News Atlas' API.
#'
#' @return Sets "email_atlasdanoticia" and "password_atlasdanoticia" as
#'   environment variables.
#'
#' @details News Atlas' API (\url{https://api.atlas.jor.br/docs}) allows easier
#'   access to News Atlas' data about news organizations in Brazilian cities.
#'   The API requires a token to be used. This token expires within an hour,
#'   after which it needs to be refreshed. Only registered users can require a
#'   token in the API. To register, users should access:
#'   \url{https://api.atlas.jor.br/register}.
#'
#'   Once users are registered and have authorized access, they need to request
#'   a token to extract data using the API. This can be done using
#'   \code{atlas_signin} function. News Atlas uses a JWT authentication standard
#'   to grant users access to the API.
#'
#'   \code{atlas_signin} function sets the API user (e-mail) and password only
#'   for the current R session. However, since the user and password are
#'   personal and private, we recommend a careful use of this function in R
#'   script files, so that users do not share these information.
#'
#' @export
#'
#' @author The function design was inspired on the
#'   \code{\link[ggmap]{register_google}} function.
#'
#' @examples
#'
#' # This sets your email and password for the current session and retrieves a
#' # Bearer token that lasts for 1 hour. If the current token is expired, this
#' # function uses the information previously set to retrieve a new one.
#'
#' \dontrun{
#' atlas_signin(email = "example@@email.com", password = "yourpassword")
#' }
#'

atlas_signin <- function(email, password){

  Sys.setenv("email_atlasdanoticia" = email,
             "password_atlasdanoticia" = password)

  get_token_atlasbr()

}

