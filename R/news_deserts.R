#' Retrieve data on Brazilian "news deserts"
#'
#' @description Retrieve data on Brazilian "news deserts": municipalities with
#'   no news organizations recorded by News Atlas' research.
#'
#' @usage news_deserts()
#'
#' @return A \code{data.frame} with the following variables:
#'
#'   \itemize{ \item codmun: IBGE's (Brazilian Institute of Geography and
#'   Statistics) code for the municipality (7-digit). \item municipio:
#'   municipality name. \item uf: abbreviation of the state's name. \item
#'   regiao: name of the country's region where the municipality is located at.
#'   \item populacao: municipality's population. \item ano: year from IBGE's population
#'   records (note that data on news organizations collected by Atlas' team were
#'   last updated on Nov. 30th, 2019.) \item IDHM: Human Development Index for
#'   the municipality (Census 2010). \item IDHM_R: Human Development Index - \emph{per capita}
#'   income for the municipality (Census 2010). \item IDHM_E: Human Development Index -
#'   education for the municipality (Census 2010).}
#'
#' @details \code{news_deserts} returns a dataset containing information on
#'   municipalities with no news organizations recorded by Atlas da Noticia's
#'   research team up to November, 2019 - which were designated as "news
#'   deserts". The function returns only those municipalities.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' # Extract data on all "news deserts" municipalities
#'
#' deserts_dataset <- news_deserts()
#'

news_deserts <- function(){

  message("\nExtracting data from News Atlas' API.\n \n...")

  banco <- extract_data_atlasbr_api("https://api.atlas.jor.br/api/v1/data/cities-without-media")

  message("\nDone.\n")

  return(banco)

  }

