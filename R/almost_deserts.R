#' Retrieve data on Brazilian "almost news deserts"
#'
#' @description Retrieve data on Brazilian 'almost news deserts": municipalities
#'   with 1 or 2 news organizations recorded by News Atlas' research.
#'
#' @usage almost_deserts()
#'
#' @return A \code{data.frame} with the following variables:
#'
#'   \itemize{ \item municipio: municipality name. \item uf: abbreviation of the
#'   state's name. \item regiao: name of the country's region where the
#'   municipality is located at. \item qtd_veiculos: number of news
#'   organizations in the municipality. \item codmun: IBGE's (Brazilian
#'   Institute of Geography and Statistics) code for the municipality (7-digit).
#'   \item populacao: municipality's population. \item ano: year from IBGE's
#'   population records (note that data on news organizations collected by
#'   Atlas' team were last updated on Nov. 30th, 2019.) \item
#'   veiculos_por_100k_hab: number of news organizations per 100,000
#'   inhabitants. \item IDHM: Human Development Index for the municipality
#'   (Census 2010). \item IDHM_R: Human Development Index - \emph{per capita}
#'   income for the municipality (Census 2010). \item IDHM_E: Human Development
#'   Index - education for the municipality (Census 2010).}
#'
#' @details \code{almost_deserts} returns a dataset containing information on
#'   municipalities with 1 or 2 news organizations recorded by Atlas da
#'   Noticia's research team up to November, 2019 - which were designated as
#'   "almost deserts". The function returns only those municipalities.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' # Extract data on all "almost deserts" municipalities
#'
#' almost_deserts_dataset <- almost_deserts()

almost_deserts <- function(){

  message("\nExtracting data from News Atlas' API.\n \n...")

  banco <- extract_data_atlasbr_api("https://api.atlas.jor.br/api/v1/data/almost-deserts")

  banco <- banco %>%
    dplyr::rename(municipio = .data$cidade) %>%
    as.data.frame()

  message("\nDone.\n")

  return(banco)

}
