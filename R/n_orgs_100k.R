#' Retrieve data on the number of news organizations per 100k/inhabitants
#'
#' @description Retrieve data on the number of news organizations per 100,000
#'   inhabitants in Brazilian municipalities.
#'
#' @usage n_orgs_100k()
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
#' @details \code{n_orgs_100k} returns a dataset containing information on
#'   municipalities with at least one news outlet recorded by Atlas da Noticia's
#'   research team up to November, 2019. It includes a variable on the number of
#'   organizations per 100,000 inhabitants. The function returns only those
#'   municipalities.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' # Extract data on all the municipalities with at least one news outlet.
#'
#' municipalities_with_media <- n_orgs_100k()


n_orgs_100k <- function(){

  message("\nExtracting data from News Atlas' API.\n \n...")

  banco <- extract_data_atlasbr_api("https://api.atlas.jor.br/api/v1/data/medias-100k-hab")

  banco <- banco %>%
    dplyr::rename(uf = .data$estado,
                  municipio = .data$cidade,
                  codmun = .data$codmun_ibge) %>%
    as.data.frame()

  message("\nDone.\n")

  return(banco)

}
