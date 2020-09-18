#' Retrieve aggregate number of news organizations per 100k/inhabitants
#'
#' @description Retrieve the aggregate number of news organizations per 100,000
#'   inhabitants in Brazilian municipalities by state or region.
#'
#' @usage n_orgs_100k_state(regions = T)
#'
#' @param regions logical. Define if variables on the regional level should be
#'   included in the dataset. Default is set to include a variable with data
#'   aggregated to the regional level - \code{regions = T}. See details for more
#'   information.
#'
#' @return A \code{data.frame} with the following variables:
#'
#'   \itemize{ \item estado: abbreviation of the state's name. \item regiao:
#'   region's name. \item med_idhm: mean of the Human Development Index at the
#'   municipalities of the corresponding state (Census 2010). \item med_idhm_e: mean of the
#'   Human Development Index - education at the municipalities of the
#'   corresponding state (Census 2010). \item med_idhm_r: mean of the Human Development Index -
#'   \emph{per capita} income at the municipalities of the corresponding state (Census 2010).
#'   \item populacao: states' population. \item qtd_veiculos: number of news
#'   organizations in the state. \item veiculos_por_100k_hab: number of news
#'   organizations per 100,000 inhabitants (state-level). \item ano: year from
#'   IBGE's (Brazilian Institute of Geography and Statistics) records - note
#'   that data on news organizations collected by Atlas' team were last updated
#'   on Nov. 30th, 2019. \item qtd_veiculos_regiao: number of news organizations
#'   in the region (\code{if regions = T}). \item populacao_regiao: regions'
#'   population (\code{if regions = T}). \item veiculos_por_100k_hab_regiao:
#'   number of news organizations per 100,000 inhabitants (regional-level)
#'   (\code{if regions = T}). }
#'
#' @details \code{n_orgs_100k_state} returns a dataset containing information on
#'   the aggregate number of news deserts municipalities in Brazilian states or
#'   regions.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#'
#' # Extract data on the aggregate number of "news deserts" municipalities in Brazilian states.
#'
#' deserts_aggregate_region_state <- n_orgs_100k_state(regions = FALSE)
#'
#' # Extract data on the aggregate number of "news deserts", including Brazilian regions' variables.
#'
#' deserts_aggregate_region <- n_orgs_100k_state(regions = TRUE)

n_orgs_100k_state <- function(regions = T){

  message("\nExtracting data from News Atlas' API.\n \n...\n")

  banco <- extract_data_atlasbr_api("https://api.atlas.jor.br/api/v1/data/med-idhm-2010-state-region")

  if(regions == T){

  banco <- banco %>%
    dplyr::group_by(.data$regiao) %>%
    dplyr::mutate(qtd_veiculos_regiao = sum(as.numeric(.data$qtd_veiculos)),
                  populacao_regiao    = sum(as.numeric(.data$populacao)),
                  veiculos_por_100k_hab_regiao = round((100000*.data$qtd_veiculos_regiao)/.data$populacao_regiao, digits = 3)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(uf = .data$estado) %>%
    as.data.frame()

  }

  message("\nDone.\n")

  return(banco)

}
