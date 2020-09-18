#' Retrieve aggregate number of Brazilian "news deserts"
#'
#' @description Retrieve the aggregate number of Brazilian news deserts
#'   (municipalities with no news organizations recorded by News Atlas'
#'   research) by state or region.
#'
#' @usage news_deserts_state(regions = F)
#'
#' @param regions logical. Define if the aggregate data should correspond to
#'   states or regions. Default is set to return states' data - \code{regions =
#'   F}.
#'
#' @return A \code{data.frame} with the following variables:
#'
#'   \itemize{ \item uf: abbreviation of the state's name (\code{if regions =
#'   F}). \item regiao: region's name (\code{if regions = T}). \item
#'   qtd_cidades: number of municipalities in the state/region. \item
#'   qtd_desertos: number of news deserts municipalities in the state/region.}
#'
#' @details \code{news_deserts_state} returns a dataset containing information on
#'   the aggregate number of news deserts municipalities in Brazilian states or
#'   regions.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' # Extract data on the aggregate number of "news deserts" municipalities in Brazilian states.
#'
#' deserts_aggregate_region_state <- news_deserts_state(regions = FALSE)
#'
#' # Extract data on the aggregate number of "news deserts" municipalities in Brazilian regions.
#'
#' deserts_aggregate_region <- news_deserts_state(regions = TRUE)

news_deserts_state <- function(regions = F){

  message("\nExtracting data from News Atlas' API.\n \n...\n")

  banco_estados <- extract_data_atlasbr_api("https://api.atlas.jor.br/api/v1/data/qtd-cities-without-media-state")

  banco_estados <- banco_estados %>%
    dplyr::rename(uf = .data$estado)

  if(regions == T){

    banco_regioes <- extract_data_atlasbr_api("https://api.atlas.jor.br/api/v1/data/qtd-cities-without-media-state-region")

    banco <- quiet(dplyr::left_join(banco_estados, banco_regioes))

    banco <- banco %>%
      dplyr::group_by(.data$regiao) %>%
      dplyr::summarise(qtd_cidades  = sum(.data$qtd_cidades),
                       qtd_desertos = sum(.data$qtd_desertos)) %>%
      dplyr::ungroup() %>%
      as.data.frame()

    message("\nDone.\n")

    return(banco)

  } else

  return(banco_estados)

  message("\nDone.\n")

}
