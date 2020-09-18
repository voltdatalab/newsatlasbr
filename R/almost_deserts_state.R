#' Retrieve aggregate number of Brazilian "almost news deserts"
#'
#' @description Retrieve the aggregate number of Brazilian "almost news deserts"
#'   (municipalities with 1 or 2 news organizations recorded by News Atlas'
#'   research) by state or region.
#'
#' @usage almost_deserts_state(regions = F)
#'
#' @param regions logical. Define if the aggregate data should correspond to
#'   states or regions. Default is set to return states' data - \code{regions =
#'   F}.
#'
#' @return A \code{data.frame} with the following variables:
#'
#'   \itemize{ \item uf: abbreviation of the state's name (\code{if regions =
#'   F}). \item regiao: region's name. \item qtd_quase_desertos: number of
#'   almost news deserts municipalities in the state/region.}
#'
#' @details \code{almost_deserts_state} returns a dataset containing information
#'   on the aggregate number of news deserts municipalities in Brazilian states
#'   or regions.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' # Extract data on the aggregate number of "almost news deserts" municipalities in Brazilian states.
#'
#' deserts_aggregate_state <- almost_deserts_state(regions = FALSE)
#'
#' # Extract data on the aggregate number of "almost news deserts" municipalities in Brazilian regions.
#'
#' deserts_aggregate_region <- almost_deserts_state(regions = TRUE)

almost_deserts_state <- function(regions = F){

  message("\nExtracting data from News Atlas' API.\n \n...")

  banco <- extract_data_atlasbr_api("https://api.atlas.jor.br/api/v1/data/almost-deserts")

  if(regions == T){

    banco <- banco %>%
      dplyr::group_by(.data$regiao) %>%
      dplyr::summarise(qtd_quase_desertos = dplyr::n()) %>%
      dplyr::ungroup() %>%
      as.data.frame()

  } else {

  banco <- banco %>%
    dplyr::group_by(.data$estado) %>%
    dplyr::summarise(qtd_quase_desertos = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::rename(uf = .data$estado) %>%
    as.data.frame()

  }

  message("\nDone.\n")

  return(banco)

}


