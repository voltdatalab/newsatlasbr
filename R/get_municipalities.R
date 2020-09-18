#' Retrieve data on the n. of news organizations by municipalities
#'
#' @description Retrieve data on the number of news organizations for each of
#'   the 5,565 Brazilian municipalities (2010 Census).
#'
#' @usage get_municipalities()
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
#' @details \code{get_municipalities} returns a dataset containing information
#'   on the number of news organizations for each of the 5,565 Brazilian
#'   municipalities. It comprises data from "news deserts" and cities with at
#'   least 1 organization to create one dataset.
#'
#' @seealso \code{\link{news_deserts}} for a function that extracts data on
#'   municipalities where there's no news organization,
#'   \code{\link{almost_deserts}} for cities with 1 or 2, and
#'   \code{\link{n_orgs_100k}} for all that have at least 1 opened organization
#'   there.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' brazil <- get_municipalities()

get_municipalities <- function(){

  message("\n(1) Extract data on news deserts.")
  banco_desertos <- news_deserts()

  message("(2) Extract data on municipalities with at least 1 news organization.")
  banco_100k <- n_orgs_100k()

  message("(3) Join datasets.")
  banco <- suppressMessages(dplyr::full_join(banco_100k, banco_desertos)) %>%
    dplyr::mutate(qtd_veiculos = ifelse(is.na(.data$qtd_veiculos), 0, .data$qtd_veiculos),
                  veiculos_por_100k_hab = ifelse(is.na(.data$veiculos_por_100k_hab), 0, .data$veiculos_por_100k_hab)) %>%
    as.data.frame()

  message("\nDone.\n")

  return(banco)

}
