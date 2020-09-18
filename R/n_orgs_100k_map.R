#' Plot Brazilian maps on the number of news organizations per 100k/inhabitants
#'
#' @description Plot a map with information on the number of news organizations
#'   per 100k/inhabitants.
#'
#' @usage n_orgs_100k_map(aggregation = "municipalities", export = F)
#'
#' @param aggregation aggregation (character). Can be one out of 3 levels:
#'   "municipalities", "states" or "regions" (which can also be written in
#'   Portuguese, with no accents: "municipios:, "estados" and "regioes").
#'   Default is set to \code{aggregation = "municipalities"}.
#' @param export logical. Should the map be exported as a .jpg file? Default is
#'   set to no \code{export = F}.
#'
#' @return A \code{ggplot} object with a Brazil map filled according to the
#'   number of news organizations per 100k/inhabitants.
#'
#' @details \code{n_orgs_100k_map} returns \code{ggplot} object with a Brazil
#'   map filled according to the number of news organizations per
#'   100k/inhabitants in the chosen aggregation level. Spatial data is extracted
#'   using the \href{https://github.com/ipeaGIT/geobr}{geobr package}.
#'
#' @seealso \code{\link{news_deserts_map}} and \code{\link{almost_deserts_map}}
#'   for other functions that generates maps, and \code{\link{n_orgs_100k}} and
#'   \code{\link{n_orgs_100k_state}} for functions to extract data on the number
#'   of organizations per 100k/inhabitants.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' # Plot a map with the number of news organizations per
#' # 100k/inhabitants at the regional level.
#'
#' n_orgs_100k_map(aggregation = "regions", export = FALSE)
#'
#' # Save a ggplot object into an object
#' # called states_organizations_number with the number of news
#' # organizations per 100k/inhabitants in each of state
#'
#' states_organizations_number <- n_orgs_100k_map(aggregation = "states", export = FALSE)
#'
#' # Plot and export a map showing the number news organizations per
#' # 100k/inhabitants at the municipality level
#'
#' n_orgs_100k_map(aggregation = "municipalities", export = TRUE)

n_orgs_100k_map <- function(aggregation = "municipalities",
                               export = F){

  if(!aggregation %in% c("municipalities", "municipios",
                         "states", "estados",
                         "regions", "regioes")) {

    stop("Please provide a valid input to the aggregation argument. The possible values are
    \"municipalities\", \"municipios\", \"states\", \"estados\", \"regions\", \"regioes\".")

  } else

  message("\nExtracting spatial data from geobr package.\n")

  if(aggregation == "municipalities" | aggregation == "municipios"){

    municipalities <- geobr::read_municipality()
    municipalities$code_muni <- as.character(municipalities$code_muni)

    cities_100k <- n_orgs_100k()

    cities_100k <- dplyr::left_join(municipalities, cities_100k,
                                    by = c("code_muni" = "codmun")) %>%
      dplyr::mutate(veiculos_por_100k_hab = ifelse(is.na(.data$veiculos_por_100k_hab), 0, .data$veiculos_por_100k_hab),
                    veiculos_por_100k_hab = as.numeric(.data$veiculos_por_100k_hab),
                    categorias_n_veiculos = dplyr::case_when(.data$veiculos_por_100k_hab == 0 ~ 0,
                                                             .data$veiculos_por_100k_hab > 0 & .data$veiculos_por_100k_hab <= 5 ~ 1,
                                                             .data$veiculos_por_100k_hab > 5 & .data$veiculos_por_100k_hab <= 9 ~ 2,
                                                             .data$veiculos_por_100k_hab > 9 & .data$veiculos_por_100k_hab <= 15 ~ 3,
                                                             .data$veiculos_por_100k_hab > 15 ~ 4),
                    categorias_n_veiculos = factor(.data$categorias_n_veiculos,
                                                   labels = c("0", "Up to 5,000", "Between 5,001 and 9",
                                                              "Between 9,001 and 15", "Over 15")))

    message("\nGenerating map.\n \n...\n")

    mapa <- ggplot2::ggplot(cities_100k) +
      ggplot2::geom_sf(size = 0.03, ggplot2::aes(fill  = .data$categorias_n_veiculos)) +
      ggplot2::coord_sf(datum = NA) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "bottom",
            legend.direction = "horizontal",
            legend.title = ggplot2::element_text(size = 10, hjust = 0.5),
            plot.title   = ggplot2::element_text(hjust = 0.5),
            plot.caption = ggplot2::element_text(hjust = 0.5)) +
      viridis::scale_fill_viridis("magma", discrete = T) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, title = "N. of media organizations\n(per 100k/inhab.)"))

    if(export == T){

      export_graph("cities_100k_map")

    }

  }

  if(aggregation == "states" | aggregation == "estados" | aggregation == "regions" | aggregation == "regioes"){

    states <- geobr::read_state() %>%
      dplyr::mutate(name_region = ifelse(.data$name_region == "Centro Oeste", "Centro-Oeste", .data$name_region))

    states_df   <- n_orgs_100k_state(regions = T)

    states_100k <- dplyr::left_join(states, states_df, by = c("abbrev_state" = "uf")) %>%
      dplyr::mutate(veiculos_por_100k_hab = ifelse(is.na(.data$veiculos_por_100k_hab), 0, .data$veiculos_por_100k_hab),
                    veiculos_por_100k_hab = as.numeric(.data$veiculos_por_100k_hab),
                    categorias_n_veiculos_estados = dplyr::case_when(.data$veiculos_por_100k_hab == 0 ~ 0,
                                                                     .data$veiculos_por_100k_hab > 0 & .data$veiculos_por_100k_hab <= 5 ~ 1,
                                                                     .data$veiculos_por_100k_hab > 5 & .data$veiculos_por_100k_hab <= 9 ~ 2,
                                                                     .data$veiculos_por_100k_hab > 9 & .data$veiculos_por_100k_hab <= 15 ~ 3,
                                                                     .data$veiculos_por_100k_hab > 15 ~ 4),
                    categorias_n_veiculos_estados = factor(.data$categorias_n_veiculos_estados, levels = c(0:4),
                                                           labels = c("0", "Up to 5,000", "Between 5,001 and 9",
                                                              "Between 9,001 and 15", "Over 15")),
                    categorias_n_veiculos_regioes = dplyr::case_when(.data$veiculos_por_100k_hab_regiao == 0 ~ 0,
                                                                     .data$veiculos_por_100k_hab_regiao > 0 & .data$veiculos_por_100k_hab_regiao <= 5 ~ 1,
                                                                     .data$veiculos_por_100k_hab_regiao > 5 & .data$veiculos_por_100k_hab_regiao <= 9 ~ 2,
                                                                     .data$veiculos_por_100k_hab_regiao > 9 & .data$veiculos_por_100k_hab_regiao <= 15 ~ 3,
                                                                     .data$veiculos_por_100k_hab_regiao > 15 ~ 4),
                    categorias_n_veiculos_regioes = factor(.data$categorias_n_veiculos_regioes, levels = c(0:4),
                                                           labels = c("0", "Up to 5,000", "Between 5,001 and 9",
                                                                      "Between 9,001 and 15", "Over 15")))

    message("\nGenerating map.\n \n...\n")

      if(aggregation == "states" | aggregation == "estados"){

        mapa <- ggplot2::ggplot(states_100k) +
          ggplot2::geom_sf(color = "black", size = 0.1, ggplot2::aes(fill = .data$categorias_n_veiculos_estados)) +
          ggplot2::coord_sf(datum = NA) +
          ggplot2::theme_void() +
          ggplot2::theme(legend.position = "bottom",
                legend.direction = "horizontal",
                legend.title = ggplot2::element_text(hjust = 0.5),
                plot.title   = ggplot2::element_text(hjust = 0.5),
                plot.caption = ggplot2::element_text(hjust = 0.5)) +
          viridis::scale_fill_viridis("magma", discrete = T) +
          ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, title = "N. of media organizations\n(per 100k/inhab.)"))

        if(export == T) {

          export_graph("states_100k_map")

          }


        }

    if(aggregation == "regions" | aggregation == "regioes"){  # Se nao precisarmos fazer quebras diferentes pros graficos, basta subir uma das chaves
      # e fazer so um bloco de codigo para gerar ambos os graficos

      mapa <- ggplot2::ggplot(states_100k) +
        ggplot2::geom_sf(color = "black", size = 0.1, ggplot2::aes(fill = .data$categorias_n_veiculos_regioes)) +
        ggplot2::coord_sf(datum = NA) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "bottom",
              legend.direction = "horizontal",
              legend.title = ggplot2::element_text(hjust = 0.5),
              plot.title   = ggplot2::element_text(hjust = 0.5),
              plot.caption = ggplot2::element_text(hjust = 0.5)) +
        viridis::scale_fill_viridis("magma", discrete = T) +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, title = "N. of media organizations\n(per 100k/inhab.)"))

      if(export == T) {

        export_graph("regions_100k_map")

      }

    }

  }

  return(mapa)

  message("\nDone.\n")

}

